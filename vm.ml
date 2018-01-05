module S = Syntax
module F = Flat

exception Error of string
let err s = raise (Error s)

type binOp = S.binOp

type id = int
type label = string

let fresh_label = Misc.fresh_id_maker "_"

type operand =
    Param of int     (* param(n) *)
  | Local of id      (* local(ofs) *)
  | Proc  of label   (* labimm(l) *)
  | IntV  of int     (* imm(n) *)

type instr =
    Move of id * operand (* local(ofs) <- op *)
  | BinOp of id * S.binOp * operand * operand
    (* local(ofs) <- bop(op_1, op_2) *)
  | Label of label (* l: *)
  | BranchIf of operand * label (* if op then goto l *)
  | Goto of label (* goto l *)
  | Call of id * operand * operand list
    (* local(ofs) <- call op_f(op_1, ..., op_n) *)
  | Return of operand (* return(op) *)
  | Malloc of id * operand list (* new ofs [op_1, ..., op_n] *)
  | Read of id * operand * int (* read ofs #i(op) *)
  | BEGIN of label (* データフロー解析で内部的に使用 *)
  | END of label   (* データフロー解析で内部的に使用 *)

type decl =
    ProcDecl of label * int * instr list  (* int は局所変数の個数 *)

type prog = decl list

(* ==== Formatter ==== *)

let string_of_binop = function
    S.Plus -> "add"
  | S.Mult -> "mul"
  | S.Lt   -> "lt"

let string_of_operand = function
    Param i -> "p" ^ string_of_int i
  | Local o -> (* -1 は生存変数解析で使われる特殊な値 *)
      if o = -1 then "*" else "t" ^ string_of_int o
  | Proc  l -> l
  | IntV  i -> string_of_int i

let string_of_instr idt tab = function
    Move (t, v) ->
      idt ^ "move" ^ tab ^ "t" ^ string_of_int t ^ ", " ^
      string_of_operand v
  | BinOp (t, op, v1, v2) ->
      idt ^ string_of_binop op ^ tab ^ "t" ^ string_of_int t ^ ", " ^
      string_of_operand v1 ^ ", " ^ string_of_operand v2
  | Label lbl -> lbl ^ ":"
  | BranchIf (v, lbl) ->
      idt ^ "bif" ^ tab ^ string_of_operand v ^ ", " ^ lbl
  | Goto lbl ->
      idt ^ "goto" ^ tab ^ lbl
  | Call (dst, tgt, [a0; a1]) ->
      idt ^ "call" ^ tab ^ "t" ^ string_of_int dst ^ ", " ^
      string_of_operand tgt ^
      "(" ^ string_of_operand a0 ^ ", " ^ string_of_operand a1 ^ ")"
  | Call (_, _, args) -> err ("Illegal number of arguments: " ^
                              string_of_int (List.length args))
  | Return v ->
      idt ^ "ret" ^ tab ^ string_of_operand v
  | Malloc (t, vs) ->
      idt ^ "new" ^ tab ^ "t" ^ string_of_int t ^ " [" ^
      (String.concat ", " (List.map string_of_operand vs)) ^ "]"
  | Read (t, v, i) ->
      idt ^ "read" ^ tab ^ "t" ^ string_of_int t ^ " #" ^
      string_of_int i ^ "(" ^ string_of_operand v ^ ")"
  | BEGIN lbl ->
      idt ^ "<BEGIN: " ^ lbl ^ ">"
  | END lbl ->
      idt ^ "<END: " ^ lbl ^ ">"

let string_of_decl (ProcDecl (lbl, n, instrs)) =
  "proc " ^ lbl ^ "(" ^ string_of_int n ^ ") =\n" ^
  (String.concat "\n"
     (List.map (fun i -> string_of_instr "  " "\t" i) instrs)) ^ "\n"

let string_of_vm prog =
  String.concat "\n" (List.map string_of_decl prog)


(* ==== 仮想機械コードへの変換 ==== *)

let trans_decl (F.RecDecl (proc_name, params, body)) =
  let local_count = ref 0 in
  let value_to_operand env = function
      F.Var x -> Environment.lookup x env
    | F.Fun f -> Proc f
    | F.IntV i -> IntV i
  in
  let rec trans_cexp env target = function
      F.ValExp value ->
        [Move (target, value_to_operand env value)]
    | F.BinOp (op, v1, v2) ->
        let lhs = value_to_operand env v1 in
        let rhs = value_to_operand env v2 in
        [BinOp (target, op, lhs, rhs)]
    | F.AppExp (v1, v2) ->
        let appfun = value_to_operand env v1 in
        let apparg = List.map (value_to_operand env) v2 in
        [Call (target, appfun, apparg)]
    | F.IfExp (cond, e1, e2) ->
        let ifcond = value_to_operand env cond in
        let iftrue = trans_exp env target e1 in
        let iffalse = trans_exp env target e2 in
        let iftrue_label = fresh_label "iftrue" in
        let endif_label = fresh_label "endif" in
        [BranchIf (ifcond, iftrue_label)] @ iffalse @ [Goto endif_label; Label iftrue_label] @ iftrue @ [Label endif_label]
    | F.TupleExp values ->
        [Malloc (target, List.map (value_to_operand env) values)]
    | F.ProjExp (value, idx) ->
        let projval = value_to_operand env value in
        [Read (target, projval, idx)]
  and trans_exp env target = function
      F.CompExp cexp -> trans_cexp env target cexp
    | F.LetExp (id, cexp, exp) ->
        let let_target = !local_count in
        let newenv = Environment.extend id (Local let_target) env in
        local_count := !local_count + 1;
        (trans_cexp env let_target cexp) @ (trans_exp newenv target exp)
    | F.LoopExp (id, cexp, exp) ->
        let loop_target = !local_count in
        local_count := !local_count + 1;
        let looplabel = fresh_label "loop" in
        let newenv = Environment.extend id (Local loop_target) @@ Environment.extend "__loop__" (Proc looplabel) @@ Environment.extend "__loopvar__" (Local loop_target) env in
        (trans_cexp env loop_target cexp) @ [Label looplabel] @ (trans_exp newenv target exp)
    | F.RecurExp value ->
        let Proc(loop_label) = Environment.lookup "__loop__" env in
        [Move (target, value_to_operand env value); Goto loop_label]
  in
  let rec enumerate idx = function
      [] -> []
    | h::t -> (idx, h)::(enumerate (idx + 1) t)
  in
  let env =
    let rec extend env = function
      [] -> env
    | (num, id)::t -> extend (Environment.extend id (Param num) env) t
    in extend Environment.empty @@ enumerate 0 params
  in
  let transed_body = trans_exp env 0 body in
  let nlocals = !local_count in
  ProcDecl (proc_name, nlocals, transed_body @ [Return (Local 0)])

(* entry point *)
let trans = List.map trans_decl
