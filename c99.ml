module S = Syntax
module F = Flat

exception Error of string

let err s = raise (Error s)

type id = F.id

type value =
  IntV of int
| Var of id
| Fun of id

type exp =
  Value of value
| BinOp of S.binOp * value * value
| Apply of value * value list
| If of value * stmt list * stmt list
| Malloc of value list
| Read of value * int

and stmt =
  Assign of id * exp
| VarDecl of id
| VarAssign of id * exp
| Expr of exp
| Loop of stmt list
| Recur of id * exp
| Return of exp

type decl = Decl of id * id list * stmt list

type code = decl list

let string_of_value = function
  IntV i -> "(_t){" ^ (string_of_int i) ^ "}"
| Var x -> x
| Fun f -> f

let string_of_intvar = function
  IntV i -> string_of_int i
| Var x -> x ^ ".value"
| Fun f -> err "function should not be a integer value"

let rec enumerate idx = function
    [] -> []
  | h::t -> (idx, h)::(enumerate (idx + 1) t)

let rec string_of_exp nest =
  let indent = String.make (nest * 2) ' ' in
  function
  Value v -> string_of_value v
| BinOp (op, lhs, rhs) -> "(_t){" ^ (string_of_intvar lhs) ^ " " ^ (S.string_of_op op) ^ " " ^ (string_of_intvar rhs) ^ "}"
| Apply (lhs, args) -> (string_of_value lhs) ^ ".func(" ^ (String.concat ", " @@ List.map string_of_value args) ^ ")"
| If (v, s1, s2) ->
    let stmt_true = String.concat "\n" @@ List.map (string_of_stmt (nest + 1)) s1 in
    let stmt_false = String.concat "\n" @@ List.map (string_of_stmt (nest + 1)) s2 in
    "if (" ^ (string_of_intvar v) ^ ") {\n" ^ stmt_true ^ "\n" ^ indent ^ "} else {\n" ^ stmt_false ^ indent ^ "\n" ^ indent ^ "}"
| Malloc _ -> err "must not malloc on string_of_exp"
| Read (v, idx) -> (string_of_value v) ^ ".tuple[" ^ (string_of_int idx) ^ "]"

and string_of_stmt nest =
  let indent = String.make (nest * 2) ' ' in
  function
    Assign (id, rhs) -> (
      match rhs with 
        Malloc vs ->
          let size = List.length vs in
          let vs_idx = enumerate 0 vs in
          let malloc_strs =
            List.map
            (fun (idx, v) ->
              match v with
                Fun f -> indent ^ id ^ ".tuple[" ^ (string_of_int idx) ^ "].func = " ^ (string_of_value v) ^ ";"
              | _ -> indent ^ id ^ ".tuple[" ^ (string_of_int idx) ^ "] = " ^ (string_of_value v) ^ ";"
            )
            vs_idx in
          indent ^ "_t " ^ id ^ ";\n" ^ indent ^ id ^ ".tuple = malloc(" ^ string_of_int size ^ " * sizeof(_t));" ^ "\n" ^ (String.concat "\n" malloc_strs)
      | _ -> indent ^ "const _t " ^ id ^ " = " ^ (string_of_exp nest rhs) ^ ";"
    )
  | VarDecl id -> indent ^ "_t " ^ id ^ ";"
  | VarAssign (id, exp) -> indent ^ id ^ " = " ^ (string_of_exp nest exp) ^ ";"
  | Expr exp -> indent ^ (string_of_exp nest exp) ^ ";"
  | Return exp -> indent ^ "return " ^ (string_of_exp nest exp) ^ ";"
  | Loop stmts ->
    let stmt_s = String.concat "\n" @@ List.map (string_of_stmt (nest + 1)) stmts in
    indent ^ "while (1){\n" ^ stmt_s ^ "\n" ^ indent ^ "}"
  | Recur (id, exp) ->
    indent ^ id ^ " = " ^ (string_of_exp nest exp) ^ ";\n" ^ indent ^ "continue;"

let string_of_decl (Decl (func, params, stmts)) =
  let stmts_s = List.map (string_of_stmt 1) stmts in
  let funchead = "_t " ^ func ^ "(" ^ (String.concat ", " @@ List.map (fun x -> "_t " ^ x) params) ^ ") {" in
  (String.concat "\n" (funchead::stmts_s)) ^ "\n}\n"

let c_header =
  "#include <stdio.h>\n" ^
  "#include <stdlib.h>\n"

let c_union =
  "typedef union MLType {\n" ^
  "  int value;\n" ^
  "  union MLType (*func)(union MLType, union MLType);\n" ^
  "  union MLType *tuple;\n" ^
  "} _t;\n"

let c_main =
  "int main (void) {\n" ^
  "  printf(\"%d\\n\", _toplevel((_t){0}, (_t){0}).value);\n" ^
  "  return 0;\n" ^
  "}\n"

let string_of_code code =
  let prototypes = List.map (fun (Decl (name, _, _)) -> "_t " ^ name ^ "(_t, _t);") code in
  let decls = List.rev_append (List.rev_map string_of_decl code) [c_main] in
  String.concat "\n" (c_header::c_union::prototypes@""::decls)

(* 文をたどって return <exp> を <var> = <exp> に変更する *)
let ret_to_varassign var =
  let rec r_exp = function
    If (v, s1, s2) -> If (v, List.map r_stmt s1, List.map r_stmt s2)
  | e -> e
  and r_stmt = function
    Return exp -> VarAssign (var, exp)
  | Expr e -> Expr (r_exp e)
  | Loop stmts -> Loop (List.map r_stmt stmts)
  | s -> s
  in r_stmt

let trans_body =
  let t_val = function
    F.Var x -> Var x
  | F.Fun f -> Fun f
  | F.IntV i -> IntV i
  in
  let rec t_cexp env = function
    F.ValExp value -> Value (t_val value)
  | F.BinOp (op, lhs, rhs) -> BinOp (op, t_val lhs, t_val rhs)
  | F.AppExp (v1, vs) -> Apply (t_val v1, List.map t_val vs)
  | F.IfExp (v, e1, e2) -> If (t_val v, t_exp env e1, t_exp env e2)
  | F.TupleExp vs -> Malloc (List.map t_val vs)
  | F.ProjExp (v, idx) -> Read (t_val v, idx)
  and t_exp env = function
    F.CompExp (F.IfExp (v, e1, e2)) -> [Expr (t_cexp env (F.IfExp (v, e1, e2)))]
  | F.CompExp cexp -> [Return (t_cexp env cexp)]
  | F.LetExp (id, F.IfExp (v, e1, e2), exp) ->
      let ifexp = ret_to_varassign id @@ Expr (t_cexp env (F.IfExp (v, e1, e2))) in
      (VarDecl id)::ifexp::(t_exp env exp)
  | F.LetExp (id, cexp, exp) ->
      let rhs = t_cexp env cexp in
      (Assign (id, rhs))::(t_exp env exp)
  | F.LoopExp (id, F.IfExp (v, e1, e2), exp) ->
      let newenv = Environment.extend "__loop__" id env in
      let ifexp = ret_to_varassign id @@ Expr (t_cexp env (F.IfExp (v, e1, e2))) in
      (VarDecl id)::ifexp::[Loop (t_exp newenv exp)]
  | F.LoopExp (id, cexp, exp) ->
      let newenv = Environment.extend "__loop__" id env in
      [VarDecl id; VarAssign (id, t_cexp env cexp); Loop (t_exp newenv exp)]
  | F.RecurExp v ->
      let loopvar = Environment.lookup "__loop__" env in
      [Recur (loopvar, Value (t_val v))]
  in t_exp Environment.empty

let trans_decl (F.RecDecl (proc_name, params, body)) =
  (* Decl (proc_name, params, [Return (Value (IntV 1))]) *)
  Decl (proc_name, params, trans_body body)

let compile = List.map trans_decl
