module S = Syntax

exception Error of string

let err s = raise (Error s)

type id = Vm.id
type label = Vm.label

type operand =
| Param of int
| Local of id
| Proc of label
| IntV of int

type stmt =
| VarDecl of id
| Move of id * operand (* local(ofs) <- op *)
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

type decl = Decl of label * stmt list

type code = decl list

let string_of_param p = "p" ^ string_of_int p
let string_of_local t = "t" ^ string_of_int t

let string_of_operand = function
| Param p -> string_of_param p
| Local t -> string_of_local t
| Proc l -> "(_t){.func = " ^ l ^ "}"
| IntV i -> "(_t){" ^ (string_of_int i) ^ "}"

let string_of_intvar = function
  IntV i -> string_of_int i
| Param p -> string_of_param p ^ ".value"
| Local t -> string_of_local t ^ ".value"
| Proc f -> err "function should not be a integer value"

let rec enumerate idx = function
    [] -> []
  | h::t -> (idx, h)::(enumerate (idx + 1) t)

let string_of_stmt = function
| VarDecl id -> "  _t " ^ string_of_local id ^ ";"
| Move (id, oper) -> "  " ^ string_of_local id ^ " = " ^ string_of_operand oper ^ ";"
| BinOp (id, op, lhs, rhs) ->
    "  " ^ string_of_local id ^ " = (_t){" ^ string_of_intvar lhs ^ " " ^ S.string_of_op op ^ " " ^ string_of_intvar rhs ^ "};"
| Label l -> l ^ ":;"
| BranchIf (oper, l) -> "  if (" ^ string_of_intvar oper ^ ") goto " ^ l ^ ";"
| Goto l -> "  goto " ^ l ^ ";"
| Call (id, oper, args) ->
    let str_of_args = String.concat ", " (List.map string_of_operand args) in
    "  " ^ string_of_local id ^ " = " ^ string_of_operand oper ^ ".func(" ^ str_of_args ^ ");"
| Return oper -> "  return " ^ string_of_operand oper ^ ";"
| Malloc (id, ops) ->
    let size = List.length ops in
    let str_of_assigns =
      snd @@
      List.fold_left
      (fun (idx, buf) op ->
        (idx + 1, buf ^ "  " ^ string_of_local id ^ ".tuple[" ^ string_of_int idx ^ "] = " ^ string_of_operand op ^ ";\n")
      )
      (0, "") ops in
    "  " ^ string_of_local id ^ ".tuple = malloc(" ^ string_of_int size ^ " * sizeof(_t));\n" ^
    "  if (" ^ string_of_local id ^ ".tuple == NULL) exit(EXIT_FAILURE);\n" ^
    "  on_exit(cleanup, " ^ string_of_local id ^ ".tuple);\n" ^
    str_of_assigns
| Read (id, oper, ofs) ->
    "  " ^ string_of_local id ^ " = " ^ string_of_operand oper ^ ".tuple[" ^ string_of_int ofs ^ "];"

let string_of_decl (Decl (func, stmts)) =
  let str_of_stmts = List.map string_of_stmt stmts in
  let funchead = "_t " ^ func ^ "(_t p0, _t p1) {" in
  (String.concat "\n" (funchead::str_of_stmts)) ^ "\n}\n"

let c_header =
"#include <stdio.h>
"

let c_union =
"typedef union MLType {
  int value;
  union MLType (*func)(union MLType, union MLType);
  union MLType *tuple;
} _t;
"

let c_cleanup =
"void cleanup (int status, void *tuple) {
  free(tuple);
}
"

let c_main =
"int main (void) {
  printf(\"%d\\n\", _toplevel((_t){0}, (_t){0}).value);
  return 0;
}
"

let string_of_code code =
  let prototypes = List.map (fun (Decl (name, _)) -> "_t " ^ name ^ "(_t, _t);") code in
  let decls = List.rev_append (List.rev_map string_of_decl code) [c_main] in
  String.concat "\n" (c_header::c_union::c_cleanup::prototypes@""::decls)

let trans_oper = function
| Vm.Param p -> Param p
| Vm.Local t -> Local t
| Vm.Proc f -> Proc f
| Vm.IntV i -> IntV i

let trans_body = function
| Vm.Move (id, oper) -> Move (id, trans_oper oper)
| Vm.BinOp (id, op, lhs, rhs) -> BinOp (id, op, trans_oper lhs, trans_oper rhs)
| Vm.Label l -> Label l
| Vm.BranchIf (oper, l) -> BranchIf (trans_oper oper, l)
| Vm.Goto l -> Goto l
| Vm.Call (id, oper, args) -> Call (id, trans_oper oper, List.map trans_oper args)
| Vm.Return oper -> Return (trans_oper oper)
| Vm.Malloc (id, args) -> Malloc (id, List.map trans_oper args)
| Vm.Read (id, oper, ofs) -> Read (id, trans_oper oper, ofs)
| Vm.BEGIN _ -> err "BEGIN"
| Vm.END _ -> err "END"

let trans_decl (Vm.ProcDecl (proc_name, nlocals, body)) =
  (* Decl (proc_name, params, [Return (Value (IntV 1))]) *)
  let transed_body = List.map trans_body body in
  let complete_body =
    let rec gen_vardecls n =
      if n < 0 then []
      else VarDecl n :: gen_vardecls (n - 1)
    in
    (gen_vardecls nlocals) @ transed_body in
  Decl (proc_name, complete_body)

let compile = List.map trans_decl
