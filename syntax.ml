exception Error of string
let err s = raise (Error s)

type id = string

type binOp = Plus | Mult | Lt

let string_of_op = function
    Plus -> "+"
  | Mult -> "*"
  | Lt   -> "<"

type exp =
    Var       of id
  | ILit      of int
  | BLit      of bool
  | BinOp     of binOp * exp * exp
  | IfExp     of exp * exp * exp
  | LetExp    of id * exp * exp
  | FunExp    of id * exp
  | AppExp    of exp * exp
  | LetRecExp of id * id * exp * exp
  | LoopExp   of id * exp * exp (* loop <id> = <exp> in <exp> *)
  | RecurExp  of exp            (* recur <exp> *)
  | TupleExp  of exp * exp      (* (<exp>, <exp>) *)
  | ProjExp   of exp * int      (* <exp> . <int> *)

let rec print_ast ast = match ast with
  Var x -> print_string x
| ILit i -> print_int i
| BLit true -> print_string "true"
| BLit false -> print_string "false"
| BinOp (op, lhs, rhs) ->
    print_string "(";
    print_ast lhs;
    print_string @@ string_of_op op;
    print_ast rhs;
    print_string ")"
| IfExp (e1, e2, e3) ->
    print_string "if ";
    print_ast e1;
    print_string " then\n";
    print_ast e2;
    print_string "\nelse\n";
    print_ast e3
| LetExp (x, e1, e2) ->
    print_string "let ";
    print_string x;
    print_string " = ";
    print_ast e1;
    print_string " in\n";
    print_ast e2
| FunExp (x, e) ->
    print_string "fun ";
    print_string x;
    print_string " -> ";
    print_ast e
| LetRecExp (f, x, e1, e2) ->
    print_string "let rec ";
    print_string f;
    print_string " ";
    print_string x;
    print_string " = ";
    print_ast e1;
    print_string " in\n";
    print_ast e2
| AppExp (e1, e2) ->
    print_string "(";
    print_ast e1;
    print_string ") ";
    print_string "(";
    print_ast e2;
    print_string ")";
| LoopExp (x, e1, e2) ->
    print_string "loop ";
    print_string x;
    print_string " = ";
    print_ast e1;
    print_string " in ";
    print_ast e2;
| RecurExp e ->
    print_string "recur ";
    print_ast e;
| TupleExp (e1, e2) ->
    print_string "(";
    print_ast e1;
    print_string ", ";
    print_ast e2;
    print_string ")"
| ProjExp (e, i) ->
    print_ast e;
    print_string ".";
    print_int i;
