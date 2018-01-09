open Pretty
module S = Syntax

exception Error of string
let err s = raise (Error s)

type id = S.id
type binOp = S.binOp

let fresh_id = Misc.fresh_id_maker "_"

(* ==== 値 ==== *)
type value =
    Var  of id
  | IntV of int

(* ==== 式 ==== *)
type cexp =
    ValExp    of value
  | BinOp     of binOp * value * value
  | AppExp    of value * value
  | IfExp     of value * exp * exp
  | TupleExp  of value * value
  | ProjExp   of value * int

and exp =
    CompExp   of cexp
  | LetExp    of id * cexp * exp
  | LetRecExp of id * id * exp * exp
  | LoopExp   of id * cexp * exp
  | RecurExp  of value

(* ==== Formatter ==== *)

let string_of_norm e =
  let pr_of_op = function
      S.Plus -> text "+"
    | S.Mult -> text "*"
    | S.Lt -> text "<" in
  let pr_of_value = function
      Var id -> text id
    | IntV i ->
        let s = text (string_of_int i) in
        if i < 0 then text "(" <*> s <*> text ")" else s
  in
  let rec pr_of_cexp p e =
    let enclose p' e = if p' < p then text "(" <*> e <*> text ")" else e in
    match e with
      ValExp v -> pr_of_value v
    | BinOp (op, v1, v2) ->
        enclose 2 (pr_of_value v1 <+> pr_of_op op <+> pr_of_value v2)
    | AppExp (f, v) ->
        enclose 3 (pr_of_value f <+> pr_of_value v)
    | IfExp (v, e1, e2) ->
        enclose 1
          (group ((nest 2
                     (group ((text "if 0 <")
                             <+> pr_of_value v
                             <+> text "then"
                             <|> pr_of_exp 1 e1))) <|>
                  (nest 2
                     (group (text "else" <|> pr_of_exp 1 e2)))))
    | TupleExp (v1, v2) ->
        text "(" <*> pr_of_value v1 <*> text ","
        <+> pr_of_value v2 <*> text ")"
    | ProjExp (v, i) ->
        enclose 2 (pr_of_value v <*> text "." <*> text (string_of_int i))
  and pr_of_exp p e =
    let enclose p' e = if p' < p then text "(" <*> e <*> text ")" else e in
    match e with
      CompExp ce -> pr_of_cexp p ce
    | LetExp (id, ce, e) ->
        enclose 1
          ((nest 2 (group (text "let" <+> text id <+>
                           text "=" <|> pr_of_cexp 1 ce)))
           <+> text "in" <|> pr_of_exp 1 e)
    | LetRecExp (id, v, body, e) ->
        enclose 1
          ((nest 2 (group (text "let" <+> text "rec" <+>
                           text id <+> text v <+>
                           text "=" <|> pr_of_exp 1 body)))
           <+> text "in" <|> pr_of_exp 1 e)
    | LoopExp (id, ce, e) ->
        enclose 1
          ((nest 2 (group (text "loop" <+> text id <+>
                           text "=" <|> pr_of_cexp 1 ce)))
           <+> text "in" <|> pr_of_exp 1 e)
    | RecurExp v ->
        enclose 3 (text "recur" <+> pr_of_value v)
  in layout (pretty 30 (pr_of_exp 0 e))


(* ==== 正規形への変換 ==== *)

and normalize e =
  (* MiniML -> 言語Cへの変換I (継続渡しスタイル) *)
  let rec convert_I env exp k =
    match exp with
      S.Var x ->
        let newid = try (Environment.lookup x env) with Environment.Not_bound -> x in
        k (S.Var newid)
    | S.ILit i -> k (S.ILit i)
    | S.BLit true -> k (S.ILit 1)
    | S.BLit false -> k (S.ILit 0)
    | S.BinOp (op, e1, e2) ->
        let lhs = fresh_id "lhs" in
        let rhs = fresh_id "rhs" in
        convert_I env e1
          (fun x -> convert_I env e2 (
            fun y -> k (S.LetExp (lhs, x, S.LetExp (rhs, y, S.BinOp (op, S.Var lhs, S.Var rhs)))))
          )
    | S.IfExp (e1, e2, e3) ->
        let cond = fresh_id "cond" in
        convert_I env e1
          (fun x -> convert_I env e2 (
            fun y -> convert_I env e3 (
              fun z -> k (S.LetExp (cond, x, S.IfExp (S.Var cond, y, z))))
            )
          )
    | S.LetExp (id, e1, e2) ->
        let var = fresh_id "let" in
        let newenv = Environment.extend id var env in
        convert_I env e1
          (fun x -> convert_I newenv e2
            (fun y -> k (S.LetExp (var, x, y))
            )
          )
    | S.FunExp (x, e) ->
        let func = fresh_id "fun" in
        convert_I env (S.LetRecExp (func, x, e, S.Var func)) k
    | S.AppExp (e1, e2) ->
        let appfunc = fresh_id "app" in
        let appval = fresh_id "appval" in
        convert_I env e1 (fun x ->
          convert_I env e2 (fun y ->
            k (S.LetExp (appfunc, x, S.LetExp (appval, y, S.AppExp (S.Var appfunc, S.Var appval))))
          )
        )
    | S.LetRecExp (f, x, e1, e2) ->
        let func = fresh_id "letrec" in
        let arg = fresh_id "letrecarg" in
        let newenv = Environment.extend f func env in
        convert_I (Environment.extend x arg newenv) e1 (fun x ->
          convert_I newenv e2 (fun y ->
            k (S.LetRecExp (func, arg, x, y))
          )
        )
    | S.LoopExp (id, e1, e2) ->
        let var = fresh_id "loop" in
        let newenv = Environment.extend id var env in
        convert_I env e1 (fun x ->
          convert_I newenv e2 (fun y ->
            k (S.LoopExp (var, x, y))
          )
        )
    | S.RecurExp e ->
        let var = fresh_id "recur" in
        convert_I env e (fun x -> k (S.LetExp (var, x, S.RecurExp (S.Var var))))
    | S.TupleExp (e1, e2) ->
        let tup_fstvar = fresh_id "tuple_fst" in
        let tup_sndvar = fresh_id "tuple_snd" in
        let tupvar = fresh_id "tuple" in
        convert_I env e1 (fun x ->
          convert_I env e2 (fun y -> k (
            S.LetExp (tup_fstvar, x,
              S.LetExp (tup_sndvar, y,
                S.LetExp (tupvar, S.TupleExp (S.Var tup_fstvar, S.Var tup_sndvar), S.Var tupvar))
              )
            )
          )
        )
    | S.ProjExp (e, i) ->
        (* make i 0-index *)
        if not (i = 1 || i = 2) then err "projection index must be 1 or 2.";
        let projvar = fresh_id "proj" in
        convert_I env e (fun x -> k (S.LetExp (projvar, x, S.ProjExp (S.Var projvar, i - 1))))
  in
  (* 言語C -> 正規形への変換 *)
  let rec convert_N exp =
    (* let x = e1 in e2 / loop x = e1 in e2 の e1 中のletを外に出す *)
    let rec unnest_let exp =
      match exp with
        S.LetExp (x, e1, e2) -> (
          match e1 with
            S.LetExp (ix, ie1, ie2) -> unnest_let @@ S.LetExp (ix, ie1, unnest_let @@ S.LetExp (x, ie2, e2))
          | S.LetRecExp (ifun, ix, ie1, ie2) -> S.LetRecExp (ifun, ix, unnest_let ie1, unnest_let @@ S.LetExp (x, ie2, e2))
          | _ -> S.LetExp (x, e1, unnest_let e2)
      )
      | S.LetRecExp (f, x, e1, e2) -> S.LetRecExp (f, x, e1, unnest_let e2)
      | S.LoopExp (x, e1, e2) -> (
          match e1 with
            S.LetExp (ix, ie1, ie2) -> unnest_let @@ S.LetExp (ix, ie1, unnest_let @@ S.LoopExp (x, ie2, e2))
          | S.LetRecExp (f, ix, ie1, ie2) -> S.LetRecExp (f, ix, ie1, unnest_let @@ S.LoopExp (x, ie2, e2))
          | _ -> S.LoopExp (x, e1, unnest_let e2)
      )
      | _ -> exp
    in
    let value = function
        S.Var x -> Var x
      | S.ILit i -> IntV i
      | _ -> err "cannot convert to value"
    in
    let unnested_exp = unnest_let exp in
    match unnested_exp with
      S.Var x -> CompExp (ValExp (Var x))
    | S.ILit i -> CompExp (ValExp (IntV i))
    | S.BLit _ -> err "boolean should be converted to integer on convert_I"
    | S.BinOp (op, v1, v2) -> CompExp (BinOp (op, value v1, value v2))
    | S.IfExp (e1, e2, e3) -> CompExp (IfExp (value e1, e2 |> convert_N, e3 |> convert_N))
    | S.LetExp (x, e1, e2) -> (
        match (convert_N e1) with
          CompExp ce -> LetExp (x, ce, e2 |> convert_N)
        | _ -> err "cannot uncomp"
    )
    | S.FunExp _ -> err "funexp should be converted to letrecexp on convert_I"
    | S.AppExp (e1, e2) -> CompExp (AppExp (value e1, value e2))
    | S.LetRecExp (f, x, e1, e2) -> LetRecExp (f, x, e1 |> convert_N, e2 |> convert_N)
    | S.LoopExp (x, e1, e2) -> (
        match (convert_N e1) with
          CompExp ce -> LoopExp (x, ce, e2 |> convert_N)
        | _ -> err "cannot uncomp"
    )
    | S.RecurExp e -> RecurExp (value e)
    | S.TupleExp (e1, e2) -> CompExp (TupleExp (value e1, value e2))
    | S.ProjExp (e, i) -> CompExp (ProjExp (value e, i))
  in
  convert_I (Environment.empty) e convert_N


(* ==== recur式が末尾位置にのみ書かれていることを検査 ==== *)

let recur_check e =
  let rec check e ok = match e with
    S.BinOp (_, e1, e2) ->
      check e1 false;
      check e2 false;
  | S.IfExp (e1, e2, e3) ->
      check e1 false;
      check e2 ok;
      check e3 ok;
  | S.LetExp (_, e1, e2) ->
      check e1 false;
      check e2 ok;
  | S.FunExp (_, e) -> check e false;
  | S.AppExp (e1, e2) ->
      check e1 false;
      check e2 false;
  | S.LetRecExp (_, _, e1, e2) ->
      check e1 false;
      check e2 ok;
  | S.LoopExp (_, e1, e2) ->
      check e1 false;
      check e2 true;
  | S.RecurExp _ -> if ok then () else err "recur check failed"
  | S.TupleExp (e1, e2) ->
      check e1 false;
      check e2 false;
  | S.ProjExp (e, _) -> check e false;
  | _ -> ()
  in check e false


(* ==== entry point ==== *)
let rec convert prog =
  recur_check prog;
  normalize prog
