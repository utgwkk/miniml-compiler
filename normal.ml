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
  (* 言語C上で定数畳み込みを行う
   * - 定数
   * - if式
   * - タプルの射影 *)
  let rec fold_const exp k =
    (* 二項演算を計算する *)
    let apply_prim op e1 e2 = match op, e1, e2 with
        S.Plus, S.ILit i1, S.ILit i2 -> Some (S.ILit (i1 + i2))
      | S.Plus, _, _ -> None
      | S.Mult, S.ILit i1, S.ILit i2 -> Some (S.ILit (i1 * i2))
      | S.Mult, _, _ -> None
      | S.Lt, S.ILit i1, S.ILit i2 -> Some (S.BLit (i1 < i2))
      | S.Lt, _, _ -> None
    in
      match exp with
    | S.BinOp (op, e1, e2) ->
        fold_const e1 (fun lhs ->
          fold_const e2 (fun rhs ->
            match (apply_prim op lhs rhs) with
              Some res -> k res
            | None -> k exp
      ))
    | S.IfExp (e1, e2, e3) ->
        fold_const e1 (fun cond ->
          fold_const e2 (fun ok ->
            fold_const e3 (fun ng ->
              if cond = S.BLit true then k ok
              else if cond = S.BLit false then k ng
              else k (S.IfExp (cond, ok, ng))
      )))
    | S.LetExp (id, e1, e2) ->
        fold_const e1 (fun x ->
          fold_const e2 (fun y -> k (S.LetExp (id, x, y))
      ))
    | S.FunExp (id, e) ->
        fold_const e (fun x -> k (S.FunExp (id, x))
      )
    | S.AppExp (e1, e2) ->
        fold_const e1 (fun x ->
          fold_const e2 (fun y -> k (S.AppExp (x, y))
      ))
    | S.LetRecExp (id, para, e1, e2) ->
        fold_const e1 (fun x ->
          fold_const e2 (fun y -> k (S.LetRecExp (id, para, x, y))
      ))
    | S.LoopExp (id, e1, e2) ->
        fold_const e1 (fun x ->
          fold_const e2 (fun y -> k (S.LoopExp (id, x, y))
      ))
    | S.RecurExp e ->
        fold_const e (fun x -> k (S.RecurExp x)
      )
    | S.TupleExp (e1, e2) ->
        fold_const e1 (fun x ->
          fold_const e2 (fun y -> k (S.TupleExp (x, y))
      ))
    | S.ProjExp (e, i) ->
        fold_const e (fun tup ->
            match tup with
          S.TupleExp (fst, snd) -> k (if i = 1 then fst else if i = 2 then snd else err "index out of range")
        | _ -> k (S.ProjExp (tup, i))
      )
    | _ -> k exp
  in
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
        let projvar = fresh_id "proj" in
        convert_I env e (fun x -> k (S.LetExp (projvar, x, S.ProjExp (S.Var projvar, i))))
  in
  (* 言語C上でベータ簡約を行う *)
  let rec beta env exp k =
      match exp with
      S.Var x ->
        let newvar = try Environment.lookup x env with Environment.Not_bound -> x in
        k (S.Var newvar)
    | S.ILit i -> k (S.ILit i)
    | S.BinOp (op, e1, e2) ->
        beta env e1 (fun x ->
          beta env e2 (fun y -> k (
          S.BinOp (op, x, y))
        )
      )
    | S.IfExp (e1, e2, e3) ->
        beta env e1 (fun x ->
          beta env e2 (fun y ->
            beta env e3 (fun z -> k (
            S.IfExp (x, y, z))
          )
        )
      )
    | S.LetExp (id, e1, e2) -> (
          match e1 with
        S.Var id2 -> let newenv = Environment.extend id id2 env in beta newenv e2 k
      | e ->
        beta env e1 (fun x ->
          beta env e2 (fun y -> k (
          S.LetExp (id, x, y))
          )
        )
    )
    | S.FunExp (id, e) ->
        beta env e (fun x -> k (S.FunExp (id, x)))
    | S.AppExp (e1, e2) ->
        beta env e1 (fun x ->
          beta env e2 (fun y -> k (
          S.AppExp (x, y))
        )
      )
    | S.LetRecExp (id, para, e1, e2) ->
        beta env e1 (fun x ->
          beta env e2 (fun y -> k (
          S.LetRecExp (id, para, x, y))
        )
      )
    | S.LoopExp (id, e1, e2) ->
        beta env e1 (fun x ->
          beta env e2 (fun y -> k (
          S.LoopExp (id, x, y))
        )
      )
    | S.RecurExp e -> beta env e (fun x -> k (S.RecurExp x))
    | S.TupleExp (e1, e2) ->
        beta env e1 (fun x ->
          beta env e2 (fun y -> k (
          S.TupleExp (x, y))
        )
      )
    | S.ProjExp (e, i) ->
        beta env e (fun x -> k (S.ProjExp (x, i)))
  in
  (* 言語C上でコピー伝播を行う *)
  let rec copy_propagation (map : (S.id, S.exp) MyMap.t) (exp : S.exp) k =
      match exp with
      S.Var x -> (
        match MyMap.get x map with
          Some newexp -> k newexp
        | None -> k exp
    )
    | S.BinOp (op, e1, e2) ->
        copy_propagation map e1 (fun x ->
          copy_propagation map e2 (fun y -> k (
            S.BinOp (op, x, y))
          )
        )
    | S.IfExp (e1, e2, e3) ->
        copy_propagation map e1 (fun x ->
          copy_propagation map e2 (fun y ->
            copy_propagation map e3 (fun z -> k (
              S.IfExp (x, y, z))
            )
          )
        )
    | S.LetExp (id, e1, e2) -> (
        match e1 with
      | S.ILit _ ->
          let newmap = MyMap.merge map (MyMap.singleton id e1) in
          copy_propagation newmap e2 (fun x -> k x)
      | _ -> copy_propagation map e1 (fun x ->
               copy_propagation map e2 (fun y -> k (S.LetExp (id, x, y))
            )
          )
    )
    | S.FunExp (id, e) ->
        copy_propagation map e (fun x -> k (S.FunExp (id, x)))
    | S.AppExp (e1, e2) ->
        copy_propagation map e1 (fun x ->
          copy_propagation map e2 (fun y -> k (
            S.AppExp (x, y))
          )
        )
    | S.LetRecExp (func, id, e1, e2) ->
        copy_propagation map e1 (fun x ->
          copy_propagation map e2 (fun y -> k (
            S.LetRecExp (func, id, x, y)
        )
      )
    )
    | S.LoopExp (id, e1, e2) ->
        copy_propagation map e1 (fun x ->
          copy_propagation map e2 (fun y -> k (
            S.LoopExp (id, x, y))
      )
    )
    | S.RecurExp e ->
        copy_propagation map e (fun x -> k (S.RecurExp x))
    | S.TupleExp (e1, e2) ->
        copy_propagation map e1 (fun x ->
          copy_propagation map e2 (fun y -> k (S.TupleExp (x, y))
      )
    )
    | S.ProjExp (e, i) ->
        copy_propagation map e (fun x -> k (S.ProjExp (x, i)))
    | _ -> k exp
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
    | S.LoopExp (x, e1, e2) -> LoopExp (x, ValExp (value e1), e2 |> convert_N)
    | S.RecurExp e -> RecurExp (value e)
    | S.TupleExp (e1, e2) -> CompExp (TupleExp (value e1, value e2))
    | S.ProjExp (e, i) -> CompExp (ProjExp (value e, i))
  in
  (* 変換を適用した結果が変わらなくなるまで適用する *)
  let rec until_fix f e k =
    let applied = f e (fun x -> x) in
    if e = applied then k applied
    else until_fix f applied k
  in
  until_fix fold_const e (fun x ->
  convert_I (Environment.empty) x (fun x ->
  beta (Environment.empty) x (fun x ->
  until_fix (fun x -> (copy_propagation MyMap.empty) x fold_const) x (fun x ->
  convert_N x
  ))))


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
