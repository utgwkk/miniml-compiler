open Pretty
module S = Syntax
module N = Normal

exception Error of string
let err s = raise (Error s)

type id = S.id
type binOp = S.binOp

let fresh_id = N.fresh_id

(* ==== 値 ==== *)
type value =
    Var  of id
  | IntV of int

(* ==== 式 ==== *)
type cexp =
    ValExp    of value
  | BinOp     of binOp * value * value
  | AppExp    of value * value list     (* NEW *)
  | IfExp     of value * exp * exp
  | TupleExp  of value list             (* NEW *)
  | ProjExp   of value * int

and exp =
    CompExp   of cexp
  | LetExp    of id * cexp * exp
  | LetRecExp of id * id list * exp * exp  (* NEW *)
  | LoopExp   of id * cexp * exp
  | RecurExp  of value

(* ==== Formatter ==== *)

let string_of_closure e =
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
  let pr_of_values = function
      [] -> text "()"
    | v :: vs' ->
        (text "(" <*>
         (List.fold_left
            (fun d v -> d <*> text "," <+> pr_of_value v)
            (pr_of_value v) vs')
         <*> (text ")"))
  in
  let pr_of_ids = function
      [] -> text "()"
    | id :: ids' ->
        (text "(" <*>
         (List.fold_left
            (fun d i -> d <*> text "," <+> text i)
            (text id) ids')
         <*> (text ")"))
  in
  let rec pr_of_cexp p e =
    let enclose p' e = if p' < p then text "(" <*> e <*> text ")" else e in
    match e with
      ValExp v -> pr_of_value v
    | BinOp (op, v1, v2) ->
        enclose 2 (pr_of_value v1 <+> pr_of_op op <+> pr_of_value v2)
    | AppExp (f, vs) ->
        enclose 3 (pr_of_value f <+> pr_of_values vs)
    | IfExp (v, e1, e2) ->
        enclose 1
          (group ((nest 2
                     (group ((text "if 0 <")
                             <+> pr_of_value v
                             <+> text "then"
                             <|> pr_of_exp 1 e1))) <|>
                  (nest 2
                     (group (text "else" <|> pr_of_exp 1 e2)))))
    | TupleExp vs -> pr_of_values vs
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
    | LetRecExp (id, parms, body, e) ->
        enclose 1
          ((nest 2 (group (text "let" <+> text "rec" <+> text id <+>
                           pr_of_ids parms <+>
                           text "=" <|> pr_of_exp 1 body)))
           <+> text "in" <|> pr_of_exp 1 e)
    | LoopExp (id, ce, e) ->
        enclose 1
          ((nest 2 (group (text "loop" <+> text id <+>
                           text "=" <|> pr_of_cexp 1 ce)))
           <+> text "in" <|> pr_of_exp 1 e)
    | RecurExp v ->
        enclose 3 (text "recur" <+> pr_of_value v)
  in layout (pretty 40 (pr_of_exp 0 e))


(* 式中に登場する自由変数の集合を求める
 * exc は自由変数 *でないもの* の集合 *)
let collect_freevars exc exp =
  let c_val exc = function
      N.Var x -> if MySet.member x exc then MySet.empty else MySet.singleton x
    | N.IntV _ -> MySet.empty
  in
  let rec c_cexp exc = function
      N.ValExp v -> c_val exc v
    | N.BinOp (_, v1, v2) -> MySet.union (c_val exc v1) (c_val exc v2)
    | N.AppExp (v1, v2) -> MySet.union (c_val exc v1) (c_val exc v2)
    | N.IfExp (v, e1, e2) -> MySet.union (c_val exc v) (MySet.union (c_exp exc e1) (c_exp exc e2))
    | N.TupleExp (v1, v2) -> MySet.union (c_val exc v1) (c_val exc v2)
    | N.ProjExp (v, i) -> c_val exc v
  and c_exp exc = function
      N.CompExp ce -> c_cexp exc ce
    | N.LetExp (id, ce1, e2) ->
        let newexc = MySet.insert id exc in
        MySet.union (c_cexp exc ce1) (c_exp newexc e2)
    | N.LetRecExp (id, para, e1, e2) ->
        let recexc = MySet.insert id exc in
        MySet.union (c_exp (MySet.insert para recexc) e1) (c_exp recexc e2)
    | N.LoopExp (id, ce1, e2) ->
        let newexc = MySet.insert id exc in
        MySet.union (c_cexp exc ce1) (c_exp newexc e2)
    | N.RecurExp v -> c_val exc v
  in c_exp exc exp

(* exp中の自由変数 [fv1; fv2; ...] に対してクロージャの環境からの射影を代入する
 * let fv1 = f.1 in
 * let fv2 = f.2 in ... exp
 *)
let rec assign_freevars f exp idx = function
    [] -> exp
  | h::rest -> LetExp (h, ProjExp (Var f, idx), assign_freevars f exp idx rest)

(* cl_exp だけでは関数ポインタが用いられていないので，
 * 関数適用に対して関数ポインタを用いるように正規形の木構造を修正する．
 * AppExp が見つかるたびに新しい関数ポインタ参照変数を導入する
 * 右辺式 (cexp) になりうる場所を全て走査している
 *)
let apply_cl =
  let rec acl_cexp = function
      IfExp (v, e1, e2) -> IfExp (v, acl_exp e1, acl_exp e2)
    | ce -> ce
  and acl_exp = function
      CompExp (AppExp (Var f, vs)) ->
        let funcptrname = fresh_id "fp" in
        LetExp (funcptrname, ProjExp (Var f, 0), CompExp (AppExp (Var funcptrname, (Var f)::vs)))
    | CompExp ce -> CompExp (acl_cexp ce)
    | LetExp (id, ce, CompExp (AppExp (Var f, vs))) ->
        let funcptrname = fresh_id "fp" in
        LetExp (id, acl_cexp ce, LetExp (funcptrname, ProjExp (Var f, 0), CompExp (AppExp (Var funcptrname, (Var f)::vs))))
    | LetExp (id, AppExp (Var f, vs), e) ->
        let funcptrname = fresh_id "fp" in
        LetExp (funcptrname, ProjExp (Var f, 0), LetExp (id, AppExp (Var funcptrname, (Var f)::vs), acl_exp e))
    | LetExp (id, ce, e) -> LetExp (id, acl_cexp ce, acl_exp e)
    | LetRecExp (f, xs, e1, CompExp (AppExp (Var g, vs))) ->
        let funcptrname = fresh_id "fp" in
        LetRecExp (f, xs, acl_exp e1, LetExp (funcptrname, ProjExp (Var g, 0), CompExp (AppExp (Var funcptrname, (Var g)::vs))))
    | LetRecExp (f, xs, e1, e2) ->
        LetRecExp (f, xs, acl_exp e1, acl_exp e2)
    | LoopExp (id, AppExp (Var f, vs), e) ->
        let funcptrname = fresh_id "fp" in
        LetExp (funcptrname, ProjExp (Var f, 0), LoopExp (id, AppExp (Var funcptrname, (Var f)::vs), acl_exp e))
    | e -> e
  in acl_exp


(* entry point *)
let convert exp =
  let cl_value = function
      N.Var x -> Var x
    | N.IntV i -> IntV i
  in
  let rec cl_cexp cexp k = match cexp with
      N.ValExp v -> k (ValExp (cl_value v))
    | N.BinOp (op, v1, v2) ->
        k (BinOp (op, cl_value v1, cl_value v2))
    | N.AppExp (N.Var x, v2) ->
        k (AppExp (Var x, [cl_value v2]))
    | N.AppExp _ -> err "cannot apply"
    | N.IfExp (v, e1, e2) ->
        cl_exp e1 (fun e1' ->
          cl_exp e2 (fun e2' ->
            k (IfExp (cl_value v, e1', e2'))
          )
        )
    | N.TupleExp (v1, v2) ->
        k (TupleExp [cl_value v1; cl_value v2])
    | N.ProjExp (v, i) ->
        k (ProjExp (cl_value v, i))
  and cl_exp exp k = match exp with
      N.CompExp ce -> cl_cexp ce (fun ce' -> k (CompExp ce'))
    | N.LetExp (id, ce, e) -> (
        cl_cexp ce (fun ce' ->
          cl_exp e (fun e' ->
            k (LetExp (id, ce', e'))
          )
        )
      )
    | N.LetRecExp (f, x, e1, e2) ->
        (* let rec _b_func (f, x) = <e1> in let f = (_b_func, freevars...) in <e2> *)
        let newfuncname = fresh_id "b" in
        let freevars_set = collect_freevars (MySet.from_list [f; x]) e1 in
        let fvs = MySet.to_list freevars_set in
        cl_exp e1 (fun e1' ->
          cl_exp e2 (fun e2' ->
            k (LetRecExp (newfuncname, [f; x], assign_freevars f e1' 1 fvs,
                LetExp (f, TupleExp (List.map (fun x -> Var x) @@ newfuncname::fvs), e2'))
            )
          )
        )
    | N.LoopExp (id, ce, e) ->
        cl_cexp ce (fun ce' ->
          cl_exp e (fun e' ->
            k (LoopExp (id, ce', e'))
          )
        )
    | N.RecurExp v -> k (RecurExp (cl_value v))
  in cl_exp exp apply_cl
