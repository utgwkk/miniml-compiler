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



(* entry point *)
let convert exp =
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
  in
  (* 自由変数をクロージャの環境参照に変える *)
  let rec change_freevar_name freevars f exp idx =
    match freevars with
      [] -> exp
    | h::t -> LetExp (h, ProjExp (Var f, idx), change_freevar_name t f exp (idx + 1))
  in
  (* クロージャを作りうるものの集合 *)
  (* id MySet.t ref *)
  let closure_funcs = ref MySet.empty in
  (* 変数名→関数ポインタの環境 *)
  (* id Environment.t ref *)
  let funcptr_env = ref Environment.empty in
  (* letの右辺式がクロージャを作りうるかどうか
   * 1. 右辺式が関数ポインタである
   * 2. 右辺式が関数適用である*)
  let cexp_makes_closure = function
      N.ValExp (N.Var x) -> MySet.member x !closure_funcs
    | N.AppExp (N.Var x, _) -> MySet.member x !closure_funcs
    | _ -> false
  in
  (* クロージャ変換の本体ここから *)
  let closure_conv_value value k = match value with
      N.Var x -> k (Var x)
    | N.IntV i -> k (IntV i)
  in
  let rec closure_conv_cexp cexp k = match cexp with
      N.ValExp v -> closure_conv_value v (fun newv -> k (ValExp newv))
    | N.BinOp (op, v1, v2) ->
        closure_conv_value v1 (fun newv1 ->
          closure_conv_value v2 (fun newv2 ->
            k (BinOp (op, newv1, newv2))
          )
        )
    | N.AppExp (N.Var x, v2) ->
        (* もとの関数はクロージャ表現になっているので，
         * そのかわりに関数ポインタを適用する *)
        let funcptr = try Environment.lookup x !funcptr_env with Environment.Not_bound -> x in
        closure_conv_value v2 (fun newv2 ->
          k (AppExp (Var funcptr, [Var x; newv2]))
        )
    | N.AppExp (N.IntV _, _) -> err "cannot apply function to int"
    | N.IfExp (v, e1, e2) ->
        closure_conv_value v (fun newv ->
          closure_conv_exp e1 (fun newe1 ->
            closure_conv_exp e2 (fun newe2 ->
              k (IfExp (newv, newe1, newe2))
            )
          )
        )
    | N.TupleExp (v1, v2) ->
        closure_conv_value v1 (fun newv1 ->
          closure_conv_value v2 (fun newv2 ->
            k (TupleExp [newv1; newv2])
          )
        )
    | N.ProjExp (v, i) ->
        closure_conv_value v (fun newv ->
          k (ProjExp (newv, i))
        )
  and closure_conv_exp exp k = match exp with
      N.CompExp cexp -> closure_conv_cexp cexp (fun newcexp -> k (CompExp newcexp))
    | N.LetExp (id, ce1, e2) ->
        closure_conv_cexp ce1 (fun newce1 -> 
          if cexp_makes_closure ce1 then (
            (* letの右辺式を正規化した結果が関数を指しているなら，
             * letの束縛変数も関数を指しているものとする *)
            closure_funcs := MySet.insert id !closure_funcs;
            let funcname = fresh_id "cl_func" in
            (* 変数名→関数ポインタの写像環境に x -> funcname を追加*)
            funcptr_env := Environment.extend id funcname !funcptr_env;
            closure_conv_exp e2 (fun newe2 -> k (LetExp (id, newce1, LetExp (funcname, ProjExp (Var id, 0), newe2))))
          ) else
            closure_conv_exp e2 (fun newe2 -> k (LetExp (id, newce1, newe2)))
        )
    | N.LetRecExp (f, x, e1, e2) ->
        let funcptr = fresh_id "funcptr" in
        closure_funcs := MySet.insert f !closure_funcs;
        (* 関数内の自由変数の集合を求める (関数ポインタも自由変数としておく) *)
        let freevars = (MySet.to_list @@ collect_freevars (MySet.from_list [f; x]) e1) in
        let freevar_vars = List.map (fun x -> Var x) (funcptr::freevars) in
        (* <> : フレッシュな変数
         * [] : クロージャ変換
         * / : 代入などの操作
         * let rec <fresh_funcptr> (f, x) =
         *   [e1/自由変数をクロージャ環境参照に変更する] in
         * let f = (<fresh_funptr>, (f内の自由変数の集合)...) in [e2]
         * *)
        closure_conv_exp e1 (fun newe1 ->
          closure_conv_exp e2 (fun newe2 ->
            k (LetRecExp (funcptr, [f; x], change_freevar_name freevars f newe1 1, LetExp (f, TupleExp freevar_vars, newe2)))
          )
        )
    | N.LoopExp (x, ce1, e2) ->
        closure_conv_cexp ce1 (fun newce1 ->
          closure_conv_exp e2 (fun newe2 ->
            k (LoopExp (x, newce1, newe2))
          )
        )
    | N.RecurExp v -> closure_conv_value v (fun newv -> k (RecurExp newv))
  in closure_conv_exp exp (fun x -> x)
