module S = Syntax
open Arm_spec

exception Error of string
let err s = raise (Error s)

(* ==== メモリアクセス関連 ==== *)

(* 「reg中のアドレスからoffsetワード目」をあらわすaddr *)
let mem_access reg offset = RI (reg, offset * 4)

let local_access i = mem_access Fp (-i-2)

(* Vm.Param は 0 から数えるものと仮定 *)
let param_to_reg = function
    0 -> A1
  | 1 -> A2
  | i -> err ("invalid parameter: " ^ string_of_int i)

(* Vm.operandから値を取得し，レジスタrdに格納するような
   stmt listを生成 *)
let gen_operand rd = function
    Vm.Param i ->
      let rs = param_to_reg i in
      if rd = rs then [] else [Instr (Mov (rd, (R rs)))]
  | Vm.Local i -> [Instr (Ldr (rd, local_access i))]
  | Vm.Proc  lbl -> [Instr (Ldr (rd, L lbl))]
  | Vm.IntV  i -> [Instr (Mov (rd, I i))]

(* ==== 仮想マシンコード --> アセンブリコード ==== *)

(* V.decl -> loc list *)
let gen_decl (Vm.ProcDecl (name, nlocal, instrs)) =
  let convert = function
      Vm.Move (id, operand) ->
        let local_addr = local_access id in
        (gen_operand V1 operand) @ [Instr (Str (V1, local_addr))]
    | Vm.BinOp (id, op, oper1, oper2) ->
        let local_addr = local_access id in
        let header = (gen_operand V1 oper1) @ (gen_operand V2 oper2) in
        let body = (match op with
            S.Plus -> [Instr (Add (V1, V1, R V2))]
          | S.Mult -> [Instr (Mul (V1, V1, R V2))]
          | S.Lt ->
              let label_ok = fresh_label "lt_ok" in
              let label_end = fresh_label "lt_end" in
              [
                Instr (Cmp (V1, R V2));
                Instr (Blt label_ok);
                Instr (Mov (V1, I 0));
                Instr (B label_end);
                Label label_ok;
                Instr (Mov (V1, I 1));
                Label label_end;
              ]
        ) in
        let footer = [Instr (Str (V1, local_addr))] in
        header @ body @ footer
    | Vm.Label label -> [Label label]
    | Vm.BranchIf (oper, label) ->
        let header = gen_operand V1 oper in
        header @ [
          Instr (Cmp (V1, I 0));
          Instr (Bne label);
        ]
    | Vm.Goto label -> [Instr (B label)]
    | Vm.Call (id, opf, [op1; op2]) ->
        let local_addr = local_access id in
        let header = [
          Instr (Str (A1, RI (Sp, -4)));
          Instr (Str (A2, RI (Sp, -8)));
        ] @
        (gen_operand A1 op1) @
        (gen_operand A2 op2) @
        (gen_operand V1 opf) in
        let body = [
          Instr (Blx V1);
          Instr (Str (A1, local_addr));
          Instr (Ldr (A1, RI (Sp, -4)));
          Instr (Ldr (A2, RI (Sp, -8)));
        ]
        in header @ body
    | Vm.Call _ -> err "arienai call!!!!!!!!!!"
    | Vm.Malloc (id, ops) ->
        let local_addr = local_access id in
        let opnum = List.length ops in
        let header = [
          Instr (Str (A1, RI (Sp, 0)));
          Instr (Str (A2, RI (Sp, -4)));
          Instr (Mov (A1, I opnum));
          Instr (Bl "mymalloc");
        ] in
        let rec enumerate idx = function
            [] -> []
          | h::t -> (idx, h)::(enumerate (idx + 1) t)
        in
        let body = [
          Instr (Mov (V2, R A1));
          Instr (Str (V2, local_addr));
          Instr (Ldr (A1, RI (Sp, 0)));
          Instr (Ldr (A2, RI (Sp, -4)));
        ] in
        let footer = List.flatten @@ List.map (fun (idx, op) -> (gen_operand V1 op) @ [Instr (Str (V1, RI (V2, idx * 4)))]) @@ enumerate 0 ops
        in header @ body @ footer
    | Vm.Read (id, oper, index) ->
        let local_addr = local_access id in
        (gen_operand V2 oper) @
        [
          Instr (Ldr (V1, RI (V2, index * 4)));
          Instr (Str (V1, local_addr))
        ]
    | Vm.Return oper ->
        let header = gen_operand A1 oper in
        let name_ret = name ^ "_ret" in
        header @ [
          Instr (B name_ret);
        ]
    | Vm.BEGIN _ -> err "BEGIN yamero now"
    | Vm.END _ -> err "END yamero now"
  in
  let header = [
    Dir (D_align 2);
    Dir (D_global name);
    Label name
  ] in
  let funcstart = [
    Instr (Str (Fp, RI (Sp, 0)));
    Instr (Str (Lr, RI (Sp, -4)));
    Instr (Sub (Fp, Sp, I 4));
    Instr (Sub (Sp, Sp, I (4 * (nlocal))));
  ] in
  let footer = [
    Label (name ^ "_ret");
    Instr (Add (Sp, Fp, I 4));
    Instr (Ldr (Lr, RI (Sp, -4)));
    Instr (Ldr (Fp, RI (Sp, 0)));
    Instr (Bx Lr)
  ] in
  let body = List.flatten @@ List.map convert instrs in
  header @ funcstart @ body @ footer

(* entry point: Vm.decl list -> stmt list *)
let codegen vmprog =
  Dir D_text :: List.concat (List.map gen_decl vmprog)
