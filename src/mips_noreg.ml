module S = Syntax
open Mips_spec

exception Error of string
let err s = raise (Error s)

(* ==== メモリアクセス関連 ==== *)

(* 「reg中のアドレスからoffsetワード目」をあらわすaddr *)
let mem_access reg offset = RI (reg, offset * 4)

let local_access i = mem_access Fp (-i-2)

(* Vm.Param は 0 から数えるものと仮定 *)
let param_to_reg = function
    0 -> A0
  | 1 -> A1
  | 2 -> A2
  | 3 -> A3
  | i -> err ("invalid parameter: " ^ string_of_int i)

(* Vm.operandから値を取得し，レジスタrdに格納するような
   stmt listを生成 *)
let gen_operand rd = function
    Vm.Param i ->
      let rs = param_to_reg i in
      if rd = rs then [] else [Instr (Move (rd, rs))]
  | Vm.Local i -> [Instr (Lw (rd, local_access i))]
  | Vm.Proc  lbl -> [Instr (La (rd, lbl))]
  | Vm.IntV  i -> [Instr (Li (rd, i))]

(* ==== 仮想マシンコード --> アセンブリコード ==== *)

let fresh_label = Misc.fresh_id_maker "_"

(* V.decl -> loc list *)
let gen_decl (Vm.ProcDecl (name, nlocal, instrs)) =
  let convert = function
      Vm.Move (id, operand) ->
        let local_addr = local_access id in
        (gen_operand T0 operand) @ [Instr (Sw (T0, local_addr))]
    | Vm.BinOp (id, op, oper1, oper2) ->
        let local_addr = local_access id in
        let header = (gen_operand T0 oper1) @ (gen_operand T1 oper2) in
        let body = (match op with
            S.Plus -> [Instr (Add (T0, T0, T1))]
          | S.Mult ->
            [
              Instr (Mult (T0, T1));
              Instr (Mflo T0)
            ]
          | S.Lt -> [Instr (Slt (T0, T0, T1))]
        ) in
        let footer = [Instr (Sw (T0, local_addr))] in
        header @ body @ footer
    | Vm.Label label -> [Label label]
    | Vm.BranchIf (oper, label) ->
        let header = gen_operand T0 oper in
        header @ [
          Instr (Blt (Zero, T0, label));
        ]
    | Vm.Goto label -> [Instr (J label)]
    | Vm.Call (id, opf, [op1; op2]) ->
        let local_addr = local_access id in
        let ret_label = fresh_label "ret" in
        let header = 
        [
          Instr (Sw (A0, RI (Sp, 0)));
          Instr (Sw (A1, RI (Sp, 4)));
        ] @
        (gen_operand A0 op1) @
        (gen_operand A1 op2) @ 
        (gen_operand T0 opf) @ [
          Instr (Sw (Ra, RI (Sp, 8)));
          Instr (La (Ra, ret_label));
          Instr (Jal (R T0))
        ]
        in
        let body = [
          Label ret_label;
          Instr (Lw (A0, RI (Sp, 0)));
          Instr (Lw (A1, RI (Sp, 4)));
          Instr (Sw (V0, local_addr));
        ]
        in header @ body
    | Vm.Call _ -> err "arienai call!!!!!!!!!!"
    | Vm.Malloc (id, ops) ->
        let local_addr = local_access id in
        let opnum = List.length ops in
        let header = [
          Instr (Sw (A0, RI (Sp, 0)));
          Instr (Li (A0, opnum * 4));
          Instr (Li (V0, 9));
          Instr Syscall
        ] in
        let rec enumerate idx = function
            [] -> []
          | h::t -> (idx, h)::(enumerate (idx + 1) t)
        in
        let body = [
          Instr (Lw (A0, RI (Sp, 0)));
          Instr (Move (T1, V0));
          Instr (Sw (T1, local_addr));
        ] in
        let footer = List.flatten @@ List.map (fun (idx, op) -> (gen_operand T0 op) @ [Instr (Sw (T0, RI (T1, idx * 4)))]) @@ enumerate 0 ops
        in header @ body @ footer
    | Vm.Read (id, oper, index) ->
        let local_addr = local_access id in
        (gen_operand T1 oper) @
        [
          Instr (Lw (T0, RI (T1, index * 4)));
          Instr (Sw (T0, local_addr))
        ]
    | Vm.Return oper ->
        if name = "_toplevel" then
        gen_operand A0 oper @ 
        [
          Instr (Li (V0, 1));
          Instr Syscall
        ]
        else gen_operand V0 oper 
    | Vm.BEGIN _ -> err "BEGIN yamero now"
    | Vm.END _ -> err "END yamero now"
  in
  let name' =
    if name = "_toplevel" then "main"
    else name
  in
  let header = [
    Dir (D_global name');
    Label name'
  ] in
  let funcstart = [
    Instr (Sw (Ra, RI (Sp, -8)));
    Instr (Sw (Fp, RI (Sp, -4)));
    Instr (Addi (Fp, Sp, -4));
    Instr (Addi (Sp, Sp, -(4 * (nlocal + 4))));
  ] in
  let footer = [
    Instr (Addi (Sp, Fp, 4));
    Instr (Lw (Ra, RI (Fp, -4)));
    Instr (Lw (Fp, RI (Fp, 0)));
    Instr (Jr Ra)
  ] in
  let body = List.flatten @@ List.map convert instrs in
  header @ funcstart @ body @ footer

(* entry point: Vm.decl list -> stmt list *)
let codegen vmprog =
  Dir D_text :: List.concat (List.map gen_decl vmprog)
