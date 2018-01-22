(* ==== ARM命令セット仕様 ==== *)

(* ==== 即値，ラベル ==== *)
type imm = int
type label = string

(* ==== レジスタ ==== *)
let nreg = 8 (* 使用する汎用レジスタの個数 *)

type reg =
  | Zero
  | At
  | V0
  | V1
  | A0
  | A1
  | A2
  | A3
  | T0
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6
  | T7
  | S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | T8
  | T9
  | Gp
  | Sp
  | Fp
  | Ra
  | Pc

(* ==== アドレッシング・モード ==== *)
type addr =
    I  of imm         (* 即値(Immediate) *)
  | R  of reg         (* レジスタ(Register) *)
  | L  of label       (* ラベル(Label) *)
  | RI of reg * imm   (* レジスタ間接(Register Indirect) *)

(* ==== 命令 ==== *)
type instr =
    Add  of reg * reg * reg
  | Addi of reg * reg * imm
  | Bne  of reg * reg * label
  | Blt  of reg * reg * label
  | J    of label
  | Jal  of addr
  | Jr   of reg
  | La   of reg * label
  | Lw   of reg * addr
  | Li   of reg * imm
  | Move of reg * reg
  | Mflo of reg
  | Mult of reg * reg
  | Sub  of reg * reg * reg
  | Slt  of reg * reg * reg
  | Slti  of reg * reg * imm
  | Sw   of reg * addr
  | Syscall

(* ==== アセンブラ指示子 ==== *)
type directive =
    D_word  of int
  | D_global of string
  | D_text

type stmt =
    Dir of directive
  | Label of label
  | Instr of instr

let string_of_reg r =
  match r with
  | Zero -> "$zero"
  | At -> "$at"
  | A0 -> "$a0"
  | A1 -> "$a1"
  | A2 -> "$a2"
  | A3 -> "$a3"
  | V0 -> "$v0"
  | V1 -> "$v1"
  | T0 -> "$t0"
  | T1 -> "$t1"
  | T2 -> "$t2"
  | T3 -> "$t3"
  | T4 -> "$t4"
  | T5 -> "$t5"
  | T6 -> "$t6"
  | T7 -> "$t7"
  | S0 -> "$s0"
  | S1 -> "$s1"
  | S2 -> "$s2"
  | S3 -> "$s3"
  | S4 -> "$s4"
  | S5 -> "$s5"
  | S6 -> "$s6"
  | S7 -> "$s7"
  | T8 -> "$t8"
  | T9 -> "$t9"
  | Gp -> "$gp"
  | Fp -> "$fp"
  | Sp -> "$sp"
  | Ra -> "$ra"
  | Pc -> "$pc"

let string_of_addr = function
    I i -> "#" ^ string_of_int i
  | R r -> string_of_reg r
  | L lbl -> lbl
  | RI (r, i) -> Printf.sprintf "%d(%s)" i (string_of_reg r)

let string_of_instr instr =
  let emit_instr op rands = op ^ "\t" ^ (String.concat ", " rands)
  in match instr with
    Add (r1, r2, r3) ->
      emit_instr "add" [string_of_reg r1; string_of_reg r2; string_of_reg r3]
  | Addi (r1, r2, i) ->
      emit_instr "addi" [string_of_reg r1; string_of_reg r2; string_of_int i]
  | Bne (r1, r2, lbl) -> emit_instr "bne" [string_of_reg r1; string_of_reg r2; lbl]
  | Blt (r1, r2, lbl) -> emit_instr "blt" [string_of_reg r1; string_of_reg r2; lbl]
  | J  lbl  -> emit_instr "j"  [lbl]
  | Jal lbl  -> emit_instr "jal" [string_of_addr lbl]
  | Jr r  -> emit_instr "jr" [string_of_reg r]
  | La (r1, l) ->
      emit_instr "la" [string_of_reg r1; l]
  | Li (r1, i) ->
      emit_instr "li" [string_of_reg r1; string_of_int i]
  | Lw (r1, addr) ->
      emit_instr "lw" [string_of_reg r1; string_of_addr addr]
  | Mflo r1 -> emit_instr "mflo" [string_of_reg r1]
  | Mult (r1, r2) ->
      emit_instr "mult" [string_of_reg r1; string_of_reg r2]
  | Move (r1, r2) ->
      emit_instr "move" [string_of_reg r1; string_of_reg r2]
  | Sw (r1, addr) ->
      emit_instr "sw" [string_of_reg r1; string_of_addr addr]
  | Slt (r1, r2, r3) ->
      emit_instr "slt" [string_of_reg r1; string_of_reg r2; string_of_reg r3]
  | Slti (r1, r2, i) ->
      emit_instr "slt" [string_of_reg r1; string_of_reg r2; string_of_int i]
  | Sub (r1, r2, r3) ->
     emit_instr "sub" [string_of_reg r1; string_of_reg r2; string_of_reg r3]
  | Syscall -> "syscall"

let string_of_directive = function
    D_word i  -> "word " ^ string_of_int i
  | D_global s -> "globl " ^ s
  | D_text     -> "text"

let string_of_stmt = function
    Instr i -> "\t" ^ string_of_instr i
  | Label lbl -> lbl ^ ":"
  | Dir   d -> "\t." ^ string_of_directive d

let string_of prog = String.concat "\n" (List.map string_of_stmt prog)
