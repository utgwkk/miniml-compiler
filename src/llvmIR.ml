open Llvm
module F = Flat
module S = Syntax

exception Error of string

(* entrypoint *)
let codegen filename (prog : F.prog) = 
  let context = create_context () in
  let the_module = create_module context filename in
  let builder = builder context in
  let int_t = i64_type context in

  let codegen_value env = function
    | F.Var x -> Environment.lookup x env
    | F.Fun x ->
        begin match lookup_function x the_module with
          | Some f -> const_ptrtoint f int_t
          | None -> raise (Error ("function " ^ x ^ " does not exist"))
        end
    | F.IntV i -> const_int int_t i
  in

  let rec codegen_cexp env = function
    | F.ValExp v -> codegen_value env v
    | F.BinOp (op, v1, v2) ->
        let lhs = codegen_value env v1 in
        let rhs = codegen_value env v2 in
        begin match op with
          | S.Plus -> build_add lhs rhs "addtmp" builder
          | S.Mult -> build_mul lhs rhs "multmp" builder
          | S.Lt -> build_icmp Icmp.Slt lhs rhs "cmptmp" builder
        end
    | F.AppExp (vf, varg) ->
        let vf' = codegen_value env vf in
        (* Cast function pointer *)
        let pointer_t =
          function_type int_t [|int_t ; int_t|]
          |> pointer_type
        in
        let vf' =
          build_inttoptr vf' pointer_t "fptr" builder
        in
        (* Cast all arguments to i64 *)
        let varg' =
          List.map (codegen_value env) varg
          |> List.map (fun v ->
              build_pointercast v int_t "callcast" builder
            )
          |> Array.of_list
        in
        build_call vf' varg' "call" builder
    | F.IfExp (v, e1, e2) ->
        let bool_t = i1_type context in
        let cond = build_trunc (codegen_value env v) bool_t "bool" builder in
        let zero = const_int bool_t 0 in
        let cond_val = build_icmp Icmp.Ult zero cond "ifcond" builder in
        let start_bb = insertion_block builder in
        let func = block_parent start_bb in
        let then_bb = append_block context "then" func in

        (* then *)
        position_at_end then_bb builder;
        let then_val = codegen_exp env e1 in
        let new_then_bb = insertion_block builder in

        (* else *)
        let else_bb = append_block context "else" func in
        position_at_end else_bb builder;
        let else_val = codegen_exp env e2 in

        let new_else_bb = insertion_block builder in

        (* merging path *)
        let merge_bb = append_block context "ifcont" func in
        position_at_end merge_bb builder;
        let incoming =
          [(then_val, new_then_bb); (else_val, new_else_bb)]
        in
        let phi = build_phi incoming "iftmp" builder in
        position_at_end start_bb builder;
        ignore (build_cond_br cond_val then_bb else_bb builder);

				position_at_end new_then_bb builder;
        ignore (build_br merge_bb builder);
				position_at_end new_else_bb builder;
        ignore (build_br merge_bb builder);
        
        position_at_end merge_bb builder;
        phi
    | F.TupleExp vs ->
        let vs' =
          List.map (codegen_value env) vs
          |> Array.of_list
        in
        let elem_types = Array.map (fun v -> type_of v) vs' in
        let tuple_t = struct_type context elem_types in
        let tupleaddr = build_malloc tuple_t "tuple" builder in
        Array.iteri (fun idx v' ->
          let idx' = const_int (i32_type context) idx in
          let addr = build_in_bounds_gep tupleaddr [|const_int int_t 0 ; idx'|] "tupleelem" builder in
          let v' = build_bitcast v' int_t "tuplecast" builder in
          ignore (build_store v' addr builder)
        ) vs';
        (* Cast to int *)
        build_pointercast tupleaddr int_t "tupleaddr" builder
    | F.ProjExp (v, idx) ->
        let v' = codegen_value env v in
        let idx' = const_int (i32_type context) idx in
        (* Cast to struct *)
        let ptr_type =
          struct_type context (Array.init (idx + 1) (fun _ -> int_t))
          |> pointer_type
        in
        let casted =
          build_inttoptr v' ptr_type "tupleptr" builder
        in
        let ptr = build_in_bounds_gep casted [|const_int int_t 0 ; idx'|] "projelem" builder in
        build_load ptr "proj" builder
  and codegen_exp env = function
    | F.CompExp ce -> codegen_cexp env ce
    | F.LetExp (id, ce, e) ->
        let ce' = codegen_cexp env ce in
        let newenv = Environment.extend id ce' env in
        codegen_exp newenv e
    | F.LoopExp (id, ce, e) ->
        let ce' = codegen_cexp env ce in
        let storage = build_alloca (type_of ce') "loopvar" builder in
        ignore (build_store ce' storage builder);

        (* loop start *)
        let preheader_bb = insertion_block builder in
        let func = block_parent preheader_bb in
        let loop_bb = append_block context "loop" func in
        ignore (build_br loop_bb builder);
        position_at_end loop_bb builder;

        let newenv =
          env
          |> Environment.extend id (build_load storage "loop" builder)
          |> Environment.extend "__loopvar__" storage
          |> Environment.extend "__loop__" (value_of_block loop_bb)
        in

        (* loop body *)
        let e' = codegen_exp newenv e in

        (* loop end *)
        let after_bb = append_block context "afterloop" func in
        ignore (build_br after_bb builder);
        position_at_end after_bb builder;
        e'
    | F.RecurExp v ->
        let v' = codegen_value env v in
        let looplabel = Environment.lookup "__loop__" env in
        let ptr = Environment.lookup "__loopvar__" env in
        let loop_bb = block_of_value looplabel in
        ignore (build_store v' ptr builder);
        ignore (build_br loop_bb builder);
        let preheader_bb = insertion_block builder in
        let func = block_parent preheader_bb in
        let after_bb = append_block context "afterrecur" func in
        position_at_end after_bb builder;
        const_null int_t
  in

  let codegen_decl (F.RecDecl (label, args, exp)) =
    let func = 
      match lookup_function label the_module with
        | Some f -> f
        | None -> failwith "unreachable"
    in
    let bb = append_block context "entry" func in
    position_at_end bb builder;

    let initial_env =
      List.fold_left (fun (env, idx) name ->
        let arg = param func idx in
        set_value_name name arg;
        (Environment.extend name arg env, idx + 1)
      ) (Environment.empty, 0) args
      |> fst
    in
    let rval = codegen_exp initial_env exp in
    let _ = build_ret rval builder in ();
    Llvm_analysis.assert_valid_function func
  in

  List.iter (fun (F.RecDecl (label, args, _)) ->
    let func_t =
      Array.of_list args
      |> Array.map (fun _ -> int_t)
      |> function_type int_t
    in
    let _ = declare_function label func_t the_module in
    ()
  ) prog;
  List.iter codegen_decl prog;
  the_module
