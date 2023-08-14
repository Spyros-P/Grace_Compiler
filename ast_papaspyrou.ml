open Llvm

type var  = char
type oper = O_plus | O_minus | O_times

type ast_stmt =
| S_print of ast_expr
| S_let of var * ast_expr
| S_for of ast_expr * ast_stmt
| S_block of ast_stmt list
| S_if of ast_expr * ast_stmt

and ast_expr =
| E_const of int
| E_var of var
| E_op of ast_expr * oper * ast_expr

let vars = Array.make 26 0

let rec run_expr ast =
  match ast with
  | E_const n         -> n
  | E_var x           -> vars.(int_of_char x - int_of_char 'a')
  | E_op (e1, op, e2) -> let v1 = run_expr e1
                         and v2 = run_expr e2 in
                         match op with
                         | O_plus  -> v1 + v2
                         | O_minus -> v1 - v2
                         | O_times -> v1 * v2

let rec run_stmt ast =
  match ast with
  | S_print e    -> let v = run_expr e in
                    Printf.printf "%d\n" v
  | S_let (x, e) -> let v = run_expr e in
                    vars.(int_of_char x - int_of_char 'a') <- v
  | S_for (e, s) -> let v = run_expr e in
                    for i = 1 to v do
                      run_stmt s
                    done
  | S_block b    -> run b
  | S_if (e, s)  -> let v = run_expr e in
                    if v <> 0 then run_stmt s

and run asts = List.iter run_stmt asts

type llvm_info = {
  context          : Llvm.llcontext;
  the_module       : Llvm.llmodule;
  builder          : Llvm.llbuilder;
  i8               : Llvm.lltype;
  i32              : Llvm.lltype;
  i64              : Llvm.lltype;
  c32              : int -> Llvm.llvalue;
  c64              : int -> Llvm.llvalue;
  the_vars         : Llvm.llvalue;
  the_nl           : Llvm.llvalue;
  the_writeInteger : Llvm.llvalue;
  the_writeString  : Llvm.llvalue;
}

let rec compile_expr info ast =
  match ast with
  | E_const n         -> info.c64 n
  | E_var x           -> let n = int_of_char x - int_of_char 'a' in
                         let v = Llvm.build_gep info.the_vars
                                                [| info.c32 0; info.c32 n |]
                                                (String.make 1 x ^ "_ptr")
                                                info.builder in
                         Llvm.build_load v (String.make 1 x) info.builder
  | E_op (e1, op, e2) -> let l = compile_expr info e1 in
                         let r = compile_expr info e2 in
                         begin
                           match op with
                           | O_plus  -> Llvm.build_add l r "addtmp" info.builder
                           | O_minus -> Llvm.build_sub l r "subtmp" info.builder
                           | O_times -> Llvm.build_mul l r "multmp" info.builder
                         end

let rec compile_stmt info ast =
  match ast with
  | S_print e    -> let n = compile_expr info e in
                    ignore (Llvm.build_call info.the_writeInteger [| n |] ""
                                            info.builder);
                    let nl = Llvm.build_gep info.the_nl
                                            [| info.c32 0; info.c32 0 |]
                                            "nl" info.builder in
                    ignore (Llvm.build_call info.the_writeString [| nl |] ""
                                            info.builder)
  | S_let (x, e) -> let n = int_of_char x - int_of_char 'a' in
                    let l = Llvm.build_gep info.the_vars
                                           [| info.c32 0; info.c32 n |]
                                           (String.make 1 x ^ "_ptr")
                                           info.builder in
                    let r = compile_expr info e in
                    ignore (Llvm.build_store r l info.builder)
  | S_for (e, s) -> let n = compile_expr info e in
                    let bb = Llvm.insertion_block info.builder in
                    let f = Llvm.block_parent bb in
                    let loop_bb = Llvm.append_block info.context "loop" f in
                    let body_bb = Llvm.append_block info.context "body" f in
                    let after_bb = Llvm.append_block info.context "after" f in
                    ignore (Llvm.build_br loop_bb info.builder);
                    Llvm.position_at_end loop_bb info.builder;
                    let phi_iter =
                      Llvm.build_phi [(n, bb)] "iter" info.builder in
                    let loop_cond =
                      Llvm.build_icmp Llvm.Icmp.Sgt phi_iter (info.c64 0)
                                      "loop_cond" info.builder in
                    ignore (Llvm.build_cond_br loop_cond body_bb after_bb
                                               info.builder);
                    Llvm.position_at_end body_bb info.builder;
                    let remaining = Llvm.build_sub phi_iter (info.c64 1)
                                                   "remaining" info.builder in
                    compile_stmt info s;
                    Llvm.add_incoming
                      (remaining, Llvm.insertion_block info.builder) phi_iter;
                    ignore (Llvm.build_br loop_bb info.builder);
                    Llvm.position_at_end after_bb info.builder
  | S_block b    -> List.iter (compile_stmt info) b
  | S_if (e, s)  -> let v = compile_expr info e in
                    let cond = Llvm.build_icmp Llvm.Icmp.Ne v (info.c64 0)
                                               "if_cond" info.builder in
                    let bb = Llvm.insertion_block info.builder in
                    let f = Llvm.block_parent bb in
                    let then_bb = Llvm.append_block info.context "then" f in
                    let after_bb = Llvm.append_block info.context "after" f in
                    ignore (Llvm.build_cond_br cond then_bb after_bb
                                               info.builder);
                    Llvm.position_at_end then_bb info.builder;
                    compile_stmt info s;
                    ignore (Llvm.build_br after_bb info.builder);
                    Llvm.position_at_end after_bb info.builder

let llvm_compile_and_dump asts =
  (* Initialize *)
  Llvm_all_backends.initialize ();
  let context = Llvm.global_context () in
  let the_module = Llvm.create_module context "minibasic program" in
  let builder = Llvm.builder context in
  let pm = Llvm.PassManager.create () in
  List.iter (fun f -> f pm) [
    Llvm_scalar_opts.add_memory_to_register_promotion;
    Llvm_scalar_opts.add_instruction_combination;
    Llvm_scalar_opts.add_reassociation;
    Llvm_scalar_opts.add_gvn;
    Llvm_scalar_opts.add_cfg_simplification;
  ];
  (* Initialize types *)
  let i8 = Llvm.i8_type context in
  let i32 = Llvm.i32_type context in
  let i64 = Llvm.i64_type context in
  (* Initialize constant functions *)
  let c32 = Llvm.const_int i32 in
  let c64 = Llvm.const_int i64 in
  (* Initialize global variables *)
  let vars_type = Llvm.array_type i64 26 in
  let the_vars = Llvm.declare_global vars_type "vars" the_module in
  Llvm.set_linkage Llvm.Linkage.Private the_vars;
  Llvm.set_initializer (Llvm.const_null vars_type) the_vars;
  Llvm.set_alignment 16 the_vars;
  let nl = "\n" in
  let nl_type = Llvm.array_type i8 (1 + String.length nl) in
  let the_nl = Llvm.declare_global nl_type "nl" the_module in
  Llvm.set_linkage Llvm.Linkage.Private the_nl;
  Llvm.set_global_constant true the_nl;
  Llvm.set_initializer (Llvm.const_stringz context nl) the_nl;
  Llvm.set_alignment 1 the_nl;
  (* Initialize library functions *)
  let writeInteger_type =
    Llvm.function_type (Llvm.void_type context) [| i64 |] in
  let the_writeInteger =
    Llvm.declare_function "writeInteger" writeInteger_type the_module in
  let writeString_type =
    Llvm.function_type (Llvm.void_type context) [| Llvm.pointer_type i8 |] in
  let the_writeString =
    Llvm.declare_function "writeString" writeString_type the_module in
  (* Define and start and main function *)
  let main_type = Llvm.function_type i32 [| |] in
  let main = Llvm.declare_function "main" main_type the_module in
  let bb = Llvm.append_block context "entry" main in
  Llvm.position_at_end bb builder;
  (* Emit the program code *)
  let info = {
    context          = context;
    the_module       = the_module;
    builder          = builder;
    i8               = i8;
    i32              = i32;
    i64              = i64;
    c32              = c32;
    c64              = c64;
    the_vars         = the_vars;
    the_nl           = the_nl;
    the_writeInteger = the_writeInteger;
    the_writeString  = the_writeString;
  } in
  List.iter (compile_stmt info) asts;
  ignore (Llvm.build_ret (c32 0) builder);
  (* Verify *)
  Llvm_analysis.assert_valid_module the_module;
  (* Optimize *)
  ignore (Llvm.PassManager.run_module the_module pm);
  (* Print out the IR *)
  Llvm.print_module "a.ll" the_module
