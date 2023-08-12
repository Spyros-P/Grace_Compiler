open Ast
open Llvm
open Llvm_analysis
open Llvm_scalar_opts
open Llvm_all_backends

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

(* the following functions are helpers for handling expressions *)

let codegen_int info i = info.c32 i
let codegen_char info c = info.c64 (Char.code c)

let codegen_uop info oper expr =
  match oper with
  | UnopPlus -> expr
  | UnopMinus  -> Llvm.build_neg expr "negtmp" info.builder

let codegen_bop info oper expr1 expr2 =
  match oper with
  | BopAdd -> Llvm.build_add  expr1 expr2 "addtmp" info.builder
  | BopSub -> Llvm.build_sub  expr1 expr2 "subtmp" info.builder
  | BopMul -> Llvm.build_mul  expr1 expr2 "multmp" info.builder
  | BopDiv -> Llvm.build_sdiv expr1 expr2 "divtmp" info.builder
  | BopMod -> Llvm.build_srem expr1 expr2 "modtmp" info.builder

let codegen_lval: llvm_info -> lvalue -> llvalue = fun info lval -> assert false

let codegen_func_call: llvm_info -> string -> llvalue list -> llvalue = fun info fn expr_list -> assert false

let rec codegen_expr info ast =
  match ast with
  | EInt(i, _)                    -> codegen_int       info i
  | EChar(c, _)                   -> codegen_char      info c
  | ELVal(lval, _)                -> codegen_lval      info lval (* not implemented yet*)
  | EFuncCall(fn, expr_list, _)   -> codegen_func_call info fn (List.map (codegen_expr info) expr_list)  (* not implemented yet*)
  | EUnOp(op, expr, _)            -> begin
                                      let e = codegen_expr info expr in
                                      codegen_uop info op e
                                     end
  | EBinOp(op, expr1, expr2, _)   -> begin 
                                      let e1 = codegen_expr info expr1 
                                      and e2 = codegen_expr info expr2 in 
                                      codegen_bop info op e1 e2 
                                     end


(* the following functions are helpers for handling statements *)
let codegen_ret info = ignore (Llvm.build_ret_void info.builder)
let codegen_retval info expr = ignore (Llvm.build_ret (codegen_expr info expr) info.builder)

let rec codegen_cond info cond =
  match cond with
  | ELbop(oper, cond1, cond2, _) -> begin
                                      let c1 = codegen_cond info cond1
                                      and c2 = codegen_cond info cond2 in
                                      match oper with
                                      | LbopAnd -> Llvm.build_and c1 c2 "andtemp" info.builder
                                      | LbopOr  -> Llvm.build_or  c1 c2 "ortemp"  info.builder
                                    end
  | ELuop(oper, cond, _)         -> begin
                                      let c = codegen_cond info cond in
                                      match oper with
                                      | LuopNot -> Llvm.build_neg c "negtemp" info.builder
                                    end
  | EComp(oper, expr1, expr2, _) -> begin
                                      let e1 = codegen_expr info expr1
                                      and e2 = codegen_expr info expr2 in
                                      match oper with
                                      | CompEq   -> Llvm.build_icmp Llvm.Icmp.Eq  e1 e2 "if_cond" info.builder
                                      | CompNeq  -> Llvm.build_icmp Llvm.Icmp.Ne  e1 e2 "if_cond" info.builder
                                      | CompGr   -> Llvm.build_icmp Llvm.Icmp.Sgt e1 e2 "if_cond" info.builder
                                      | CompLs   -> Llvm.build_icmp Llvm.Icmp.Slt e1 e2 "if_cond" info.builder
                                      | CompGrEq -> Llvm.build_icmp Llvm.Icmp.Sge e1 e2 "if_cond" info.builder
                                      | CompLsEq -> Llvm.build_icmp Llvm.Icmp.Sle e1 e2 "if_cond" info.builder
                                    end

let codegen_ass: llvm_info -> lvalue -> llvalue -> unit = fun info lval expr -> assert false

let codegen_call_func: llvm_info -> string -> llvalue list -> unit = fun info fn params -> assert false

let rec codegen_stmt info statement =
  match statement with
  | EEmpty(_)                      -> ((*do nothing*))
  | EBlock(stmts, _)               -> codegen_block     info stmts
  | ECallFunc(fn, params, _)       -> codegen_call_func info fn (List.map (codegen_expr info) params)  (* not implemented yet *)
  | EAss(lval, expr, _)            -> codegen_ass       info lval (codegen_expr info expr)  (* not implemented yet *)
  | EIf(cond, stmt, _)             -> codegen_if        info cond stmt
  | EIfElse(cond, stmt1, stmt2, _) -> codegen_ifelse    info cond stmt1 stmt2
  | EWhile(cond, stmt, _)          -> codegen_while     info cond stmt
  | ERet(_)                        -> codegen_ret       info
  | ERetVal(expr, _)               -> codegen_retval    info expr

and codegen_block info block =
  match block with
  | EListStmt(stmt_list, _) -> List.iter (codegen_stmt info) stmt_list

and codegen_if info cond stmt =
  let c       = codegen_cond info cond in
  let bb      = Llvm.insertion_block info.builder in
  let f       = Llvm.block_parent bb in
  let then_bb = Llvm.append_block info.context "then" f in
  let after_bb = Llvm.append_block info.context "after" f in
  ignore (Llvm.build_cond_br c then_bb after_bb info.builder);
  Llvm.position_at_end then_bb info.builder;
  codegen_stmt info stmt;
  ignore (Llvm.build_br after_bb info.builder);
  Llvm.position_at_end after_bb info.builder;

and codegen_ifelse info cond stmt1 stmt2 =
  let c        = codegen_cond info cond in
  let bb       = Llvm.insertion_block info.builder in
  let f        = Llvm.block_parent bb in
  let then_bb  = Llvm.append_block info.context "then" f in
  let else_bb  = Llvm.append_block info.context "else" f in
  let after_bb = Llvm.append_block info.context "after" f in
  ignore (Llvm.build_cond_br c then_bb else_bb info.builder);
  Llvm.position_at_end then_bb info.builder;
  codegen_stmt info stmt1;
  ignore (Llvm.build_br after_bb info.builder);
  Llvm.position_at_end else_bb info.builder;
  codegen_stmt info stmt2;
  ignore (Llvm.build_br after_bb info.builder);
  Llvm.position_at_end after_bb info.builder;

and codegen_while info cond stmt =
  let bb      = Llvm.insertion_block info.builder in
  let f       = Llvm.block_parent bb in
  let cond_bb = Llvm.append_block info.context "cond" f in
  let body_bb = Llvm.append_block info.context "body" f in
  let after_bb = Llvm.append_block info.context "after" f in
  ignore (Llvm.build_br cond_bb info.builder);
  Llvm.position_at_end cond_bb info.builder;
  let c = codegen_cond info cond in
  ignore (Llvm.build_cond_br c body_bb after_bb info.builder);
  Llvm.position_at_end body_bb info.builder;
  codegen_stmt info stmt;
  ignore (Llvm.build_br cond_bb info.builder);
  Llvm.position_at_end after_bb info.builder
(* define main - compile and dump function *)


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
  List.iter (codegen_block info) asts;
  ignore (Llvm.build_ret (c32 0) builder);
  (* Verify *)
  Llvm_analysis.assert_valid_module the_module;
  (* Optimize *)
  ignore (Llvm.PassManager.run_module the_module pm);
  (* Print out the IR *)
  Llvm.print_module "a.ll" the_module