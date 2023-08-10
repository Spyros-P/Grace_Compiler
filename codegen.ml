open Ast
open Llvm


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

let codegen_expr info ast =
  match ast with
  | EInt        i    -> codegen_int       info i
  | EChar       c    -> codegen_char      info c
  | EUnOp       op   -> codegen_uop       info op
  | EBinOp      op   -> codegen_binop     info op
  | ELVal       lval -> codegen_lval      info lval (* not implemented yet*)
  | EFuncCall   fn   -> codegen_func_call info fn   (* not implemented yet*)

let codegen_int  info i = info.c64 i
let codegen_char info c = info.c32 (Char.code c)
let codegen_uop info oper =
  match oper with
  | EUnOp(op, expr, _) -> let e = codegen_expr info expr in
                            begin
                              match op with
                              | UnopPlus  -> e
                              | UnopMinus -> Llvm.build_neg e "negtmp" info.builder
                            end

let codegen_bop info oper =
  match oper with
  | EBinOp(op, expr1, expr2) -> let e1 = codegen_expr info expr1
                                and e2 = codegen_expr info expr2 in
                                  begin
                                    match op with
                                    | BopAdd -> Llvm.build_add e1 e2 "addtmp" info.builder
                                    | BopSub -> Llvm.build_sub e1 e2 "subtmp" info.builder
                                    | BopMul -> Llvm.build_mul e1 e2 "multmp" info.builder
                                    | BopDiv -> Llvm.build_sdiv e1 e2 "divtmp" info.builder
                                    | BopMod -> Llvm.build_srem e1 e2 "modtmp" info.builder
                                  end

let codegen_lval info lval = raise "Not implemented"
let codegen_func_call info fn = raise "Not implemented"

let codegen_stmt info statement =
  match statement with
  | EEmpty                         -> ((*do nothing*))
  | EBlock(stmts)                  -> codegen_block     info stmts
  | ECallFunc(fn, params)          -> codegen_func_call info fn params  (* not implemented yet *)
  | EAss(lval, expr, _)            -> codegen_ass       info lval expr
  | EIf(cond, stmt, _)             -> codegen_if        info cond stmt
  | EIfElse(cond, stmt1, stmt2, _) -> codegen_ifelse    info cond stmt1 stmt2
  | EWhile(cond, stmt, _)          -> codegen_while     info cond stmt
  | ERet                           -> codegen_ret       info
  | ERetVal(expr, _)               -> codegen_retval    info expr

let codegen_block info stmt_list = 
  List.iter (codegen_stmt info) stmt_list

let codegen_func_call info fn params = raise "Not implemented"

let codegen_ass info lval expr = raise "Not implemented"

let rec codegen_cond info cond =
  match cond with
  | ELbop(lbop, cond1, cond2, _)  ->  let c1 = codegen_cond info cond1
                                      and c2 = codegen_cond info cond2 in
                                      match lbop with
                                      | LbopAnd -> Llvm.build_and c1 c2 "andtemp" info.builder
                                      | LbopOr  -> Llvm.build_or  c1 c2 "ortemp"  info.builder
  | ELuop(luop, cond, _)          ->  let c = codegen_cond info cond in
                                      match luop with
                                      | LuopNot -> Llvm.build_neg c "negtemp" info.builder
  | EComp(comp, expr1, expr2, _)  ->  let e1 = codegen_expr info expr1
                                      and e2 = codegen_expr info expr2 in
                                      match comp with
                                      | CompEq   -> Llvm.build_icmp Llvm.Icmp.Eq  e1 e2 "if_cond" info.builder
                                      | CompNeq  -> Llvm.build_icmp Llvm.Icmp.Ne  e1 e2 "if_cond" info.builder
                                      | CompGr   -> Llvm.build_icmp Llvm.Icmp.Sgt e1 e2 "if_cond" info.builder
                                      | CompLs   -> Llvm.build_icmp Llvm.Icmp.Slt e1 e2 "if_cond" info.builder
                                      | CompGrEq -> Llvm.build_icmp Llvm.Icmp.Sge e1 e2 "if_cond" info.builder
                                      | CompLsEq -> Llvm.build_icmp Llvm.Icmp.Sle e1 e2 "if_cond" info.builder

let codegen_if info cond stmt =
  let c       = codegen_cond info cond in
  let bb      = Llvm.insertion_block info.builder in
  let f       = Llvm.block_parent bb in
  let then_bb = Llvm.append_block info.context "then" f in
  let else_bb = Llvm.append_block info.context "else" f in
  ignore (Llvm.build_cond_br c then_bb else_bb info.builder);
  Llvm.position_at_end then_bb info.builder;
  codegen_stmt info stmt;
  ignore (Llvm.build_br after_bb info.builder);
  Llvm.position_at_end else_bb info.builder;

let codegen_ifelse info cond stmt1 stmt2 =
  let c       = codegen_cond info cond in
  let bb      = Llvm.insertion_block info.builder in
  let f       = Llvm.block_parent bb in
  let then_bb = Llvm.append_block info.context "then" f in
  let else_bb = Llvm.append_block info.context "else" f in
  let after_bb = Llvm.append_block info.context "after" f in
  ignore (Llvm.build_cond_br c then_bb else_bb info.builder);
  Llvm.position_at_end then_bb info.builder;
  codegen_stmt info stmt1;
  ignore (Llvm.build_br after_bb info.builder);
  Llvm.position_at_end else_bb info.builder;
  codegen_stmt info stmt2;
  ignore (Llvm.build_br after_bb info.builder);
  Llvm.position_at_end after_bb info.builder;

let codegen_while info cond stmt =
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
  Llvm.position_at_end after_bb info.builder;

let codegen_ret info = Llvm.build_ret_void info.builder
let codegen_retval info expr = Llvm.build_ret (codegen_expr info expr) info.builder

(* define main - compile and dump function *)