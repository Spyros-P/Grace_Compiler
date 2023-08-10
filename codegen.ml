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

let rec codegen_expr info ast =
  match ast with
  | EInt(i, _)                    -> codegen_int       info i
  | EChar(c, _)                   -> codegen_char      info c
  | EUnOp(op, expr, _)            -> codegen_uop       info op (codegen_expr info expr)
  | EBinOp(op, expr1, expr2, _)   -> codegen_bop       info op (codegen_expr info expr1) (codegen_expr info expr2)
  | ELVal(lval, _)                -> codegen_lval      info lval (* not implemented yet*)
  | EFuncCall(fn, expr_list, _)   -> codegen_func_call info fn (List.iter (codegen_expr info) expr_list)  (* not implemented yet*)

and codegen_int  info i = info.c64 i
and codegen_char info c = info.c32 (Char.code c)
and codegen_uop info oper expr =
  match oper with
  | UnopPlus -> expr
  | UnopNeg  -> Llvm.build_neg expr "negtmp" info.builder

and codegen_bop info oper expr1 expr2 =
  match oper with
  | BopAdd -> Llvm.build_add expr1 expr2 "addtmp" info.builder
  | BopSub -> Llvm.build_sub expr1 expr2 "subtmp" info.builder
  | BopMul -> Llvm.build_mul expr1 expr2 "multmp" info.builder
  | BopDiv -> Llvm.build_sdiv expr1 expr2 "divtmp" info.builder
  | BopMod -> Llvm.build_srem expr1 expr2 "modtmp" info.builder

let rec codegen_stmt info statement =
  match statement with
  | EEmpty(_)                      -> ((*do nothing*))
  | EBlock(stmts, _)               -> codegen_block     info stmts
  | ECallFunc(fn, params, _)       -> codegen_func_call info fn params  (* not implemented yet *)
  | EAss(lval, expr, _)            -> codegen_ass       info lval expr
  | EIf(cond, stmt, _)             -> codegen_if        info cond stmt
  | EIfElse(cond, stmt1, stmt2, _) -> codegen_ifelse    info cond stmt1 stmt2
  | EWhile(cond, stmt, _)          -> codegen_while     info cond stmt
  | ERet(_)                        -> codegen_ret       info
  | ERetVal(expr, _)               -> codegen_retval    info expr

and codegen_block info block =
  match block with
  | EListStmt(stmt_list, _) -> List.iter (codegen_stmt info) stmt_list

and codegen_cond info cond =
  match cond with
  | ELbop(oper, cond1, cond2, _) -> begin
                                      let c1 = codegen_cond info cond1
                                      and c2 = codegen_cond_info cond2 in
                                      match oper with
                                      | LbopAnd -> Llvm.build_and c1 c2 "andtemp" info.builder
                                      | LbopOr  -> Llvm.build_or  c1 c2 "ortemp"  info.builder
                                    end
  | ELuop(oper, cond, _)         -> begin
                                      let c = codegen_cond info cond in
                                      match op with
                                      | LuopNot -> Llvm.build_neg c "negtemp" info.builder
                                    end
  | EComp(oper, expr1, expr2, _) -> begin
                                      let e1 = codegen_expr info expr1
                                      and e2 = codegen_expr info expr2 in
                                      match op with
                                      | CompEq   -> Llvm.build_icmp Llvm.Icmp.Eq  e1 e2 "if_cond" info.builder
                                      | CompNeq  -> Llvm.build_icmp Llvm.Icmp.Ne  e1 e2 "if_cond" info.builder
                                      | CompGr   -> Llvm.build_icmp Llvm.Icmp.Sgt e1 e2 "if_cond" info.builder
                                      | CompLs   -> Llvm.build_icmp Llvm.Icmp.Slt e1 e2 "if_cond" info.builder
                                      | CompGrEq -> Llvm.build_icmp Llvm.Icmp.Sge e1 e2 "if_cond" info.builder
                                      | CompLsEq -> Llvm.build_icmp Llvm.Icmp.Sle e1 e2 "if_cond" info.builder
                                    end

and codegen_if info cond stmt =
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

and codegen_ifelse info cond stmt1 stmt2 =
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
  Llvm.position_at_end after_bb info.builder;

and codegen_ret info = Llvm.build_ret_void info.builder
and codegen_retval info expr = Llvm.build_ret (codegen_expr info expr) info.builder

(* define main - compile and dump function *)