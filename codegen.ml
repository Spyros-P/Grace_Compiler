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
  | ELVal       lval -> codegen_lval      info lval 
  | EFuncCall   fn   -> codegen_func_call info fn

let codegen_int info i = info.c64 i
let codegen_char info c = info.c32 (Char.code c)
let codegen_uop info oper =
  match oper with
  | EUnOp(op, expr, _) -> let e = codegen_expr info expr
                          in
                            begin
                              match op with
                              | UnopPlus  -> e
                              | UnopMinus -> build_neg e "negtmp" info.builder
                            end

let codegen_bop info oper =
  match oper with
  | EBinOp(op, expr1, expr2) -> let e1 = codegen_expr info expr1
                                and e2 = codegen_expr info expr2
                                in
                                  begin
                                    match op with
                                    | BopAdd -> build_add e1 e2 "addtmp" info.builder
                                    | BopSub -> build_sub e1 e2 "subtmp" info.builder
                                    | BopMul -> build_mul e1 e2 "multmp" info.builder
                                    | BopDiv -> build_sdiv e1 e2 "divtmp" info.builder
                                    | BopMod -> build_srem e1 e2 "modtmp" info.builder
                                  end

