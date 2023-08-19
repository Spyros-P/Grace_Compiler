open Ast
open Llvm
open Llvm_ipo
open Llvm_vectorize
open Llvm_analysis
open Llvm_scalar_opts
open Llvm_all_backends
(*open Llvm.PassManager*)



type entry =
  | Efun of func_decl * Llvm.llvalue
  | Evar of var * (Llvm.llvalue -> Llvm.llvalue)

type llvm_info = {
  context          : Llvm.llcontext;
  the_module       : Llvm.llmodule;
  builder          : Llvm.llbuilder;
  i8               : Llvm.lltype;
  i32              : Llvm.lltype;
  i64              : Llvm.lltype;
  c32              : int -> Llvm.llvalue;
  c64              : int -> Llvm.llvalue;
  i8_ptr_trash     : llvalue;
  the_nl           : Llvm.llvalue;
  funcs            : Llvm.llvalue list ref;
  build_in_table   : (string, entry) Hashtbl.t;
}




let symbol_table : (string, entry) Hashtbl.t list ref = ref []

let fun_refs    : (Llvm.llvalue, bool list) Hashtbl.t =  Hashtbl.create 10

let fun_decls : func_decl list ref = ref []

let struct_types : lltype option list ref = ref []
let activation_records : llvalue option list ref = ref []


(* ------------------------------------------------- *)


let open_scope () =
  symbol_table :=  (Hashtbl.create 10) :: !symbol_table

let close_scope () =
  symbol_table := List.tl !symbol_table

let current_scope () =
  List.hd !symbol_table

(*
    Lookup function returns a tuple of (llvalue->llvalue , int) option 
    The integer represent the depth of the scope that the variable/function was declared
    Specific integer values: -1 (means global scope)
*)
let lookup info id =
  let rec walk id st n =
    match st with
    | []          ->  None
    | cs::[]      ->  (try
                        Some (Hashtbl.find cs id, -1)
                      with Not_found -> None)
    | cs::scopes  ->  (try
                        Some (Hashtbl.find cs id, n)
                      with Not_found -> walk id scopes (n+1))
  in 
    try Some(Hashtbl.find info.build_in_table id, -1)
    with Not_found -> walk id !symbol_table 0


(* REMEMBER: check that ids dont confict with fix fun ids eg print *)
let insert id llval =
  if Hashtbl.mem (current_scope ()) id then
    failwith "insert"
  else
    Hashtbl.add (current_scope ()) id llval

let remove_head id =
  Hashtbl.remove (current_scope ()) id



let rec get_struct_param (f:func_decl) lst =
  match !(!(f.depend)), lst with
  | Some(1,_), Some(hd)::tl ->  hd
  | _ , hd::tl              ->  get_struct_param (match !(!(f.father_func)) with Some(x) -> x | _ -> failwith "get_struct_param") tl
  | _ , _                   ->  failwith "get_struct_param"


(* ------------------------------------------------- *)



(* the following functions are helpers for handling expressions *)

let codegen_int info i = info.c32 i
let codegen_char info c = Llvm.const_int (Llvm.i8_type info.context) (Char.code c)

let codegen_uop info oper expr =
  match oper with
  | UnopPlus  ->  expr
  | UnopMinus ->  Llvm.build_neg expr "negtmp" info.builder

let codegen_bop info oper expr1 expr2 =
  match oper with
  | BopAdd  ->  Llvm.build_add  expr1 expr2 "addtmp" info.builder
  | BopSub  ->  Llvm.build_sub  expr1 expr2 "subtmp" info.builder
  | BopMul  ->  Llvm.build_mul  expr1 expr2 "multmp" info.builder
  | BopDiv  ->  Llvm.build_sdiv expr1 expr2 "divtmp" info.builder
  | BopMod  ->  Llvm.build_srem expr1 expr2 "modtmp" info.builder


let rec codegen_type info atype =
  match atype with
  | EInteger(lst)   ->  (match lst with
                        | []      ->  info.i32
                        | -1::tl  ->  (codegen_type info (EInteger(tl)))
                        | hd::tl  ->  Llvm.array_type (codegen_type info (EInteger(tl))) hd)
  | ECharacter(lst) ->  (match lst with
                        | []      ->  Llvm.i8_type info.context
                        | -1::tl  ->  (codegen_type info (ECharacter(tl)))
                        | hd::tl  ->  Llvm.array_type (codegen_type info (ECharacter(tl))) hd)
  | EString         ->  failwith "codegen_type"
  | ENothing        ->  Llvm.void_type info.context

let id_get_llvalue info id =
  let rec get_ac_record ac_record i =
    let rec walk ac_record i =
      let llval = ref (Llvm.build_struct_gep ac_record 0 "prev_ac_record" info.builder) in
        for i = i downto 2 do
          llval := Llvm.build_load !llval "lval_tmp" info.builder;
          llval := Llvm.build_struct_gep !llval 0 "prev_ac_record" info.builder
        done; Llvm.build_load !llval "lval_tmp" info.builder
    in (if (i > 0) then walk ac_record i else ac_record)
  in match lookup info id with
  | Some(Efun(_,llval),i) ->  llval
  | Some(Evar(_,llval),i) ->  llval info.the_nl(*(get_ac_record (List.hd !activation_records) i)*)
  | _             ->  failwith "id_get_llvalue"


let fun_get_struct_ptr info id =
  let rec walk (decl:func_decl) ac_link depth =
    let father_decl = match !(!(decl.father_func)) with Some(x) -> x | _ -> failwith "fun_get_struct_ptr"
    in match !(!(decl.depend)), depth with
    | _ , 1         ->  ac_link
    | Some(1,_) , _ ->  walk father_decl (let prev_ac_rec = Llvm.build_load ac_link "prev_ac_rec" info.builder
                                          in Llvm.build_struct_gep prev_ac_rec 0 "prev_acc_link" info.builder) (depth-1)
    | Some(_,_) , _ ->  walk father_decl ac_link (depth-1)
    | _ , _ -> failwith "fun_get_struct_ptr"
  in let func_decl = List.hd !fun_decls
  in let parent_acc_link = Llvm.param (List.hd !(info.funcs)) 0
  in match lookup info id with
  | Some(_,-1)            ->  (fun x -> x)
  | Some(Efun(decl,_),0)  ->  (match !(!(decl.depend)) with
                              | None      ->  (fun x -> x)
                              | Some(1,_) ->  let curr_ac_rec = (match List.hd !activation_records with Some(x) -> x | _ -> failwith "fun_get_struct_ptr")
                                              in (fun x -> curr_ac_rec::x)
                              | Some(d,_) ->  (fun x -> (walk func_decl parent_acc_link (d-1))::x))
  | Some(Efun(decl,_),i)  ->  (match !(!(decl.depend)) with
                              | None      ->  (fun x -> x)
                              | Some(d,_) ->  (fun x -> (walk func_decl parent_acc_link (d+i-1))::x))
  | _              ->  failwith "fun_get_struct_ptr"

let rec codegen_lval info lval =
  match lval with
  | EAssId(id,_)            ->  id_get_llvalue info id
  | EAssString(str,_)       ->  let str_type = Llvm.array_type info.i8 (1 + String.length str) in
                                let the_str = Llvm.declare_global str_type str info.the_module in
                                Llvm.set_linkage Llvm.Linkage.Private the_str;
                                Llvm.set_global_constant true the_str;
                                Llvm.set_initializer (Llvm.const_stringz info.context str) the_str;
                                Llvm.set_alignment 1 the_str;
                                the_str
  | EAssArrEl(lval,expr,_)  ->  Llvm.build_gep (codegen_lval info lval)
                                [| info.c32 0; (codegen_expr info expr) |]
                                "pointer"
                                info.builder

and codegen_lval_load info lval =
  match lval with
  | EAssId(id,_)            ->  let llval = id_get_llvalue info id in
                                Llvm.build_load llval "lval_tmp" info.builder
  | EAssString(str,_)       ->  (codegen_lval info lval)
  | EAssArrEl(lval,expr,_)  ->  let llval = Llvm.build_gep (codegen_lval info lval)
                                [| info.c32 0; (codegen_expr info expr) |]
                                "pointer"
                                info.builder in
                                Llvm.build_load llval "lval_tmp" info.builder

and codegen_expr info ast =
  match ast with
  | EInt(i, _)                    ->  codegen_int       info i
  | EChar(c, _)                   ->  codegen_char      info c
  | ELVal(lval, _)                ->  codegen_lval_load info lval
  | EFuncCall(id, params, _)      ->  codegen_call_func info id params
  | EUnOp(op, expr, _)            ->  begin
                                        let e = codegen_expr info expr in
                                        codegen_uop info op e
                                      end
  | EBinOp(op, expr1, expr2, _)   ->  begin 
                                        let e1 = codegen_expr info expr1 
                                        and e2 = codegen_expr info expr2 in 
                                        codegen_bop info op e1 e2 
                                      end


and codegen_call_func info id params =
  let rec walk params ref_lst =
    (let get_llval param ref =
      if (ref == false) then (codegen_expr info param)
      else (match param with
            | ELVal(lval, _)  ->  let llval = codegen_lval info lval in
                                  (match lval with EAssId(_,_) -> llval
                                  | _ -> Llvm.build_gep llval [| info.c32 0; info.c32 0 |] "" info.builder)
            | _               ->  failwith "codegen_call_func") in
    match params, ref_lst with
    | [], []                              ->  []
    | param::rest_params, ref::rest_refs  ->  (get_llval param ref) :: (walk rest_params rest_refs)
    | _, _                                -> failwith "codegen_call_func") in
  let func = id_get_llvalue info id in
  let fix = fun_get_struct_ptr info id in
  let ref_lst = (try Hashtbl.find fun_refs func
                with Not_found ->  failwith "codegen_call_func") in
  Llvm.build_call func (Array.of_list (fix (walk params ref_lst))) "" info.builder

(* the following functions are helpers for handling statements *)
let codegen_ret info =
  ignore (Llvm.build_ret_void info.builder);
  let ret_bb = append_block info.context "after_ret" (List.hd !(info.funcs)) in
  position_at_end ret_bb info.builder

let codegen_retval info expr =
  ignore (Llvm.build_ret (codegen_expr info expr) info.builder);
  let ret_bb = append_block info.context "after_ret" (List.hd !(info.funcs)) in
  position_at_end ret_bb info.builder

let rec codegen_cond info cond =
  match cond with
  | ELbop(oper, cond1, cond2, _)  ->  begin
                                        let c1 = codegen_cond info cond1
                                        and c2 = codegen_cond info cond2 in
                                        match oper with
                                        | LbopAnd ->  Llvm.build_and c1 c2 "andtemp" info.builder
                                        | LbopOr  ->  Llvm.build_or  c1 c2 "ortemp"  info.builder
                                      end
  | ELuop(oper, cond, _)          ->  begin
                                        let c = codegen_cond info cond in
                                        match oper with
                                        | LuopNot ->  Llvm.build_neg c "negtemp" info.builder
                                      end
  | EComp(oper, expr1, expr2, _)  ->  begin
                                        let e1 = codegen_expr info expr1
                                        and e2 = codegen_expr info expr2 in
                                        match oper with
                                        | CompEq    ->  Llvm.build_icmp Llvm.Icmp.Eq  e1 e2 "if_cond" info.builder
                                        | CompNeq   ->  Llvm.build_icmp Llvm.Icmp.Ne  e1 e2 "if_cond" info.builder
                                        | CompGr    ->  Llvm.build_icmp Llvm.Icmp.Sgt e1 e2 "if_cond" info.builder
                                        | CompLs    ->  Llvm.build_icmp Llvm.Icmp.Slt e1 e2 "if_cond" info.builder
                                        | CompGrEq  ->  Llvm.build_icmp Llvm.Icmp.Sge e1 e2 "if_cond" info.builder
                                        | CompLsEq  ->  Llvm.build_icmp Llvm.Icmp.Sle e1 e2 "if_cond" info.builder
                                      end


let codegen_ass info lval expr =
  let llval = codegen_lval info lval in
  ignore (Llvm.build_store expr llval info.builder)

let rec codegen_stmt info statement =
  match statement with
  | EEmpty(_)                       ->  ((*do nothing*))
  | EBlock(block, _)                ->  codegen_block     info block
  | ECallFunc(id, params, _)        ->  ignore(codegen_call_func info id params)
  | EAss(lval, expr, _)             ->  codegen_ass       info lval (codegen_expr info expr)
  | EIf(cond, stmt, _)              ->  codegen_if        info cond stmt
  | EIfElse(cond, stmt1, stmt2, _)  ->  codegen_ifelse    info cond stmt1 stmt2
  | EWhile(cond, stmt, _)           ->  codegen_while     info cond stmt
  | ERet(_)                         ->  codegen_ret       info
  | ERetVal(expr, _)                ->  codegen_retval    info expr

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
  Llvm.position_at_end after_bb info.builder

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
  Llvm.position_at_end after_bb info.builder

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

let rec main_codegen_stmt info statement =
  match statement with
  | EBlock(block, _)                ->  main_codegen_block  info block
  | ERet(_)                         ->  codegen_retval      info (EInt(0, {line_start=0;line_end=0;char_start=0;char_end=0}))
  | _                               ->  codegen_stmt        info statement

and main_codegen_block info block =
  match block with
  | EListStmt(stmt_list, _) -> List.iter (main_codegen_stmt info) stmt_list

(* define main - compile and dump function *)


let codegen_param_type info param =
  if param.ref then Llvm.pointer_type (codegen_type info param.atype)
  else (codegen_type info param.atype)

let codegen_fun_array_args info args (f_decl:func_decl) =
  let params = List.map (codegen_param_type info) args
  in let final_params = if !(!(f_decl.depend)) <> None then (Llvm.pointer_type (get_struct_param f_decl !struct_types)::params) else params
  in Array.of_list final_params


let codegen_activation_record info func ffunc =
  let rec get_ac_rec_vars local_defs =
    match local_defs with
    | []              ->  []
    | EVarDef(x)::tl  ->  if !(x.to_ac_rec) then x::(get_ac_rec_vars tl)
                          else let llval = Llvm.build_alloca (codegen_type info x.atype) x.id info.builder
                          in let get_var =  (fun _ -> llval)
                          in insert x.id (Evar(x,get_var)); get_ac_rec_vars tl
    | hd::tl          ->  get_ac_rec_vars tl
  in let rec get_ac_rec_params local_defs n =
    match local_defs with
    | []          ->  []
    | hd::tl  ->  if !(hd.to_ac_rec) then hd::(get_ac_rec_params tl (n+1))
                  else let llval = Llvm.build_alloca (codegen_type info hd.atype) hd.id info.builder
                  in let get_param =  (fun _ -> llval) in Llvm.build_store (Llvm.param ffunc n) llval info.builder;
                  insert hd.id (Evar({id=hd.id;atype=hd.atype;to_ac_rec=hd.to_ac_rec;pos=hd.pos},get_param)); get_ac_rec_params tl (n+1)
  in let rec insert_activation_record info ac_record (params:func_args list) (vars:var list) n =
    match params, vars with
    | [], []      ->  ()
    | [], hd::tl  ->  let get_var = (fun llval -> Llvm.build_struct_gep llval n "ac_record_arg" info.builder)
                      in insert hd.id (Evar(hd,get_var)); insert_activation_record info ac_record params tl (n+1)
    | hd::tl, _   ->  let get_param = (fun llval -> Llvm.build_struct_gep llval n "ac_record_var" info.builder)
                      in ignore (Llvm.build_store (Llvm.param ffunc n) (get_param ac_record) info.builder);
                      insert hd.id (Evar({id=hd.id;atype=hd.atype;to_ac_rec=hd.to_ac_rec;pos=hd.pos},get_param)); insert_activation_record info ac_record tl vars (n+1)
  in let vars = get_ac_rec_vars func.local_defs
  in let params = get_ac_rec_params func.args (if !(func.depend) = None then 0 else 1)
  in let vars_types = List.map (fun (x:var) -> codegen_type info x.atype) vars
  in let params_types = List.map (codegen_param_type info) params
  in let struct_contents = if !(func.gen_acc_link) then (Llvm.pointer_type (get_struct_param (fun_def2decl func) !struct_types))::(List.append params_types vars_types) else List.append params_types vars_types
  in if (List.length struct_contents <> 0) then (
        let struct_type = Llvm.struct_type info.context (Array.of_list struct_contents)
        in let activation_record = Llvm.build_alloca struct_type "activation_record" info.builder
        in if !(func.gen_acc_link)  then (let struct_first_element = Llvm.build_struct_gep activation_record 0 "prev_ac_record" info.builder;
                                          in ignore (Llvm.build_store (Llvm.param ffunc 0) struct_first_element info.builder));
        insert_activation_record info activation_record params vars 1;
        struct_types :=  Some(struct_type)::(!struct_types);
        activation_records :=  Some(activation_record)::(!activation_records)
  ) else (struct_types :=  None::(!struct_types); activation_records :=  None::(!activation_records))


let rec codegen_localdef info def =
  match def with
  | EFuncDef(func)        ->  let ffunc_type = Llvm.function_type (codegen_type info func.ret) (codegen_fun_array_args info func.args (fun_def2decl func)) in (* fix array *)
                              let ffunc = Llvm.declare_function func.id ffunc_type info.the_module in
                              let bb = Llvm.append_block info.context "entry" ffunc in
                              insert func.id (Efun(fun_def2decl func,ffunc));
                              Hashtbl.add fun_refs ffunc (List.map (fun x -> x.ref) func.args);
                              fun_decls := (fun_def2decl func)::!fun_decls;
                              open_scope ();
                              Llvm.position_at_end bb info.builder;
                              codegen_activation_record info func ffunc;
                              info.funcs := ffunc::!(info.funcs);
                              List.iter (codegen_localdef info) func.local_defs;
                              Llvm.position_at_end bb info.builder;
                              codegen_block info func.body;
                              (match func.ret with
                              | ENothing  ->  ignore (Llvm.build_ret_void info.builder)
                              | _         ->  ignore (Llvm.build_ret (info.c32 0) info.builder));
                              info.funcs := List.tl !(info.funcs);
                              struct_types := List.tl !struct_types;
                              activation_records := List.tl !activation_records;
                              fun_decls := List.tl !fun_decls;
                              close_scope ()
  | EFuncDecl(func_decl)  ->  failwith "codegen_localdef"
  | EVarDef(var)          ->  ()

let rec main_codegen_localdef info def =
match def with
| EFuncDef(func)        ->  codegen_localdef info def
| EFuncDecl(func_decl)  ->  codegen_localdef info def
| EVarDef(var)          ->  let ltype = codegen_type info var.atype in
                            let llval = Llvm.declare_global ltype var.id info.the_module in
                            Llvm.set_linkage Llvm.Linkage.Private llval;
                            Llvm.set_initializer (Llvm.const_null ltype) llval;
                            insert var.id (Evar(var,(fun _ -> llval)))

let codegen_build_in_decl info (decl:func_decl) =
  let ffunc_type = Llvm.function_type (codegen_type info decl.ret) (codegen_fun_array_args info decl.args decl) in (* fix array *)
  let ffunc = Llvm.declare_function decl.id ffunc_type info.the_module in
  Hashtbl.add info.build_in_table decl.id (Efun(decl,ffunc));
  Hashtbl.add fun_refs ffunc (List.map (fun arg -> arg.ref) decl.args)

let llvm_compile_and_dump main_func =
  (* Initialize *)
  Llvm_all_backends.initialize ();
  let context = Llvm.global_context () in
  let the_module = Llvm.create_module context "grace program" in
  let builder = Llvm.builder context in
  let pm = Llvm.PassManager.create () in
  let optimizations = [
    add_ipsccp; add_memory_to_register_promotion; add_dead_arg_elimination;
    add_instruction_combination; add_cfg_simplification;
    add_function_inlining; add_function_attrs; add_scalar_repl_aggregation;
    add_early_cse; add_cfg_simplification; add_instruction_combination;
    add_tail_call_elimination; add_reassociation; add_loop_rotation;
    add_loop_unswitch; add_instruction_combination; add_cfg_simplification;
    add_ind_var_simplification; add_loop_idiom; add_loop_deletion;
    add_loop_unroll; add_gvn; add_memcpy_opt; add_sccp; add_licm;
    add_global_optimizer; add_global_dce;
    add_aggressive_dce; add_cfg_simplification; add_instruction_combination;
    add_dead_store_elimination; add_loop_vectorize; add_slp_vectorize;
    add_strip_dead_prototypes; add_global_dce; add_cfg_simplification;
  ] in
  if true then List.iter (fun f -> f pm) optimizations;
  (* Initialize types *)
  let i8 = Llvm.i8_type context in
  let i32 = Llvm.i32_type context in
  let i64 = Llvm.i64_type context in
  (* Initialize constant functions *)
  let c32 = Llvm.const_int i32 in
  let c64 = Llvm.const_int i64 in
  (* Initialize global variables *)
  let nl = "\n" in
  let nl_type = Llvm.array_type i8 (1 + String.length nl) in
  let the_nl = Llvm.declare_global nl_type "nl" the_module in
  Llvm.set_linkage Llvm.Linkage.Private the_nl;
  Llvm.set_global_constant true the_nl;
  Llvm.set_initializer (Llvm.const_stringz context nl) the_nl;
  Llvm.set_alignment 1 the_nl;
  (* Create symbol table for build in functions *)
  let build_in_table = Hashtbl.create 10 in
  open_scope ();
  (* Define and start and main function *)
  let main_type = Llvm.function_type i32 [| |] in
  let main = Llvm.declare_function "main" main_type the_module in
  let bb = Llvm.append_block context "entry" main in
  Llvm.position_at_end bb builder;
  let i8_ptr_trash = Llvm.build_alloca i8 "$_i8_ptr_trash" builder in
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
    i8_ptr_trash     = i8_ptr_trash;
    the_nl           = the_nl;
    funcs            = ref [main];
    build_in_table   = build_in_table;
  } in
  fun_decls := [fun_def2decl main_func];
  List.iter (codegen_build_in_decl info) build_in_defs;
  struct_types       := [None];
  activation_records := [None];
  List.iter (main_codegen_localdef info) main_func.local_defs;
  Llvm.position_at_end bb builder;
  main_codegen_block info main_func.body;
  ignore (Llvm.build_ret (c32 0) builder);
  close_scope ();
  (* Verify *)
  (*Llvm_analysis.assert_valid_module the_module;*)
  (* Optimize *)
  ignore (Llvm.PassManager.run_module the_module pm);
  (* Print out the IR *)
  Llvm.print_module "a.ll" the_module
