open Printf
open Error
open Read
open Ast
open Symbol


(* ------------------------------------------------- *)


let curr_fun : func_decl list ref = ref []

(* function to print the contents of the "curr_fun" list*)
let print_functions () = 
  List.iter (fun (x:func_decl) -> printf "%s\n" x.id) !curr_fun;
  printf "\n"


let get_curr_fun () =
  try
    List.hd !curr_fun
  with
    Failure _ -> failwith "get_curr_fun"

(* TODO: This is garbage, just store the main function ...*)
let get_main_fun () =
  try 
    List.hd (List.rev !curr_fun)
  with
    Failure _ -> failwith "get_main_fun"

(* (caller,callee,int) tuple *)
let caller_callee_dependancies : (func_decl * func_decl * int) list ref = ref []

(* ------------------------------------------------- *)

let update_depend depend i =
  if (i < 1) then false else
  match !depend with
  | None          ->  depend := Some(i,i); true
  | Some(min,max) ->  if (i < min) then (depend := Some(i,max); true)
                      else (if (max < i) then (depend := Some(min,i); true) else false)


let sem_closing_scope () =
  let find_decl _ entr =
    match entr with
    | Efundecl(x, _) -> error "Function \"%s\" declared but was never defined\n" x.id; print_file_lines filename x.pos.line_start x.pos.line_end
    | _ -> ()
  in Hashtbl.iter find_decl (current_scope ())


let symbol_add_arg (arg:func_args) =
  (match lookup_head arg.id with
  | None -> ()
  | Some(Efuncdef(x,_)) ->  error "Name conflict: function \"%s\" and function argument \"%s\"\n" x.id x.id;
                            printf "Function definition:\n";
                            print_file_lines filename x.pos.line_start x.pos.line_end;
                            printf "\nArgument definition:\n";
                            print_file_lines filename arg.pos.line_start arg.pos.line_end;
                            exit 1
  | Some(Evar(x,_,_)) ->  let curr_func = get_curr_fun()
                          in
                              error "Two function arguments have the same name \"%s\", in function \"%s\"\n" x.id curr_func.id;
                              print_file_lines filename curr_func.pos.line_start curr_func.pos.line_end;
                              exit 1
  | _ -> failwith "symbol_add_arg");
  insert arg.id (Evar({id=arg.id;atype=arg.atype;to_ac_rec=arg.to_ac_rec;pos=arg.pos}, ref false, ref false))



(* TODO: Agree on deleting this function *)
let rec equal_lists cmp l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (x::xs, y::ys) when cmp x y -> equal_lists cmp xs ys
  | _ -> false

let equal_types t1 t2 =
  match t1, t2 with
  | EInteger(l1)  , EInteger(l2)    ->  List.equal (fun x y -> x = y || x = -1 || y = -1) l1 l2 (* should fix*)
  | ECharacter(l1), ECharacter(l2)  ->  List.equal (fun x y -> x = y || x = -1 || y = -1) l1 l2 (* should fix*)
  | EString , EString               ->  true
  | ENothing, ENothing              ->  true
  | _ , _                           ->  false

let check_function_return_type (func:func_decl) t =
    if equal_types func.ret t
    then ()
    else (error "Function \"%s\" returns type of \"%s\" not type of \"%s\"\n" func.id (types_to_str func.ret) (types_to_str t); print_file_lines filename func.pos.line_start func.pos.line_end; exit 1)
  

let check_eq_neq_op t1 t2 com pos =
    if (equal_types t1 t2 && (equal_types t1 (EInteger([])) || equal_types t1 (ECharacter([]))))
    then ()
    else (error ("Comparison operator \"%s\" should be used between integers or between characters\n") (comp_to_str com);
          printf "\t%s %s %s\n" (types_to_str t1) (comp_to_str com) (types_to_str t2);
          print_file_lines filename pos.line_start pos.line_end;
          exit 1)

let check_comp_ops t1 t2 com pos =
    if (equal_types t1 (EInteger([])) && equal_types t2 (EInteger([])))
    then ()
    else (error "Comparison operator \"%s\" should be used between integers\n" (comp_to_str com);
          printf "\t%s %s %s\n" (types_to_str t1) (comp_to_str com) (types_to_str t2);
          print_file_lines filename pos.line_start pos.line_end;
          exit 1)

let check_decl_def (decl:func_decl) (def:func) =
  let rec equal_fun_args (arg1:func_args list) (arg2:func_args list) =
    match arg1, arg2 with
    | [], []          ->  true
    | h1::t1, h2::t2  ->  if (compare h1.id h2.id)==0
                          then (if equal_types h1.atype h2.atype
                                then (if h1.ref == h2.ref then (equal_fun_args t1 t2) else (error "Argument reference don't agree in function \"%s\" definition and declaration\n" def.id; false))
                                else (error "Argument types don't agree in function \"%s\" definition and declaration\n" def.id; false))
                          else (error "Argument names don't agree in function \"%s\" definition and declaration\n" def.id; false)
    | _ , _           ->  (error "Not equal number of arguments between function \"%s\" definition and declaration\n" def.id;false)
  in
    if (equal_fun_args def.args decl.args)
    then (if (equal_types def.ret decl.ret)
          then ()
          else (error "Return types don't agree in function \"%s\" definition and declaration\n" def.id;
                printf "Function declaration:\n";
                print_file_lines filename decl.pos.line_start decl.pos.line_end;
                printf "\nFunction definition:\n";
                print_file_lines filename def.pos.line_start def.pos.line_end;
                exit 1))
    else (printf "Function declaration:\n";
          print_file_lines filename decl.pos.line_start decl.pos.line_end;
          printf "\nFunction definition:\n";
          print_file_lines filename def.pos.line_start def.pos.line_end;
          exit 1)



let rec get_lval_type (x:lvalue) =
  match x with
  | EAssString(str,_)     ->  ECharacter([(String.length str)+1])
  | EAssArrEl(lval,e,pos) ->  let
                                t=(sem_expr e)
                              in
                                if equal_types t (EInteger([])) (*expression inside array brackets must have integer type*)
                                then
                                  let
                                    tp=get_lval_type lval
                                  in
                                    (match tp with
                                    | EInteger(hd::tl)   -> EInteger(tl)
                                    | ECharacter(hd::tl) -> ECharacter(tl)
                                    | _ -> (error "Array dimensions have been exceeded\n"; print_file_lines filename pos.line_start pos.line_end; exit 1))
                                else (error "Array brackets must contain an expression evaluted to integer not type of \"%s\"\n" (types_to_str t); exit 1)
  | EAssId(str,pos)       ->  match lookup str with
                              | None -> (error "Variable \"%s\" has not been declared\n" str; print_file_lines filename pos.line_start pos.line_end; exit 1)
                              | Some(Evar(v,b,_),i) ->  b := true; (* used *)
                                                        let curr_fun = get_curr_fun() in
                                                        if (i>0) then (v.to_ac_rec := true; ignore (update_depend !(curr_fun.depend) i)); v.atype
                              | _ -> failwith "get_lval_type"
and sem_expr (e:expr) =
  match e with
  | ELVal(l,_)              ->  get_lval_type l
  | EInt(i,_)               ->  EInteger([])
  | EChar(c,_)              ->  ECharacter([])
  | EFuncCall(id,elst,pos)  ->  begin
                                    let
                                      fn,i = (match lookup id with
                                            | Some(Efuncdef(decl, b),i) -> b := true; (* used *) decl,i
                                            | Some(Efundecl(decl, b),i) -> b := true; (* used *) decl,i
                                            | _ -> failwith "sem_expr")
                                    in
                                      (let curr_fn = get_curr_fun() in
                                      if (i = -1 && id = (get_main_fun ()).id) then
                                        (error "Main Function \"%s\" is not callable.\n" (get_main_fun ()).id ; print_file_lines filename pos.line_start pos.line_end; exit 1)
                                      else
                                        if (i>(-1) && (curr_fn.id <> fn.id)) then caller_callee_dependancies := (curr_fn,fn,i-1)::!caller_callee_dependancies;
                                        if (List.equal equal_types (List.map (fun (n:func_args) -> n.atype) fn.args) (List.map (fun n -> sem_expr n) elst))
                                        then fn.ret
                                        else (error "Function argument types missmatch in function \"%s\"\n" fn.id;
                                              print_file_lines filename fn.pos.line_start fn.pos.line_end;
                                              printf "\nUsed in:\n";
                                              print_file_lines filename pos.line_start pos.line_end;
                                              exit 1))
                                    end
  | EBinOp(bop,e1,e2,pos)   ->  let
                                  t1=(sem_expr e1) and t2=(sem_expr e2)
                                in
                                  if (equal_types t1 (EInteger([])) && equal_types t2 (EInteger([])))
                                  then t1
                                  else (error "Operator \"%s\" should be used between integers\n" (binop_to_str bop);
                                        printf "\t%s %s %s\n" (types_to_str t1) (binop_to_str bop) (types_to_str t2);
                                        print_file_lines filename pos.line_start pos.line_end;
                                        exit 1)
  | EUnOp(op,e,pos)         ->  let
                                  t=(sem_expr e)
                                in
                                  if equal_types t (EInteger([]))
                                  then t
                                  else (error "Operator \"%s\" should be assigned to an integer\n" (uop_to_str op);
                                        printf "\t%s %s\n" (uop_to_str op) (types_to_str t);
                                        print_file_lines filename pos.line_start pos.line_end;
                                        exit 1)

let rec lval_is_string (l:lvalue) =
  match l with
  | EAssString(str,_)   ->  (true,str)
  | EAssArrEl(lval,e,_) ->  lval_is_string lval
  | _                   ->  (false,"")

let rec sem_stmt (s:stmt) =
  match s with
  | EEmpty(pos)             ->  warning "Empty statement\n"; print_file_lines filename pos.line_start pos.line_end
  | EBlock(b,_)             ->  sem_block b
  | ECallFunc(x,y,pos)      ->  begin
                                    let
                                      fn,i = match lookup x with
                                            | Some(Efuncdef(decl,b),i) -> b := true; (* used *) decl,i
                                            | Some(Efundecl(decl,b),i) -> b := true; (* used *) decl,i
                                            | Some(Evar(var,_,_),_)    -> error "\"%s\" has defined as a variable but used as a function\n" x;
                                                                        printf "Variable definition:\n";
                                                                        print_file_lines filename var.pos.line_start var.pos.line_end;
                                                                        printf "\nUsed as:\n";
                                                                        print_file_lines filename pos.line_start pos.line_end;
                                                                        exit 1
                                            | None -> error "Function \"%s\" used but was not previously declared\n" x;
                                                      print_file_lines filename pos.line_start pos.line_end;
                                                    exit 1
                                    in
                                      let curr_fn = get_curr_fun() in
                                      if (i = -1 && x = (get_main_fun()).id)
                                        then (error "Main Function \"%s\" is not callable.\n" (get_main_fun ()).id ; print_file_lines filename pos.line_start pos.line_end; exit 1)
                                      else 
                                      if (i>(-1) && (curr_fn.id <> fn.id)) then caller_callee_dependancies := (curr_fn,fn,i-1)::!caller_callee_dependancies;
                                      if (List.equal equal_types (List.map (fun (n:func_args) -> n.atype) fn.args) (List.map (fun n -> sem_expr n) y))
                                      then (match fn.ret with ENothing -> () | _ -> warning "Return value (type of %s) of function \"%s\" is ignored\n" (types_to_str fn.ret) x; print_file_lines filename pos.line_start pos.line_end)
                                      else (error "Function argument types missmatch in fuction call of \"%s\"\n" x; print_file_lines filename pos.line_start pos.line_end; exit 1)
                                end
  | EAss(lval,e,pos)        ->  (match lval_is_string lval with
                                | (res,str) ->  if res then (error "Assignment of read-only location '\"%s\"'\n" str; print_file_lines filename pos.line_start pos.line_end; exit 1)
                                                else (
                                                  let
                                                    t1=(get_lval_type lval) and t2=sem_expr e
                                                  in
                                                    match t1 with
                                                    | EInteger(hd::tl)   -> error "Cannot assign to array%s.\n" str; print_file_lines filename pos.line_start pos.line_end; exit 1
                                                    | ECharacter(hd::tl) -> error "Cannot assign to array%s.\n" str; print_file_lines filename pos.line_start pos.line_end; exit 1
                                                    | _ ->  if equal_types t1 t2
                                                            then ()
                                                            else (error "Cannot assign type of \"%s\" to type of \"%s\"\n" (types_to_str t2) (types_to_str t1); print_file_lines filename pos.line_start pos.line_end; exit 1)))
  | EIf(c,stm,_)            ->  sem_cond c; sem_stmt stm
  | EIfElse(c,stm1,stm2,_)  ->  sem_cond c; sem_stmt stm1; sem_stmt stm2
  | EWhile(c,stm,_)         ->  sem_cond c; sem_stmt stm
  | ERet(_)                 ->  check_function_return_type (get_curr_fun()) ENothing
  | ERetVal(e,_)            ->  check_function_return_type (get_curr_fun()) (sem_expr e)
and sem_cond (c:cond) =
  match c with
  | ELbop(b,c1,c2,_)      ->  sem_cond c1; sem_cond c2
  | ELuop(u,c,_)          ->  sem_cond c
  | EComp(com,e1,e2,pos)  ->  let
                                t1=sem_expr e1 and t2=sem_expr e2
                              in
                                match com with
                                | CompEq   -> check_eq_neq_op t1 t2 com pos
                                | CompNeq  -> check_eq_neq_op t1 t2 com pos
                                | _        -> check_comp_ops t1 t2 com pos


and sem_block (b:block) =
  match b with
  | EListStmt([],pos)   ->  warning "Block is empty\n"; print_file_lines filename pos.line_start pos.line_end
  | EListStmt(s_lst,_)  ->  List.iter (fun x -> sem_stmt x) s_lst

let check_refs (decl:func_decl) =
  let rec walk arg_lst =
    match arg_lst with
    | hd::tl  ->  (match hd.atype with
                  | ECharacter([]) -> walk tl
                  | EInteger([]) -> walk tl
                  | _ -> if hd.ref then walk tl else false)
    | []      -> true
  in walk decl.args


let rec symbol_add_def (decl:local_def) =
  match decl with
  | EFuncDef(func) -> (match lookup_head func.id with
                      | None -> if (check_refs (fun_def2decl func)) then ()
                                else (error "Function array arguments must be declared as references\n";
                                      printf "Function definition:\n";
                                      print_file_lines filename func.pos.line_start func.pos.line_end;
                                      exit 1)
                      | Some(Efundecl(x,_)) ->  x.depend := func.depend; x.father_func := func.father_func; check_decl_def x func; remove_head func.id
                      | Some(Efuncdef(x,_)) ->  if (x.pos.line_start <> 0) then
                                                (error "Duplicate definition of function \"%s\"\n" func.id;
                                                printf "First definition:\n";
                                                print_file_lines filename x.pos.line_start x.pos.line_end;
                                                printf "\nSecond definition:\n";
                                                print_file_lines filename func.pos.line_start func.pos.line_end;
                                                exit 1)
                      | Some(Evar(x,_,_))   ->  error "Name conflict: variable \"%s\" and function \"%s\"\n" func.id func.id;
                                                printf "Variable definition:\n";
                                                print_file_lines filename x.pos.line_start x.pos.line_end;
                                                printf "\nFunction definition:\n";
                                                print_file_lines filename func.pos.line_start func.pos.line_end;
                                                exit 1);
                      insert func.id (Efuncdef(fun_def2decl func, ref false)); sem_fun func
  | EFuncDecl(func_decl) -> (match lookup_head func_decl.id with
                            | None -> if (check_refs func_decl) then ()
                                      else (error "Function array arguments must be declared as references\n";
                                            printf "Function definition:\n";
                                            print_file_lines filename func_decl.pos.line_start func_decl.pos.line_end;
                                            exit 1)
                            | Some(Efundecl(x,_)) ->  error "Duplicate declaration of function \"%s\"\n" x.id;
                                                      printf "Fist declaration:\n";
                                                      print_file_lines filename x.pos.line_start x.pos.line_end;
                                                      printf "\nSecond declaration:\n";
                                                      print_file_lines filename func_decl.pos.line_start func_decl.pos.line_end;
                                                      exit 1
                            | Some(Efuncdef(x,_)) ->  if (x.pos.line_start == 0)
                                                      then (error "Name conflict: internal function \"%s\" and function \"%s\"\n" x.id x.id;
                                                            printf "Function declaration:\n";
                                                            print_file_lines filename func_decl.pos.line_start func_decl.pos.line_end;
                                                            exit 1)
                                                      else
                                                      (error "Declaration after definition of function \"%s\"\n" x.id;
                                                      printf "Definition:\n";
                                                      print_file_lines filename x.pos.line_start x.pos.line_end;
                                                      printf "\nDeclaration:\n";
                                                      print_file_lines filename func_decl.pos.line_start func_decl.pos.line_end;
                                                      exit 1)
                            | Some(Evar(x,_,_))   ->  error "Name conflict: function \"%s\" and variable \"%s\"\n" x.id x.id;
                                                      printf "Variable definition:\n";
                                                      print_file_lines filename x.pos.line_start x.pos.line_end;
                                                      printf "\nFunction declaration:\n";
                                                      print_file_lines filename func_decl.pos.line_start func_decl.pos.line_end;
                                                      exit 1);
                            insert func_decl.id (Efundecl(func_decl, ref false))
  | EVarDef(var) -> (match lookup_head var.id with
                    | None -> ()
                    | Some(Efundecl(x,_)) ->  error "Name conflict: function \"%s\" and variable \"%s\"\n" x.id x.id;
                                              printf "Function declaration:\n";
                                              print_file_lines filename x.pos.line_start x.pos.line_end;
                                              printf "\nSecond declaration:\n";
                                              print_file_lines filename var.pos.line_start var.pos.line_end;
                                              exit 1
                    | Some(Efuncdef(x,_)) ->  if (x.pos.line_start == 0)
                                              then (error "Name conflict: internal function \"%s\" and variable \"%s\"\n" x.id x.id;
                                                    printf "Variable definition:\n";
                                                    print_file_lines filename var.pos.line_start var.pos.line_end;
                                                    exit 1)
                                              else
                                              (error "Name conflict: function \"%s\" and variable \"%s\"\n" x.id x.id;
                                              printf "Function definition:\n";
                                              print_file_lines filename x.pos.line_start x.pos.line_end;
                                              printf "\nVariable definition:\n";
                                              print_file_lines filename var.pos.line_start var.pos.line_end;
                                              exit 1)
                    | Some(Evar(x,_,_))   ->  error "Duplicate definition of variable \"%s\"\n" x.id;
                                              printf "Fist definition:\n";
                                              print_file_lines filename x.pos.line_start x.pos.line_end;
                                              printf "\nSecond definition:\n";
                                              print_file_lines filename var.pos.line_start var.pos.line_end;
                                              exit 1);
                    insert var.id (Evar(var, ref false, ref false))


and sem_fun (f:func) =
  f.father_func := Some(get_curr_fun());
  let fun_decl = fun_def2decl f in
  curr_fun := fun_decl::!curr_fun;
  open_scope ();
  insert f.id (Efuncdef(fun_decl, ref false));
  List.iter symbol_add_arg f.args;
  List.iter symbol_add_def f.local_defs;
  sem_block f.body;
  (* TODO: check for unused variables etc before closing scope and display warning messages *)
  sem_closing_scope ();
  close_scope ();
  curr_fun := List.tl !curr_fun


(* Helper function for DEBUGGING perposes ONLY *)
let rec print_depend f depth =
  let string_depend depend =
    match depend with
    | None          ->  "None"
    | Some(min,max) ->  "some(" ^ string_of_int min ^ "," ^ string_of_int max ^ ")"
  in let dig depth loc_def =
    match loc_def with
    | EFuncDef(x) ->  print_depend x (depth+1)
    | _           ->  ()
  in (Printf.printf "%s%s : %s , Father : %s , Grandfather : %s\n" (String.make (depth*2) ' ') f.id (string_depend !(f.depend))
  (match !(f.father_func) with Some(f) -> f.id | None -> "NaN") (match !(f.father_func) with Some(f) -> (match !(!(f.father_func)) with Some(f) -> f.id | None -> "NaN") | None -> "NaN");
  List.iter (dig depth) (f.local_defs))


let fix_depends () =
  let rec fix_caller_callee (lst:(func_decl*func_decl*int) list) =
    match lst with
    | []              ->  false
    | (fn1,fn2,i)::tl ->  match !(!(fn2.depend)) with
                        | None          ->  fix_caller_callee tl
                        | Some(min,max) ->  if (update_depend !(fn1.depend) (min+i) || update_depend !(fn1.depend) (max+i))
                                            then true else fix_caller_callee tl
  in let result = ref true
  in while !result do
    result := fix_caller_callee !caller_callee_dependancies
  done

let rec fill_rest_fields f =
  let rec check_params args =
    match args with
    | [] -> ()
    | hd::tl -> if !(hd.to_ac_rec) then f.gen_acc_link := true else check_params tl
  in let rec filter_defs_calc_gen loc_defs =
    match loc_defs with
    | [] -> []
    | EFuncDef(x)::tl -> x::(filter_defs_calc_gen tl)
    | EVarDef(x)::tl -> if !(x.to_ac_rec) then f.gen_acc_link := true; filter_defs_calc_gen tl
    | _::tl -> filter_defs_calc_gen tl
  in let rec store_acc_link defs =
    match defs with
    | [] -> false
    | hd::tl -> (match !(hd.depend) with
                | Some(1,i) -> if i>1 then true else store_acc_link tl
                | _ -> store_acc_link tl)
  in check_params f.args;
  let defs = filter_defs_calc_gen f.local_defs
  in f.pass_acc_link := store_acc_link defs;
  List.iter fill_rest_fields defs

let sem_main (f:func) =
  match f.args, f.ret with
  | [], ENothing  ->  curr_fun := [fun_def2decl f];
                      sem_fun f;
                      fix_depends ();
                      fill_rest_fields f(*
                      print_depend f 0*)
  | _ , _         ->  (error "Main function \"%s\" must not contain any arguments and should return nothing\n" f.id; exit 1)
