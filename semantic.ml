open Printf
open Error
open Read
open Ast
open Symbol


(* ------------------------------------------------- *)


let curr_fun : func_decl list ref = ref []

(* (caller,callee) tuple *)
let sibling_dependacies : (func_decl * func_decl) list ref = ref []
let father_child_dependancies : (func_decl * func_decl) list ref = ref []

(* ------------------------------------------------- *)

let update_depend depend i =
  if (i < 1) then false else
  match !depend with
  | None          ->  depend := Some(i,i); true
  | Some(min,max) ->  if (i < min) then (depend := Some(i,max); true)
                      else (if (max < i) then (depend := Some(min,i); true) else false)

let fun_def2decl (f:func) =
  { id = f.id; args = f.args; ret = f.ret; depend = ref f.depend; father_func = ref f.father_func; pos=f.pos }

let sem_closing_scope () =
  let find_decl _ entr =
    match entr with
    | Efundecl(x, _) -> error "Function \"%s\" declared but was never defined\n" x.id; print_file_lines filename x.pos.line_start x.pos.line_end
    | _ -> ()
  in Hashtbl.iter find_decl (current_scope ())

let used id =
  match lookup id with
  | Some(Efundecl(_, b),_) -> b := true
  | Some(Efuncdef(_, b),_) -> b := true
  | Some(Evar(_, b, _),_)  -> b := true
  | _ -> failwith "used" (* should fix: check if it fails with internal functions*)


let symbol_add_arg (arg:func_args) =
  (match lookup_head arg.id with
  | None -> ()
  | Some(Efuncdef(x,_)) ->  error "Name conflict: function \"%s\" and function argument \"%s\"\n" x.id x.id;
                            printf "Function definition:\n";
                            print_file_lines filename x.pos.line_start x.pos.line_end;
                            printf "\nArgument definition:\n";
                            print_file_lines filename arg.pos.line_start arg.pos.line_end;
                            exit 1
  | Some(Evar(x,_,_))   ->  error "Two function arguments have the same name \"%s\", in function \"%s\"\n" x.id (List.hd !curr_fun).id;
                            print_file_lines filename (List.hd !curr_fun).pos.line_start (List.hd !curr_fun).pos.line_end;
                            exit 1
  | _ -> failwith "symbol_add_arg");
  insert arg.id (Evar({id=arg.id;atype=arg.atype;to_ac_rec=arg.to_ac_rec;pos=arg.pos}, ref false, ref false))




let rec equal_lists cmp l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (x::xs, y::ys) when cmp x y -> equal_lists cmp xs ys
  | _ -> false

let equal_types t1 t2 =
  match t1, t2 with
  | EInteger(l1)  , EInteger(l2)    ->  equal_lists (fun x y -> x = y || x = -1 || y = -1) l1 l2 (* should fix*)
  | ECharacter(l1), ECharacter(l2)  ->  equal_lists (fun x y -> x = y || x = -1 || y = -1) l1 l2 (* should fix*)
  | EString , EString               ->  true
  | ENothing, ENothing              ->  true
  | _ , _                           ->  false



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
                                if equal_types t (EInteger([]))
                                then
                                  let
                                    tp=get_lval_type lval
                                  in
                                    (match tp with
                                    | EInteger(hd::tl)   -> EInteger(tl)
                                    | ECharacter(hd::tl) -> ECharacter(tl)
                                    | _ -> (error "Array dimensions have been exeeded\n"; print_file_lines filename pos.line_start pos.line_end; exit 1))
                                else (error "Array brackets must contain an expression evaluted to integer not type of \"%s\"\n" (types_to_str t); exit 1)
  | EAssId(str,_)         ->  used str;
                              match lookup str with
                              | None -> error "Variable \"%s\" has not declared\n" str; exit 1
                              | Some(Evar(v,_,_),i) ->  let curr_fun = List.hd !curr_fun in
                                                        if (i>0) then (v.to_ac_rec := true; ignore (update_depend !(curr_fun.depend) i)); v.atype
                              | _ -> failwith "get_lval_type"


and sem_expr (e:expr) =
  match e with
  | ELVal(l,_)              ->  get_lval_type l
  | EInt(i,_)               ->  EInteger([])
  | EChar(c,_)              ->  ECharacter([])
  | EFuncCall(id,elst,pos)  ->  let
                                  fn,i = (match lookup id with
                                        | Some(Efuncdef(decl, _),i) -> decl,i
                                        | Some(Efundecl(decl, _),i) -> decl,i
                                        | _ -> failwith "sem_expr")
                                in
                                  (used id;
                                  let curr_fn = List.hd !curr_fun in
                                  if ((i=0) && (curr_fn.id <> fn.id)) then father_child_dependancies := (curr_fn,fn)::!father_child_dependancies
                                  else if (i=1) then sibling_dependacies := (curr_fn,fn)::!sibling_dependacies;
                                  if (equal_lists equal_types (List.map (fun (n:func_args) -> n.atype) fn.args) (List.map (fun n -> sem_expr n) elst))
                                  then fn.ret
                                  else (error "Function argument types missmatch in function \"%s\"\n" fn.id;
                                        print_file_lines filename fn.pos.line_start fn.pos.line_end;
                                        printf "\nUsed in:\n";
                                        print_file_lines filename pos.line_start pos.line_end;
                                        exit 1))
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
  | ECallFunc(x,y,pos)      ->  let
                                  fn,i = match lookup x with
                                        | Some(Efuncdef(decl,_),i) -> decl,i
                                        | Some(Efundecl(decl,_),i) -> decl,i
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
                                  let curr_fn = List.hd !curr_fun in
                                  if ((i=0) && (curr_fn.id <> fn.id)) then father_child_dependancies := (curr_fn,fn)::!father_child_dependancies
                                  else if (i=1) then sibling_dependacies := (curr_fn,fn)::!sibling_dependacies;
                                  if (equal_lists equal_types (List.map (fun (n:func_args) -> n.atype) fn.args) (List.map (fun n -> sem_expr n) y))
                                  then (match fn.ret with ENothing -> () | _ -> warning "Return value (type of %s) of function \"%s\" is ignored\n" (types_to_str fn.ret) x; print_file_lines filename pos.line_start pos.line_end)
                                  else (error "Function argument types missmatch in fuction call of \"%s\"\n" x; print_file_lines filename pos.line_start pos.line_end; exit 1)
  | EAss(lval,e,pos)        ->  (match lval_is_string lval with
                                | (res,str) ->  if res then (error "Assignment of read-only location '\"%s\"'\n" str; print_file_lines filename pos.line_start pos.line_end; exit 1)
                                                else (
                                                  let
                                                    t1=(get_lval_type lval) and t2=sem_expr e
                                                  in
                                                    if equal_types t1 t2
                                                    then ()
                                                    else (error "Type missmatching in assignment:     %s <- %s\n" (types_to_str t1) (types_to_str t2); print_file_lines filename pos.line_start pos.line_end; exit 1)))
  | EIf(c,stm,_)            ->  sem_cond c; sem_stmt stm
  | EIfElse(c,stm1,stm2,_)  ->  sem_cond c; sem_stmt stm1; sem_stmt stm2
  | EWhile(c,stm,_)         ->  sem_cond c; sem_stmt stm
  | ERet(_)                 ->  let
                                  func = (List.hd !curr_fun)
                                in
                                  if equal_types func.ret ENothing
                                  then ()
                                  else (error "Function \"%s\" returns type of \"%s\" not type of \"%s\"\n" func.id (types_to_str func.ret) (types_to_str ENothing); print_file_lines filename func.pos.line_start func.pos.line_end; exit 1)
  | ERetVal(e,_)            ->  let
                                  t=(sem_expr e) and func = (List.hd !curr_fun)
                                in
                                  if equal_types func.ret t
                                  then ()
                                  else (error "Function \"%s\" returns type of \"%s\" not type of \"%s\"\n" func.id (types_to_str func.ret) (types_to_str t); print_file_lines filename func.pos.line_start func.pos.line_end; exit 1)


and sem_cond (c:cond) =
  match c with
  | ELbop(b,c1,c2,_)      ->  sem_cond c1; sem_cond c2
  | ELuop(u,c,_)          ->  sem_cond c
  | EComp(com,e1,e2,pos)  ->  let
                                t1=sem_expr e1 and t2=sem_expr e2
                              in
                                if (equal_types t1 (EInteger([])) && equal_types t2 (EInteger([])))
                                then ()
                                else (error "Comparison operator \"%s\" should be used between integers\n" (comp_to_str com);
                                      printf "\t%s %s %s\n" (types_to_str t1) (comp_to_str com) (types_to_str t2);
                                      print_file_lines filename pos.line_start pos.line_end;
                                      exit 1)


and sem_block (b:block) =
  match b with
  | EListStmt([],pos)   ->  warning "Block is empty\n"; print_file_lines filename pos.line_start pos.line_end
  | EListStmt(s_lst,_)  ->  List.iter (fun x -> sem_stmt x) s_lst


let rec symbol_add_def (decl:local_def) =
  match decl with
  | EFuncDef(func) -> (match lookup_head func.id with
                      | None -> ()
                      | Some(Efundecl(x,_)) ->  x.depend := func.depend; x.father_func := func.father_func; check_decl_def x func; remove_head func.id
                      | Some(Efuncdef(x,_)) ->  if (x.pos.line_start == 0)
                                                then (error "Name conflict: internal function \"%s\" and function \"%s\"\n" x.id x.id;
                                                      printf "Function definition:\n";
                                                      print_file_lines filename func.pos.line_start func.pos.line_end;
                                                      exit 1)
                                                else
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
                            | None -> ()
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
  f.father_func := Some(List.hd !curr_fun);
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
  in (Printf.printf "%s%s : %s\n" (String.make (depth*2) ' ') f.id (string_depend !(f.depend)); List.iter (dig depth) (f.local_defs))


let fix_depends () =
  let rec fix_siblings (lst:(func_decl*func_decl) list) =
    match lst with
    | []            ->  false
    | (fn1,fn2)::tl ->  match !(!(fn2.depend)) with
                        | None          ->  fix_siblings tl
                        | Some(min,max) ->  if (update_depend !(fn1.depend) min || update_depend !(fn1.depend) max)
                                            then true else fix_siblings tl
  in let rec fix_father_child (lst:(func_decl*func_decl) list) =
    match lst with
    | []            ->  false
    | (fn1,fn2)::tl ->  match !(!(fn2.depend)) with
                        | None          ->  fix_father_child tl
                        | Some(min,max) ->  if (update_depend !(fn1.depend) (min-1) || update_depend !(fn1.depend) (max-1))
                                            then true else fix_father_child tl
  in let result = ref true
  in while !result do
    result := fix_siblings !sibling_dependacies || fix_father_child !father_child_dependancies
  done

let sem_main (f:func) =
  match f.args, f.ret with
  | [], ENothing  ->  curr_fun := [fun_def2decl f]; sem_fun f; fix_depends ()(*; print_endline ("siblings : " ^ string_of_int (List.length !sibling_dependacies)); print_endline ("father-children : " ^ string_of_int (List.length !father_child_dependancies)); print_depend f 0*)
  | _ , _         ->  (error "Main function \"%s\" must not contain any arguments and should return nothing\n" f.id; exit 1)
