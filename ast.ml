open Printf

type bop = BopAdd | BopSub | BopMul | BopDiv | BopMod
and uop = UnopMinus
and lbop = LbopAnd | LbopOr
and luop = LuopNot
and comp = CompEq | CompNeq | CompGr | CompLs | CompGrEq | CompLsEq

and types =
  | EInteger    of int list
  | ECharacter  of int list
  | EString
  | ENothing

and lvalue =
  | EAssId     of string
  | EAssString of string
  | EAssArrEl  of lvalue * expr

and expr =
  | ELVal     of lvalue
  | EInt      of int
  | EChar     of char
  | EFuncCall of string * expr list
  | EBinOp    of bop * expr * expr
  | EUnOp     of uop * expr

and cond =
  | ELbop   of lbop * cond * cond
  | ELuop   of luop * cond
  | EComp   of comp * expr * expr

and stmt =
  | EEmpty
  | EBlock    of block
  | ECallFunc of string * expr list
  | EAss      of lvalue * expr
  | EIf       of cond * stmt
  | EIfElse   of cond * stmt * stmt
  | EWhile    of cond * stmt
  | ERet
  | ERetVal   of expr

and block = EListStmt   of stmt list

and local_def =
  | EFuncDef  of func
  | EFuncDecl of func_decl
  | EVarDef   of var

and var = {
  id: string;
  atype: types;
}

and func_args = {
  id    : string;
  atype : types;
  ref   : bool;
}

and func_decl = {
  id          : string;
  args        : func_args list;
  ret         : types;
}

and func = {
  id          : string;
  args        : func_args list;
  local_defs  : local_def list;
  body        : block;
  ret         : types;
}


(* ------------------------------------------------- *)

let build_in_defs =
  { id = "writeString"; args = { id="str"; atype=ECharacter([-1]); ref=false }::[]; ret = ENothing }::
  { id = "writeInteger"; args = { id="i"; atype=EInteger([]); ref=false }::[]; ret = ENothing }::
  { id = "readInteger"; args = []; ret = EInteger([]) }::
  { id = "strlen"; args = { id="str"; atype=ECharacter([-1]); ref=false }::[]; ret = EInteger([]) }::
  []


(* ------------------------------------------------- *)

let rec types_to_str t =
  match t with
  | EInteger(lst)   ->  "int"  ^ String.concat "" (List.map (fun x -> "[" ^ (match x with -1 -> "" | _ -> string_of_int x) ^ "]") lst)  (* not efficient*)
  | ECharacter(lst) ->  "char" ^ String.concat "" (List.map (fun x -> "[" ^ (match x with -1 -> "" | _ -> string_of_int x) ^ "]") lst)  (* not efficient*)
  | EString         ->  "string"
  | ENothing        ->  "nothing"

and print_types t =
  print_string (types_to_str t)

let comp_to_str c =
  match c with
  | CompEq    ->  "="
  | CompNeq   ->  "#"
  | CompGr    ->  ">"
  | CompLs    ->  "<"
  | CompGrEq  ->  ">="
  | CompLsEq  ->  "<="

let binop_to_str bop =
  match bop with
  | BopAdd  ->  "+"
  | BopSub  ->  "-"
  | BopMul  ->  "*"
  | BopDiv  ->  "div"
  | BopMod  ->  "mod"

let uop_to_str uop =
  match uop with
  | UnopMinus  ->  "-"

(* ------------------------------------------------- *)


let check_for_duplicates strings =
  let rec aux lst = match lst with
    | [] -> ()
    | ""::xs -> aux xs
    | x::xs ->
      if List.mem x xs then
        (printf "\027[31mError:\027[0m Douplicate name definition\n"; exit 1)
      else
        aux xs
  in aux strings

let rec equal_lists cmp l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (x::xs, y::ys) when cmp x y -> equal_lists cmp xs ys
  | _ -> false

let rec remove_decl k (lst:func_decl list) =
  match lst with
  | hd :: tl ->
    if (compare hd.id k)==0 then (hd,tl)
    else (match remove_decl k tl with (decl,decl_list) -> (decl,hd::decl_list))
  | _ -> failwith "! Not expected state !"

let decl_of_func (f:func) =
  EFuncDecl({ id = f.id; args = f.args; ret = f.ret})

let rec get_func_decl (str:string) (lst:local_def list) =
  match lst with
  | head::tail  ->  (match head with
                    | EFuncDef(s)   ->  if compare s.id str == 0 then {id=s.id; args=s.args; ret=s.ret} else get_func_decl str tail
                    | EFuncDecl(s)  ->  if compare s.id str == 0 then s else get_func_decl str tail
                    | _             ->  get_func_decl str tail)
  | []          ->  (printf "\027[31mError:\027[0m Function has not declared\n"; exit 1)

let arg_to_def = fun (n:func_args) -> EVarDef({ id = n.id; atype = n.atype })

let func_def_to_decl (f:func) = EFuncDecl({ id=f.id; args=f.args; ret=f.ret })

let equal_types t1 t2 =
  match t1, t2 with
  | EInteger(l1)  , EInteger(l2)    ->  equal_lists (fun x y -> x = y || x = -1 || y = -1) l1 l2 (* should fix*)
  | ECharacter(l1), ECharacter(l2)  ->  equal_lists (fun x y -> x = y || x = -1 || y = -1) l1 l2 (* should fix*)
  | EString , EString               ->  true
  | ENothing, ENothing              ->  true
  | _ , _                           ->  false

let rec equal_fun_args (arg1:func_args list) (arg2:func_args list) =
  match arg1, arg2 with
  | [], []          ->  ()
  | h1::t1, h2::t2  ->  if (compare h1.id h2.id)==0
                        then (printf "\027[31mError:\027[0m Argument names don't agree in function definition and declaration\n"; exit 1)
                        else ()
  | _ , _           ->  (printf "\027[31mError:\027[0m Not equal number of arguments between function definition and declaration\n";exit 1)

let rec match_fun_decl_def (def:func) (decl:func_decl) =
  if (equal_lists ) then (if
  (equal_types def.ret decl.ret) then () else failwith "error")

let rec check_func_decl_def (fdef:func list) (fdecl:func_decl list) (ldef:local_def list)  =
  match ldef with
  | [] -> (match fdecl with [] -> () | _ -> List.iter (fun (f:func_decl) -> printf "\027[31mError:\027[0m Function \"%s\" declared but never defined\n" f.id) fdecl; exit 1)
  | head::tail ->
  match head with
  | EFuncDef(f)   ->  if (List.mem f.id (List.map (fun (f:func) -> f.id) fdef))
                      then (printf "\027[31mError:\027[0m Function \"%s\" has already defined\n" f.id; exit 1)
                      else (
                        if (List.mem f.id (List.map (fun (f:func_decl) -> f.id) fdecl))
                        then (let (decl,decl_list)=remove_decl f.id fdecl
                              in match_fun_decl_def f decl;check_func_decl_def (f::fdef) decl_list tail)
                        else check_func_decl_def (f::fdef) fdecl tail
                      )
  | EFuncDecl(f)  ->  if (List.mem f.id (List.map (fun (f:func_decl) -> f.id) fdecl))
                      then (printf "\027[31mError:\027[0m Function \"%s\" has already declared\n" f.id; exit 1)
                      else (
                        if (List.mem f.id (List.map (fun (f:func) -> f.id) fdef))
                        then (printf "\027[31mError:\027[0m Function \"%s\" has already defined\n" f.id; exit 1)
                        else check_func_decl_def fdef (f::fdecl) tail
                      )
  | EVarDef(v)    ->  check_func_decl_def fdef fdecl tail

let rec get_lval_type (f:func) (x:lvalue) (lst:local_def list) =
  match x, lst with
  | EAssString(str)   , _     ->  ECharacter([(String.length str)+1])
  | EAssArrEl(lval,e) , _     ->  if equal_types (check_expr f e lst) (EInteger([]))
                                  then
                                    let
                                      tp=get_lval_type f lval lst
                                    in
                                      (match tp with
                                      | EInteger(hd::tl)   -> EInteger(tl)
                                      | ECharacter(hd::tl) -> ECharacter(tl)
                                      | _ -> (printf "\027[31mError:\027[0m Array dimensions have been exeeded\n"; exit 1))
                                  else (printf "\027[31mError:\027[0m Array brackets must contain an expression evaluted to integer\n"; exit 1)
  | EAssId(str) , head::tail  ->  (match head with
                                  | EVarDef(v) -> if compare v.id str == 0 then v.atype else get_lval_type f x tail
                                  | _ -> get_lval_type f x tail)
  | _ , _                     ->  (printf "\027[31mError:\027[0m Variable has not declared\n"; exit 1)

and check_expr (f:func) (e:expr) (lst:local_def list) =
  match e with
  | ELVal(l)            ->  get_lval_type f l lst
  | EInt(i)             ->  EInteger([])
  | EChar(c)            ->  ECharacter([])
  | EFuncCall(id,elst)  ->  let
                              fn=get_func_decl id lst
                            in
                              if (equal_lists equal_types (List.map (fun (n:func_args) -> n.atype) fn.args) (List.map (fun n -> check_expr f n lst) elst))
                              then fn.ret
                              else (printf "\027[31mError:\027[0m Function argument types missmatch\n"; exit 1)
  | EBinOp(bop,e1,e2)   ->  let
                              t1=(check_expr f e1 lst) and t2=(check_expr f e2 lst)
                            in
                              if (equal_types t1 (EInteger([])) && equal_types t2 (EInteger([])))
                              then t1
                              else (printf "\027[31mError:\027[0m Operator should be used between integers\n"; printf "\t%s %s %s\n" (types_to_str t1) (binop_to_str bop) (types_to_str t2);exit 1)
  | EUnOp(op,e)         ->  check_expr f e lst

and check_cond (f:func) (cnd:cond) (lst:local_def list) =
  match cnd with
  | ELbop(b,c1,c2)    ->  check_cond f c1 lst; check_cond f c2 lst
  | ELuop(u,c)        ->  check_cond f c lst
  | EComp(com,e1,e2)  ->  let
                            t1=check_expr f e1 lst and t2=check_expr f e2 lst
                          in
                            if (equal_types t1 (EInteger([])) && equal_types t2 (EInteger([])))
                            then ()
                            else (printf "\027[31mError:\027[0m Comparison operators must be used between integers\n"; printf "\t%s %s %s\n" (types_to_str t1) (comp_to_str com) (types_to_str t2);exit 1)

and check_stmt (f:func) (s:stmt) (lst:local_def list) =
  match s with
  | EEmpty                ->  (printf "\027[38;5;214mWarning:\027[0m Empty statement\n")
  | EBlock(b)             ->  check_block f b lst
  | ECallFunc(x,y)        ->  let
                                fn=get_func_decl x lst
                              in
                                if (equal_lists equal_types (List.map (fun (n:func_args) -> n.atype) fn.args) (List.map (fun n -> check_expr f n lst) y))
                                then (match fn.ret with ENothing -> () | _ -> printf "\027[38;5;214mWarning:\027[0m Function called but return value is ignored\n")
                                else (printf "\027[31mError:\027[0m Function argument types missmatch\n"; exit 1)
  | EAss(lval,e)          ->  let
                                t1=get_lval_type f lval lst and t2=check_expr f e lst
                              in
                                if equal_types t1 t2
                                then ()
                                else (printf "\027[31mError:\027[0m Missmatching in assignment\n"; print_string "\t"; print_types t1; print_string " <- "; print_types t2;  print_newline (); exit 1)
  | EIf(c,stm)            ->  check_cond f c lst; check_stmt f stm lst
  | EIfElse(c,stm1,stm2)  ->  check_cond f c lst; check_stmt f stm1 lst; check_stmt f stm2 lst
  | EWhile(c,stm)         ->  check_cond f c lst; check_stmt f stm lst
  | ERet                  ->  if equal_types f.ret ENothing
                              then ()
                              else (print_types f.ret; print_types ENothing; printf "\027[31mError:\027[0m Function return type missmatch\n"; exit 1)
  | ERetVal(e)            ->  let
                                t=(check_expr f e lst)
                              in
                                if equal_types f.ret t
                                then ()
                                else (print_types f.ret; print_types t; printf "\027[31mError:\027[0m Function return type missmatch\n"; exit 1)

and check_block (f:func) (b:block) (lst:local_def list) =
  match b with
  | EListStmt([]) ->  (printf "\027[38;5;214mWarning:\027[0m Block is empty\n")
  | EListStmt(s_lst)  ->  List.iter (fun x -> check_stmt f x lst) s_lst

and check_func (f:func) (lst:local_def list) =
  (*List.iter (fun x -> match x with EVarDef(v) -> printf "Variable: %s\n" v.id | _ -> printf "Local def cannot displayed\n" ) lst;*)
  (*check_for_duplicates (List.map (fun (x:local_def) -> match x with EVarDef(v) -> v.id | _ -> failwith "Not implemented!") lst);*)
  check_for_duplicates (f.id:: List.append (List.map (fun (x:func_args) -> x.id) f.args) (List.map (fun (x:local_def) -> match x with EVarDef(v) -> v.id | EFuncDef(fn) -> fn.id | _ -> "") f.local_defs));
  check_func_decl_def [] [] [];
  check_block f f.body lst

and check_func_everything (f:func) (args:func_args list) (ldefs:local_def list) (lst:local_def list) =
  match args, ldefs with
  | [], []          -> check_func f ((decl_of_func f)::lst)
  | head::tail, _   -> check_func_everything f tail ldefs ((arg_to_def head)::lst)
  | [], head::tail  -> let
                          h=(match head with
                            | EFuncDef(func) -> check_func_everything func func.args func.local_defs lst; func_def_to_decl func
                            | _ -> head)
                        in
                          check_func_everything f [] tail (h::lst)

and syntax_analysis_main (f:func) =
  match f.args, f.ret with
  | [], ENothing  ->  check_func_everything f f.args f.local_defs (List.map (fun (x:func_decl) -> EFuncDecl(x)) build_in_defs)
  | _ , _         ->  (printf "\027[31mError:\027[0m Main function \"%s\" must not contain any arguments and should return nothing\n" f.id; exit 1)