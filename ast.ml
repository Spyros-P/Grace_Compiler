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
  | ELVal   of lvalue
  | EInt    of int
  | EChar   of char
  | EFuncCall of string * expr list
  | EBinOp  of bop * expr * expr
  | EUnOp   of uop * expr

and cond =
  | ELbop   of lbop * cond * cond
  | ELuop   of luop * cond
  | EComp   of comp * expr * expr

and stmt =
  | EEmpty
  | EBlock  of block
  | ECallFunc of string * expr list
  | EAss    of lvalue * expr
  | EIf     of cond * stmt
  | EIfElse of cond * stmt * stmt
  | EWhile  of cond * stmt
  | ERet
  | ERetVal of expr

and block =
  | EListBlock  of block list
  | EListStmt   of stmt list

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

let rec print_types t =
  match t with
  | EInteger lst -> print_string "EInteger: "; List.iter (fun x -> print_int x; print_string " ") lst; print_newline ()
  | ECharacter lst -> print_string "ECharacter: "; List.iter (fun x -> print_int x; print_string " ") lst; print_newline ()
  | EString -> print_string "EString"; print_newline ()
  | ENothing -> print_string "ENothing"; print_newline ()


(* ------------------------------------------------- *)


let check_for_duplicates strings =
  let rec aux lst = match lst with
    | [] -> ()
    | x::xs ->
      if List.mem x xs then
        failwith "Error: douplicate variable definition"
      else
        aux xs
  in aux strings

let rec equal_lists cmp l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (x::xs, y::ys) when cmp x y -> equal_lists cmp xs ys
  | _ -> false
let decl_of_func (f:func) =
  EFuncDecl({ id = f.id; args = f.args; ret = f.ret})

let rec get_func_decl (str:string) (lst:local_def list) =
  match lst with
  | head::tail  ->  (match head with
                    | EFuncDef(s)   ->  if compare s.id str == 0 then {id=s.id; args=s.args; ret=s.ret} else get_func_decl str tail
                    | EFuncDecl(s)  ->  if compare s.id str == 0 then s else get_func_decl str tail
                    | _             ->  get_func_decl str tail)
  | []          ->  failwith "Error: Function has not declared"

let arg_to_def = fun (n:func_args) -> EVarDef({ id = n.id; atype = n.atype })

let func_def_to_decl (f:func) = EFuncDecl({ id=f.id; args=f.args; ret=f.ret })

let equal_types t1 t2 =
  match t1, t2 with
  | EInteger(l1)  , EInteger(l2)    ->  equal_lists (fun x y -> x = y || x = -1 || y = -1) l1 l2 (* should fix*)
  | ECharacter(l1), ECharacter(l2)  ->  equal_lists (fun x y -> x = y || x = -1 || y = -1) l1 l2 (* should fix*)
  | EString,  EString               ->  true
  | ENothing, ENothing              ->  true
  | _, _                            ->  false

let rec get_lval_type (f:func) (x:lvalue) (lst:local_def list) =
  match x, lst with
  | EAssString(str)  , _      ->  ECharacter([(String.length str)+1])
  | EAssArrEl(lval,e), _      ->  if equal_types (check_expr f e lst) (EInteger([]))
                                  then
                                    let
                                      tp=get_lval_type f lval lst
                                    in
                                      (match tp with
                                      | EInteger(hd::tl)   -> EInteger(tl)
                                      | ECharacter(hd::tl) -> ECharacter(tl)
                                      | _ -> failwith "Error: Array dimensions have been exeeded")
                                  else failwith "Error: Array brackets must contain an expression evaluted to integer"
  | EAssId(str) , head::tail  ->  (match head with
                                  | EVarDef(v) -> if compare v.id str == 0 then v.atype else get_lval_type f x tail
                                  | _ -> get_lval_type f x tail)
  | _, _                      ->  failwith "Error: Variable has not declared"

and check_expr (f:func) (e:expr) (lst:local_def list) =
  match e with
  | ELVal(x)        ->  get_lval_type f x lst
  | EInt(x)         ->  EInteger([])
  | EChar(x)        ->  ECharacter([])
  | EFuncCall(x,y)  ->  let
                          fn=get_func_decl x lst
                        in
                          if (equal_lists equal_types (List.map (fun (n:func_args) -> n.atype) fn.args) (List.map (fun n -> check_expr f n lst) y))
                          then fn.ret
                          else failwith "Error: Function argument types missmatch"
  | EBinOp(x,y,z)   ->  let
                          t1=(check_expr f y lst) and t2=(check_expr f z lst)
                        in
                          if (equal_types t1 (EInteger([])) && equal_types t2 (EInteger([])))
                          then t1
                          else failwith "Error: Operator should be used between integers"
  | EUnOp(x,y)      ->  check_expr f y lst

and check_cond (f:func) (cnd:cond) (lst:local_def list) =
  match cnd with
  | ELbop(b,c1,c2)    ->  check_cond f c1 lst; check_cond f c2 lst
  | ELuop(u,c)        ->  check_cond f c lst
  | EComp(com,e1,e2)  ->  let
                            t1=check_expr f e1 lst and t2=check_expr f e2 lst
                          in
                            if (equal_types t1 (EInteger([])) && equal_types t2 (EInteger([])))
                            then ()
                            else (print_types t1; print_types t2; failwith "Error: Missmatching in assignment")

and check_stmt (f:func) (s:stmt) (lst:local_def list) =
  match s with
  | EEmpty          ->  ()
  | EBlock(b)       ->  check_block f b lst
  | ECallFunc(x,y)  ->  let
                          fn=get_func_decl x lst
                        in
                          if (equal_lists equal_types (List.map (fun (n:func_args) -> n.atype) fn.args) (List.map (fun n -> check_expr f n lst) y))
                          then (match fn.ret with ENothing -> () | _ -> printf "\027[38;5;214mWarning:\027[0m Function called but return value is ignored\n")
                          else failwith "Error: Function argument types missmatch"
  | EAss(lval,e)    ->  let
                          t1=get_lval_type f lval lst and t2=check_expr f e lst
                        in
                          if equal_types t1 t2
                          then ()
                          else (print_types t1; print_types t2; failwith "Error: Missmatching in assignment")
  | EIf(x,y)        ->  check_cond f x lst; check_stmt f y lst
  | EIfElse(x,y,z)  ->  check_cond f x lst; check_stmt f y lst; check_stmt f z lst
  | EWhile(x,y)     ->  check_cond f x lst; check_stmt f y lst
  | ERet            ->  if equal_types f.ret ENothing
                        then ()
                        else (print_types f.ret; print_types ENothing; failwith "Error: Function return type missmatch")
  | ERetVal(e)      ->  let
                          t=(check_expr f e lst)
                        in
                          if equal_types f.ret t
                          then ()
                          else (print_types f.ret; print_types t; failwith "Error: Function return type missmatch")

and check_block (f:func) (b:block) (lst:local_def list) =
  match b with
  | EListBlock(b_lst) ->  List.iter (fun x -> check_block f x lst) b_lst
  | EListStmt(s_lst)  ->  List.iter (fun x -> check_stmt f x lst) s_lst

and check_func (f:func) (lst:local_def list) =
  (*List.iter (fun x -> match x with EVarDef(v) -> printf "Variable: %s\n" v.id | _ -> printf "Local def cannot displayed\n" ) lst;*)
  (*check_for_duplicates (List.map (fun (x:local_def) -> match x with EVarDef(v) -> v.id | _ -> failwith "Not implemented!") lst);*)
  List.iter (fun f -> match f with EFuncDef(fn) -> check_func_everything fn fn.args fn.local_defs lst | _ -> ()) f.local_defs;
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
  | _ , _         ->  failwith "Error: main function must not contain any arguments and should return nothing"