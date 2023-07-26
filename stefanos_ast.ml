open Printf
open Error

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


exception DuplicateNameDefinition of string

let check_for_duplicates strings =
  let rec aux = function
    | [] -> ()
    | "" :: xs -> aux xs
    | x :: xs ->
        if List.mem x xs then
          raise (DuplicateNameDefinition ("Duplicate name definition of \"" ^ x ^ "\""))
        else
          aux xs
  in
  try
    aux strings;
    ()
  with
  | DuplicateNameDefinition msg -> ()

let rec equal_lists cmp l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (x::xs, y::ys) when cmp x y -> equal_lists cmp xs ys
  | _ -> false

exception InternalError of string

let rec remove_decl k (lst:func_decl list) =
  match lst with
  | hd :: tl ->
    if compare hd.id k = 0 then
      (hd, tl)
    else
      (match remove_decl k tl with
      | (decl, decl_list) -> (decl, hd :: decl_list))
  | _ ->
      raise (InternalError "Internal error :(")
  

let decl_of_func (f:func) =
  EFuncDecl({ id = f.id; args = f.args; ret = f.ret})

exception FunctionNotDeclared of string

let rec get_func_decl str lst =
  match lst with
  | head :: tail ->
      (match head with
      | EFuncDef s ->
          if compare s.id str = 0 then { id = s.id; args = s.args; ret = s.ret }
          else get_func_decl str tail
      | EFuncDecl s ->
          if compare s.id str = 0 then s
          else get_func_decl str tail
      | _ -> get_func_decl str tail)
  | [] -> raise (FunctionNotDeclared ("Function \"" ^ str ^ "\" has not been declared"))


let arg_to_def = fun (n:func_args) -> EVarDef({ id = n.id; atype = n.atype })

let func_def_to_decl (f:func) = EFuncDecl({ id=f.id; args=f.args; ret=f.ret })

let equal_types t1 t2 =
  match t1, t2 with
  | EInteger(l1)  , EInteger(l2)    ->  equal_lists (fun x y -> x = y || x = -1 || y = -1) l1 l2 (* should fix*)
  | ECharacter(l1), ECharacter(l2)  ->  equal_lists (fun x y -> x = y || x = -1 || y = -1) l1 l2 (* should fix*)
  | EString , EString               ->  true
  | ENothing, ENothing              ->  true
  | _ , _                           ->  false

let rec lval_is_string (l:lvalue) =
  match l with
  | EAssString(str)   ->  (true,str)
  | EAssArrEl(lval,e) ->  lval_is_string lval
  | _                 ->  (false,"")


exception FunctionArgumentError of string
exception FunctionDeclarationError of string

let rec equal_fun_args (arg1: func_args list) (arg2: func_args list) =
  match arg1, arg2 with
  | [], [] ->
      ()
  | h1::t1, h2::t2 ->
      if h1.id = h2.id then
        if equal_types h1.atype h2.atype then
          if h1.ref = h2.ref then
            raise (FunctionArgumentError "Argument reference doesn't agree in function definition and declaration")
          else
            raise (FunctionArgumentError "Argument types don't agree in function definition and declaration")
        else
          raise (FunctionArgumentError "Argument names don't agree in function definition and declaration")
      else
        raise (FunctionArgumentError "Not equal number of arguments between function definition and declaration")
  | _, _ ->
      raise (FunctionArgumentError "Not equal number of arguments between function definition and declaration")

let match_fun_decl_def (def:func) (decl:func_decl) =
  try
    equal_fun_args def.args decl.args;
    if not (equal_types def.ret decl.ret) then
      raise (FunctionDeclarationError "Return types don't agree in function definition and declaration")
  with
  | FunctionDeclarationError msg -> ()

let rec check_func_decl_def (fdef:func list) (fdecl:func_decl list) (ldef:local_def list) =
  match ldef with
  | [] ->
      begin
        match fdecl with
        | [] -> ()
        | _ ->
            List.iter (fun (f:func_decl) -> raise(FunctionDeclarationError ("Function \""^f.id^"\" declared but never defined\n"))) fdecl
      end
  | head :: tail ->
      begin
        match head with
        | EFuncDef f ->
            if (List.mem f.id (List.map (fun (f:func) -> f.id) fdef)) then begin
              raise(FunctionDeclarationError ("Function \"%s\" has already been defined\n" ^ f.id))
            end else begin
              if (List.mem f.id (List.map (fun (f:func_decl) -> f.id) fdecl)) then begin
                let (decl, decl_list) = remove_decl f.id fdecl in
                match_fun_decl_def f decl;
                check_func_decl_def (f :: fdef) decl_list tail
              end else
                check_func_decl_def (f :: fdef) fdecl tail
            end
        | EFuncDecl f ->
            if (List.mem f.id (List.map (fun (f:func_decl) -> f.id) fdecl)) then
              raise(FunctionDeclarationError ("Function \"%s\" has already been declared\n" ^ f.id))
            else begin
              if (List.mem f.id (List.map (fun (f:func) -> f.id) fdef)) then
                raise(FunctionDeclarationError ("Function \"%s\" has already been defined\n" ^ f.id))
              else
                check_func_decl_def fdef (f :: fdecl) tail
            end
        | EVarDef v ->
            check_func_decl_def fdef fdecl tail
      end


exception ArrayDimensionExceeded of string
exception ArrayBracketTypeError of string
exception VariableNotDeclared of string
exception OperatorError of string
exception AssignmentError of string
exception TypeMissmatchError of string
exception MainFunctionError of string

let rec get_lval_type f x lst =
  match x, lst with
  | EAssString str, _ ->
      ECharacter [(String.length str) + 1]
  | EAssArrEl (lval, e), _ ->
      let t = check_expr f e lst in
      if equal_types t (EInteger []) then
        let tp = get_lval_type f lval lst in
        (match tp with
        | EInteger (hd :: tl) -> EInteger tl
        | ECharacter (hd :: tl) -> ECharacter tl
        | _ -> raise (ArrayDimensionExceeded "Array dimensions have been exceeded"))
      else
        raise (ArrayBracketTypeError "Array brackets must contain an expression evaluated to an integer type")
  | EAssId str, head :: tail ->
      (match head with
      | EVarDef v ->
          if compare v.id str = 0 then v.atype
          else get_lval_type f x tail
      | _ -> get_lval_type f x tail)
  | EAssId str, [] ->
      raise (VariableNotDeclared ("Variable \"" ^ str ^ "\" has not been declared"))

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
                              else (raise(FunctionDeclarationError ("Function argument types missmatch in fuction call to \"" ^ id ^ "\"")))
  | EBinOp(bop,e1,e2)   ->  let
                              t1=(check_expr f e1 lst) and t2=(check_expr f e2 lst)
                            in
                              if (equal_types t1 (EInteger([])) && equal_types t2 (EInteger([])))
                              then t1
                              else (raise(OperatorError ("Operator \""^(binop_to_str bop)^"\" should be used between integers\n\t"^(types_to_str t1)^(binop_to_str bop)^(types_to_str t2)^"\n")))
  | EUnOp(op,e)         ->  let
                              t=(check_expr f e lst)
                            in
                              if equal_types t (EInteger([]))
                              then t
                              else (raise(OperatorError ("Operator \""^(uop_to_str op)^"\" should be applied to an integer\n\t"^(uop_to_str op)^(types_to_str t)^"\n")))

and check_cond (f:func) (cnd:cond) (lst:local_def list) =
  match cnd with
  | ELbop(b,c1,c2)    ->  check_cond f c1 lst; check_cond f c2 lst
  | ELuop(u,c)        ->  check_cond f c lst
  | EComp(com,e1,e2)  ->  let
                            t1=check_expr f e1 lst and t2=check_expr f e2 lst
                          in
                            if (equal_types t1 (EInteger([])) && equal_types t2 (EInteger([])))
                            then ()
                            else (raise(OperatorError ("Comparison operator \""^(comp_to_str com)^"\" should be used between integers\n\t"^(types_to_str t1)^(types_to_str t2)^(comp_to_str com)^"\n")))

and check_stmt (f:func) (s:stmt) (lst:local_def list) =
  match s with
  | EEmpty                ->  (warning "Empty statement\n")
  | EBlock(b)             ->  check_block f b lst
  | ECallFunc(x,y)        ->  let
                                fn=get_func_decl x lst
                              in
                                if (equal_lists equal_types (List.map (fun (n:func_args) -> n.atype) fn.args) (List.map (fun n -> check_expr f n lst) y))
                                then (match fn.ret with ENothing -> () | _ -> warning "Return value of function \"%s\" is ignored\n" x)
                                else (raise(TypeMissmatchError ("Function argument types missmatch in fuction call to \""^x^"\"\n")))
  | EAss(lval,e)          ->  (match lval_is_string lval with
                              | (res,str) ->  if res then (raise(AssignmentError ("Cannot assign to string literal. Assignment of read-only location '\""^str^"\"'\n")))
                                              else (
                                                let
                                                  t1=get_lval_type f lval lst and t2=check_expr f e lst
                                                in
                                                  if equal_types t1 t2
                                                  then ()
                                                  else (raise(TypeMissmatchError ("Type missmatching in assignment\n\t"^(types_to_str t1)^"<-"^(types_to_str t2)^"\n")))))
  | EIf(c,stm)            ->  check_cond f c lst; check_stmt f stm lst
  | EIfElse(c,stm1,stm2)  ->  check_cond f c lst; check_stmt f stm1 lst; check_stmt f stm2 lst
  | EWhile(c,stm)         ->  check_cond f c lst; check_stmt f stm lst
  | ERet                  ->  if equal_types f.ret ENothing
                              then ()
                              else (raise(TypeMissmatchError ("Function \""^f.id ^"\" returns type of \""^(types_to_str f.ret)^"\" not type of \""^(types_to_str ENothing)^"\"\n")))
  | ERetVal(e)            ->  let
                                t=(check_expr f e lst)
                              in
                                if equal_types f.ret t
                                then ()
                                else (raise(TypeMissmatchError ("Function \""^f.id^"\" returns type of \""^(types_to_str f.ret)^"\" not type of \""^(types_to_str t)^"\"\n")))

and check_block (f:func) (b:block) (lst:local_def list) =
  match b with
  | EListStmt([]) ->  (warning "Block is empty\n")
  | EListStmt(s_lst)  ->  List.iter (fun x -> check_stmt f x lst) s_lst

and check_func (f:func) (lst:local_def list) =
  check_block f f.body lst

and check_func_everything (f:func) (args:func_args list) (ldefs:local_def list) (lst:local_def list) =
  check_for_duplicates (f.id:: List.append (List.map (fun (x:func_args) -> x.id) f.args) (List.map (fun (x:local_def) -> match x with EVarDef(v) -> v.id | EFuncDef(fn) -> fn.id | _ -> "") f.local_defs));
  check_func_decl_def [] [] f.local_defs;
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
  | _ , _         ->  (raise(MainFunctionError ("Main function \""^f.id^"\" must not contain any arguments and should return nothing\n")))