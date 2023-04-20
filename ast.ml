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

and func_args =
{
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

let check_for_duplicates strings =
  let rec aux lst = match lst with
    | [] -> ()
    | x::xs ->
      if List.mem x xs then
        failwith "Error: douplicate variable definition"
      else
        aux xs
  in aux strings


let decl_of_func (f:func) =
  EFuncDecl({ id = f.id; args = f.args; ret = f.ret})

let arg_to_def = fun (n:func_args) -> EVarDef({ id = n.id; atype = n.atype })

let func_def_to_decl (f:func) = EFuncDecl({ id=f.id; args=f.args; ret=f.ret })

let rec equal_int_lists l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | x1::xs1, x2::xs2 -> x1 = x2 && equal_int_lists xs1 xs2
  | _, _ -> false

let equal_types t1 t2 =
  match t1, t2 with
  | EInteger(l1)  , EInteger(l2)   -> equal_int_lists l1 l2
  | ECharacter(l1), ECharacter(l2) -> equal_int_lists l1 l2
  | EString,  EString  -> true
  | ENothing, ENothing -> true
  | _, _ -> false

let rec get_lval_type (x:lvalue) (lst:local_def list) =
  match x, lst with
  | EAssArrEl(lval,e), _ -> if equal_types (check_expr e lst) (EInteger([])) then
                              let tp=get_lval_type lval lst
                              in (match tp with
                              | EInteger(hd::tl)   -> EInteger(tl)
                              | ECharacter(hd::tl) -> ECharacter(tl)
                              | _ -> failwith "Error: Array dimensions have been exeeded")
                            else failwith "Error: Array brackets must contain an expression evaluted to integer"
  | _ , head::tail -> (let s = (match x with EAssId(str) -> str | EAssString(str) -> str | _ -> failwith "! NOT EXPECTED STATE !")
                        in match head with
                            | EVarDef(v) -> if compare v.id s == 0 then v.atype else get_lval_type x tail
                            | _ -> get_lval_type x tail)
  | _, _ -> failwith "Error: Variable has not declared"

and check_expr (e:expr) (lst:local_def list) =
  match e with
  | ELVal(x)        -> get_lval_type x lst
  | EInt(x)         -> EInteger([])
  | EChar(x)        -> ECharacter([])
  | EFuncCall(x,y)  -> failwith "local_def"
  | EBinOp(x,y,z)   -> let t1=(check_expr y lst) and t2=(check_expr z lst) in if equal_types t1 t2 then t1 else failwith "Error: Missmatching types in expression"
  | EUnOp(x,y)      -> check_expr y lst

and check_stmt (s:stmt) (lst:local_def list) =
  match s with
  | EEmpty          -> ()
  | EBlock(b)       -> check_block b lst
  | ECallFunc(x,y)  -> ()
  | EAss(lval,e)    -> let t1=get_lval_type lval lst and t2=check_expr e lst in if equal_types t1 t2 then () else failwith "Error: Missmatching in assignment"
  | EIf(x,y)        -> ()
  | EIfElse(x,y,z)  -> ()
  | EWhile(x,y)     -> ()
  | ERet            -> ()
  | ERetVal(e)      -> ()

and check_block (b:block) (lst:local_def list) =
  match b with
  | EListBlock(b_lst) -> List.iter (fun x -> check_block x lst) b_lst
  | EListStmt(s_lst)  -> List.iter (fun x -> check_stmt x lst) s_lst

and check_func (f:func) (lst:local_def list) =
  (*List.iter (fun x -> match x with EVarDef(v) -> printf "Variable: %s\n" v.id | _ -> printf "Local def cannot displayed\n" ) lst;*)
  check_for_duplicates (List.map (fun (x:local_def) -> match x with EVarDef(v) -> v.id | _ -> failwith "Not implemented!") lst);
  check_block f.body lst

and check_func_everything (f:func) (args:func_args list) (ldefs:local_def list) (lst:local_def list) =
  match args, ldefs with
  | [], [] -> check_func f lst
  | head::tail, _ -> check_func_everything f tail ldefs ((arg_to_def head)::lst)
  | [], head::tail -> let h=(match head with
                            | EFuncDef(func) -> check_func_everything func func.args func.local_defs lst; func_def_to_decl func
                            | _ -> head)
                      in
                        check_func_everything f [] tail (h::lst)