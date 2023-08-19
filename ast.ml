open Printf
open Error
open Read


(* ------------------------------------------------- *)


type bop = BopAdd | BopSub | BopMul | BopDiv | BopMod
and uop  = UnopPlus | UnopMinus
and lbop = LbopAnd | LbopOr
and luop = LuopNot
and comp = CompEq | CompNeq | CompGr | CompLs | CompGrEq | CompLsEq

and types =
  | EInteger    of int list
  | ECharacter  of int list
  | EString
  | ENothing

and lvalue =
  | EAssId     of string * position
  | EAssString of string * position
  | EAssArrEl  of lvalue * expr * position

and expr =
  | ELVal     of lvalue * position
  | EInt      of int * position
  | EChar     of char * position
  | EFuncCall of string * expr list * position
  | EBinOp    of bop * expr * expr * position
  | EUnOp     of uop * expr * position

and cond =
  | ELbop   of lbop * cond * cond * position
  | ELuop   of luop * cond * position
  | EComp   of comp * expr * expr * position

and stmt =
  | EEmpty    of position
  | EBlock    of block * position
  | ECallFunc of string * expr list * position
  | EAss      of lvalue * expr * position
  | EIf       of cond * stmt * position
  | EIfElse   of cond * stmt * stmt * position
  | EWhile    of cond * stmt * position
  | ERet      of position
  | ERetVal   of expr * position

and block = EListStmt   of stmt list * position

and local_def =
  | EFuncDef  of func
  | EFuncDecl of func_decl
  | EVarDef   of var

and var = {
  id        : string;
  atype     : types;
  to_ac_rec : bool ref;         (* stored in 'activation record' llvm struct *)
  pos       : position;
}

and func_args = {
  id        : string;
  atype     : types;
  ref       : bool;
  to_ac_rec : bool ref;         (* stored in 'activation record' llvm struct *)
  pos       : position;
}

and func_decl = {
  id                : string;
  args              : func_args list;
  ret               : types;
  depend            : (int * int) option ref ref;
  father_func       : func_decl option ref ref;  (* may not be necessaey *)
  pos               : position;
}

and func = {
  id                : string;
  args              : func_args list;
  local_defs        : local_def list;
  body              : block;
  ret               : types;
  gen_acc_link      : bool ref;   (* function generates an 'activation record'/'access link' *)
  depend            : (int * int) option ref;
  father_func       : func_decl option ref;
  pos               : position;
}


(* ------------------------------------------------- *)

let build_in_defs =
  { id = "writeString"; args = { id="str"; atype=ECharacter([-1]); ref=false; to_ac_rec=ref false; pos=pos_zero }::[]; ret = ENothing; depend=ref (ref None); father_func=ref (ref None); pos=pos_zero }::
  { id = "writeInteger"; args = { id="i"; atype=EInteger([]); ref=false; to_ac_rec=ref false; pos=pos_zero }::[]; ret = ENothing; depend=ref (ref None); father_func=ref (ref None); pos=pos_zero }::
  { id = "readInteger"; args = []; ret = EInteger([]); depend=ref (ref None); father_func=ref (ref None); pos=pos_zero }::
  { id = "strlen"; args = { id="str"; atype=ECharacter([-1]); ref=false; to_ac_rec=ref false; pos=pos_zero }::[]; ret = EInteger([]); depend=ref (ref None); father_func=ref (ref None); pos=pos_zero }::
  []


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
  | UnopPlus   ->  "+"
  | UnopMinus  ->  "-"


(* ------------------------------------------------- *)


let get_expr_pos (e:expr) =
  match e with
  | ELVal(_,pos)        ->  pos
  | EInt(_,pos)         ->  pos
  | EChar(_,pos)        ->  pos
  | EFuncCall(_,_,pos)  ->  pos
  | EBinOp(_,_,_,pos)   ->  pos
  | EUnOp(_,_,pos)      ->  pos

let get_lval_pos (l:lvalue) =
match l with
| EAssId(_,pos)       -> pos
| EAssString(_,pos)   -> pos
| EAssArrEl(_,_,pos)  -> pos

let get_cond_pos (c:cond) =
  match c with
  | ELbop(_,_,_,pos)  -> pos
  | ELuop(_,_,pos)    -> pos
  | EComp(_,_,_,pos)  -> pos
