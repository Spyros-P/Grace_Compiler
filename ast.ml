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
  | EId     of string
  | EFuncCall of string * expr list
  | EBinOp  of bop * expr * expr
  | EUnOp   of uop * expr

and cond =
  | ELbop   of lbop * cond * cond
  | ELuop   of luop * cond
  | EComp   of comp * expr * expr

and stmt =
(*| EEmpty *)
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
  dim: int;
  size: int list;
}

and func_args =
{
  id    : string;
  atype : types;
  dim   : int;
  size  : int list;   (** empty bracket will represented by integer -1 *)
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






(* ------------------------------------------------- *)



let pprint_bop = function
  | BopAdd -> "+"
  | BopSub -> "-"
  | BopMul -> "*"
  | BopDiv -> "DIV"
  | BopMod -> "MOD"

let pprint_comp = function
| CompEq    -> "="
| CompNeq   -> "#"
| CompGr    -> ">"
| CompLs    -> "<"
| CompGrEq  -> ">="
| CompLsEq  -> "<="

let pprint_lbop = function
  | LbopAnd -> "AND"
  | LbopOr -> "OR"

let pprint_uop = function
  | UnopMinus -> "-"

let pprint_luop = function
  | LuopNot -> "NOT"


let rec print_args = function
  | [] -> sprintf ""
  | EArgInt(i)::t  -> sprintf "EInt(%d)," i ^ print_args t
  | EArgChar(c)::t -> sprintf "EChar(%c)," c ^ print_args t
  | EArgId(s)::t   -> sprintf "EId(%s)," s ^ print_args t

and print_mul_expr = function
  | [] -> sprintf ""
  | h::[] -> pprint_expr h
  | h::t  -> pprint_expr h ^ "," ^ print_mul_expr t

and pprint_expr = function
  | EBinOp(bop, e1, e2) ->
    "EBinOp(" ^ pprint_bop bop ^ ", " ^ pprint_expr e1 ^ ", " ^ pprint_expr e2 ^ ")"
  | EUnOp(uop, e) ->
    "EUnOp(" ^ pprint_uop uop ^ ", " ^ pprint_expr e ^ ")"
  | EFuncCall(name, lst) -> sprintf "EFuncCall(%s, [%s])" name (print_mul_expr lst)
  | ELVal(l) -> "ELVal(" ^ pprint_lval l ^ ")"
  | EInt(i)  -> sprintf "EInt(%d)" i
  | EChar(c) -> sprintf "EChar(%c)" c
  | EId(s)   -> sprintf "EId(%s)" s

and pprint_lval = function
  | EAssId(id)      -> sprintf "EAssId(%s)" id
  | EAssString(str) -> sprintf "EAssString(%s)" str
  | EAssArrEl(l,e)  -> "EAssArrEl(" ^pprint_lval l ^ "," ^ pprint_expr e ^ ")"

and pprint_cond = function
| ELbop(op,c1,c2)   -> "ELbop("^ pprint_lbop op ^","^ pprint_cond c1 ^","^ pprint_cond c2 ^")"
| ELuop(op,c)       -> pprint_luop op ^ "("^ pprint_cond c ^")"
| EComp(op,e1,e2)   -> "EComp("^ pprint_comp op ^","^ pprint_expr e1 ^","^ pprint_expr e2 ^")"

and pprint_stmt = function
| EEmpty              -> ";"
| EBlock(block)       -> "\n" ^ pprint_block block ^ "\n"
| ECallFunc(name, lst) -> sprintf "ECallFunc(%s, [%s])" name (print_mul_expr lst) ^ ";"
| EAss(l,e)           -> "EAss("^ pprint_lval l ^","^ pprint_expr e ^");"
| EIf(c,s)            -> "If("^ pprint_cond c ^ "," ^ pprint_stmt s ^");"
| EIfElse(c,s1,s2)    -> "IfElse("^ pprint_cond c ^ "," ^ pprint_stmt s1 ^ "," ^ pprint_stmt s2 ^");"
| EWhile(c,s)         -> "While("^ pprint_cond c ^ "," ^ pprint_stmt s ^");"
| ERet                -> "RET;"
| ERetVal(e)          -> "RET(" ^ pprint_expr e ^ ");"

and pprint_stmt_list = function
  | [] -> ""
  | h::[] -> pprint_stmt h
  | h::t  -> pprint_stmt h ^ pprint_stmt_list t

and pprint_block = function
  | EListStmt(list) -> "{\n" ^ pprint_stmt_list list ^ "\n}"