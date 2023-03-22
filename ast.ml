open Printf

type bop = BopAdd | BopSub | BopMul | BopDiv | BopMod
type uop = UnopMinus
type lbop = LbopAnd | Lbop | LbopOr
type luop = LuopNot
type comp = CompEq | CompNeq | CompGr | CompLs | CompGrEq | CompLsEq

type types = EInteger | ECharacter | EString | EIntList | ECharList

(* type bool = True | False *)

type func_args =
  | EArgId     of string
  | EArgInt    of int
  | EArgChar   of char

type expr =
  | EInt    of int
  | EChar   of char
  | EId     of string
  | EFuncCall of string * func_args list
  | EBinOp  of bop * expr * expr
  | EUnOp   of uop * expr

type bool_expr =
  | ELbop   of lbop * bool_expr * bool_expr
  | ELuop   of luop * bool_expr
  | EComp   of comp * expr * expr

type stmt =
  | EAss    of string * expr
  | EIf     of bool_expr * stmt
  | EIfElse of bool_expr * stmt * stmt

(* Currently no support for nested blocks *)
type block =
  | EListStmt of stmt list



type decl_list =
  | EListDecl of func_args list

type func =
  | EFun    of decl_list * block

let pprint_bop = function
  | BopAdd -> "+"
  | BopSub -> "-"
  | BopMul -> "*"
  | BopDiv -> "DIV"
  | BopMod -> "MOD"

let pprint_uop = function
  | UnopMinus -> "-"

let rec print_args = function
  | [] -> sprintf ""
  | EArgInt(i)::t -> sprintf "EInt(%d)," i ^ print_args t
  | EArgChar(c)::t -> sprintf "EChar(%c)," c ^ print_args t
  | EArgId(s)::t   -> sprintf "EId(%s)," s ^ print_args t

let rec pprint_expr = function
  | EBinOp(bop, e1, e2) ->
    "EBinOp(" ^ pprint_bop bop ^ ", " ^ pprint_expr e1 ^ ", " ^ pprint_expr e2 ^ ")"
  | EUnOp(uop, e) ->
    "EUnOp(" ^ pprint_uop uop ^ ", " ^ pprint_expr e ^ ")"
  | EFuncCall(name, lst) -> sprintf "EFuncCall(%s, [%s])" name (print_args lst)
  | EInt(i)  -> sprintf "EInt(%d)" i
  | EChar(c) -> sprintf "EChar(%c)" c
  | EId(s)   -> sprintf "EId(%s)" s