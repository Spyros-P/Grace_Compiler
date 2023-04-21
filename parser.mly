%{
    open Ast
    open Printf
%}

%token <string> ID
%token <string> STRING
%token <int>    INTEGER
%token <char>   CHARACTER

%token DIV      MOD
%token INT      CHAR
%token RETURN   NOTHING
%token AND      OR      NOT
%token FUN      VAR     REF
%token IF       THEN    ELSE    DO      WHILE

%token L_PAREN      "("
%token R_PAREN      ")"
%token L_BRACK      "["
%token R_BRACK      "]"
%token L_BRACE      "{"
%token R_BRACE      "}"
%token COMMA        ","
%token PUNCT        ";"
%token SPLIT        ":"
%token ASSIGN       "<-"
%token LESS_EQ      "<="
%token GREATER_EQ   ">="
%token PLUS         "+"
%token MINUS        "-"
%token MUL          "*"
%token EQUAL        "="
%token NOT_EQUAL    "#"
%token LESS         "<"
%token GREATER      ">"

%token EOF


%left OR
%left AND
%left NOT
/*%nonassoc "=" "#" ">" "<" "<=" ">="*/
%left "+" "-"
%left "*" DIV MOD

%nonassoc THEN
%nonassoc ELSE

%start program
%type <func> program
%type <func> func_def
%type <local_def list> local_def_star
%type <func_decl> header
%type <func_args list> fparams_def
%type <func_args list> semic_fpar_def_star
%type <func_args list> fpar_def
%type <string list> comma_id_star
%type <types> data_type                 /* check */
%type <types> ttype                     /* check */
%type <int list> brack_integer_star     /* check */
%type <types> ret_type                  /* check */
%type <types> fpar_type                 /* check */
%type <local_def list> local_def
%type <func_decl> func_decl
%type <var list> var_def
%type <stmt> stmt
%type <block> block
%type <stmt list> stmt_star
%type <expr> func_call
%type <expr list> fun_args
%type <lvalue> l_value
%type <expr> expr
%type <cond> cond

%%

/*                                                        ALIGN             */
/*                                                          |               */
program:/*                                                  V               */
    | f=func_def EOF                                        { f }
    ;

func_def:
    | h=header l_def=local_def_star b=block                 { { id = h.id; args = h.args; local_defs = l_def; body = b; ret = h.ret } }
    ;

local_def_star:
    | l=local_def lst=local_def_star                        { List.append l lst }
    | /* nothing */                                         { [] }
    ;

header:
    | FUN id=ID "(" p=fparams_def ")" ":" r=ret_type        { { id = id; args = p; ret = r} }
    | FUN id=ID "(" ")" ":" r=ret_type                      { { id = id; args = []; ret = r} }
    ;

/* >>> Help */
fparams_def:
    | lst1=fpar_def lst2=semic_fpar_def_star                { List.append lst1 lst2 }
    ;

semic_fpar_def_star:
    | lst1=semic_fpar_def_star ";" lst2=fpar_def            { List.append lst1 lst2 }
    | /* nothing */                                         { [] }
    ;
/* <<< Help */

fpar_def:
    | REF id=ID ids=comma_id_star ":" t=fpar_type           { List.map (fun name -> {id = name; atype = t; ref = true  }) (id::ids) }
    | id=ID ids=comma_id_star ":" t=fpar_type               { List.map (fun name -> {id = name; atype = t; ref = false }) (id::ids) }
    ;

/* >>> Help */
comma_id_star:
    | lst=comma_id_star "," id=ID                           { id::lst }
    | /* nothing */                                         { [] }
    ;
/* <<< Help */

data_type:
    | INT                                                   { EInteger([]) }
    | CHAR                                                  { ECharacter([]) }
    ;

ttype:
    | d=data_type lst=brack_integer_star                    { match d with EInteger([]) -> EInteger(lst) | ECharacter([]) -> ECharacter(lst) | _ -> failwith "Error ttype" }
    ;

/* >>> Help */
brack_integer_star:
    | "[" i=INTEGER "]" lst=brack_integer_star              { i::lst }
    | /* nothing */                                         { [] }
    ;
/* <<< Help */

ret_type:
    | d=data_type                                           { d }
    | NOTHING                                               { ENothing }
    ;

fpar_type:
    | d=data_type lst=brack_integer_star                    { match d with EInteger([]) -> EInteger(lst)     | ECharacter([]) -> ECharacter(lst)     | _ -> failwith "Error fpar_type" }
    | d=data_type "[" "]" lst=brack_integer_star            { match d with EInteger([]) -> EInteger(-1::lst) | ECharacter([]) -> ECharacter(-1::lst) | _ -> failwith "Error fpar_type" }
    ;

local_def:
    | f=func_def                                            { EFuncDef(f)::[] }
    | f=func_decl                                           { EFuncDecl(f)::[] }
    | v=var_def                                             { List.map (fun n -> EVarDef(n)) v }
    ;

func_decl:
    | h=header ";"                                          { h }
    ;

var_def:
    | VAR id=ID ids=comma_id_star ":" t=ttype ";"           { List.map (fun name -> {id = name; atype = t }) (id::ids) }
    ;

stmt:
    | ";"                                                   { EEmpty }
    | l=l_value "<-" e=expr ";"                             { EAss(l,e) }
    | b=block                                               { EBlock(b) }
    | call=func_call ";"                                    { match call with EFuncCall(id,list) -> ECallFunc(id,list) | _ -> failwith "Error stmt" }
    | IF c=cond THEN s=stmt                                 { EIf(c,s) }
    | IF c=cond THEN s1=stmt ELSE s2=stmt                   { EIfElse(c,s1,s2) }
    | WHILE c=cond DO s=stmt                                { EWhile(c,s) }
    | RETURN ";"                                            { ERet }
    | RETURN e=expr ";"                                     { ERetVal(e) }
    ;

block:
    | "{" list=stmt_star "}"                                { EListStmt(list) }
    ;

/* >>> Help */
stmt_star:
    | st=stmt list=stmt_star                                { st::list }
    | /* nothing */                                         { [] }
    ;
/* <<< Help */

func_call:
    | id=ID "(" arg=fun_args ")"                            { EFuncCall(id,arg) }
    ;

/* >>> Help */
fun_args:
    | e=expr "," list=fun_args                              { e::list }
    | e=expr                                                { e::[] }
    | /* nothing */                                         { [] }
    ;
/* <<< Help */

l_value:
    | id=ID                                                 { EAssId(id) }
    | s=STRING                                              { EAssString(s) }
    | l=l_value "[" e=expr "]"                              { EAssArrEl(l,e) }
    ;

expr:
    | i=INTEGER                                             { EInt(i) }
    | c=CHARACTER                                           { EChar(c) }
    | l=l_value                                             { ELVal(l) }
    | "(" e=expr ")"                                        { e }
    | f=func_call                                           { f }
    | "+" e=expr                                            { e }
    | "-" e=expr                                            { EUnOp(UnopMinus,e) }
    | e1=expr "+" e2=expr                                   { EBinOp(BopAdd,e1,e2) }
    | e1=expr "-" e2=expr                                   { EBinOp(BopSub,e1,e2) }
    | e1=expr "*" e2=expr                                   { EBinOp(BopMul,e1,e2) }
    | e1=expr DIV e2=expr                                   { EBinOp(BopDiv,e1,e2) }
    | e1=expr MOD e2=expr                                   { EBinOp(BopMod,e1,e2) }
    ;

cond:
    | "(" c=cond ")"                                        { c }
    | NOT c=cond                                            { ELuop(LuopNot,c) }
    | c1=cond AND c2=cond                                   { ELbop(LbopAnd,c1,c2) }
    | c1=cond OR c2=cond                                    { ELbop(LbopOr,c1,c2) }
    | e1=expr "=" e2=expr                                   { EComp(CompEq,e1,e2) }
    | e1=expr "#" e2=expr                                   { EComp(CompNeq,e1,e2) }
    | e1=expr "<" e2=expr                                   { EComp(CompLs,e1,e2) }
    | e1=expr ">" e2=expr                                   { EComp(CompGr,e1,e2) }
    | e1=expr "<=" e2=expr                                  { EComp(CompLsEq,e1,e2) }
    | e1=expr ">=" e2=expr                                  { EComp(CompGrEq,e1,e2) }
    ;
