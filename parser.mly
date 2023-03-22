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
%nonassoc "=" "#" ">" "<" "<=" ">="
%left "+" "-"
%left "*" DIV MOD

%nonassoc THEN
%nonassoc ELSE

%start program
%type <block> program
%type <expr> expr
%type <lvalue> l_value
%type <expr> func_call
%type <expr list> fun_args
%type <stmt list> stmt_star
%type <block> block
%type <cond> cond

%%

/*                                                        ALIGN             */
/*                                                          |               */
program:/*                                                  V               */
    | f=block EOF                                        { f }
    ;

func_def:
    | header local_def_star block                           {}
    ;

local_def_star:
    | local_def_star local_def                              {}
    | /* nothing */                                         {}
    ;

header:
    | FUN ID "(" fparams_def ")" ":" ret_type               {}
    | FUN ID "(" ")" ":" ret_type                           {}
    ;

/* >>> Help */
fparams_def:
    | fpar_def semic_fpar_def_star                          {}
    ;

semic_fpar_def_star:
    | semic_fpar_def_star ";" fpar_def                      {}
    | /* nothing */                                         {}
    ;
/* <<< Help */

fpar_def:
    | REF ID comma_id_star ":" fpar_type                    {}
    | ID comma_id_star ":" fpar_type                        {}
    ;

/* >>> Help */
comma_id_star:
    | comma_id_star "," ID                                  {}
    | /* nothing */                                         {}
    ;
/* <<< Help */

data_type:
    | INT                                                   {}
    | CHAR                                                  {}
    ;

ttype:
    | data_type brack_integer_star                          {}
    ;

/* >>> Help */
brack_integer_star:
    | brack_integer_star "[" INTEGER "]"                    {}
    | /* nothing */                                         {}
    ;
/* <<< Help */

ret_type:
    | data_type                                             {}
    | NOTHING                                               {}
    ;

fpar_type:
    | data_type brack_integer_star                          {}
    | data_type "[" "]" brack_integer_star                  {}
    ;


local_def:
    | func_def                                              {}
    | func_decl                                             {}
    | var_def                                               {}
    ;

func_decl:
    | header ";"                                            {}
    ;

var_def:
    | VAR ID comma_id_star ":" ttype ";"                    {}
    ;

stmt:
    | ";"                                                   { EEmpty }
    | l=l_value "<-" e=expr ";"                             { EAss(l,e) }
    | b=block                                               { EBlock(b) }
    | call=func_call ";"                                    { match call with EFuncCall(id,list) -> ECallFunc(id,list) }
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
