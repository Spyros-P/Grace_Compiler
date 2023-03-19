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
%type <int> program

%%

/*                                                            ALIGN         */
/*                                                              |           */
program:/*                                                      V           */
    | func_def EOF                                              { 1 }
    ;

func_def:
    | header local_def* block                                   {}
    ;

header:
    | FUN ID "(" fparams_def? ")" ":" ret_type                  {}
    ;

/* >>> Help */
fparams_def:
    | fpar_def semic_fpar_def*                                  {}
    ;

semic_fpar_def:
    | ";" fpar_def                                              {}
    ;
/* <<< Help */

fpar_def:
    | REF? ID comma_id* ":" fpar_type                           {}
    ;

/* >>> Help */
comma_id:
    | "," ID                                                    {}
    ;
/* <<< Help */

data_type:
    | INT                                                       {}
    | CHAR                                                      {}
    ;

ttype:
    | data_type brack_integer*                                  {}
    ;

/* >>> Help */
brack_integer:
    | "[" INTEGER "]"                                           {}
    ;
/* <<< Help */

ret_type:
    | data_type                                                 {}
    | NOTHING                                                   {}
    ;

fpar_type:
    | data_type empty_bracks? brack_integer*                    {}
    ;

/* >>> Help */
empty_bracks:
    | "[" "]"                                                   {}
    ;
/* <<< Help */

local_def:
    | func_def                                                  {}
    | func_decl                                                 {}
    | var_def                                                   {}
    ;

func_decl:
    | header ";"                                                {}
    ;

var_def:
    | VAR ID comma_id* ":" ttype ";"                            {}
    ;

stmt:
    | ";"                                                       {}
    | l_value "<-" expr ";"                                     {}
    | block                                                     {}
    | func_call ";"                                             {}
    | IF cond THEN stmt else_stmt?                              {}
    | WHILE cond DO stmt                                        {}
    | RETURN expr? ";"                                          {}
    ;

/* >>> Help */
else_stmt:
    | ELSE stmt                                                 {}
    ;
/* <<< Help */

block:
    | "{" stmt* "}"                                             {}
    ;

func_call:
    | ID "(" fun_args? ")"                                      {}
    ;

/* >>> Help */
fun_args:
    | expr comma_expr*                                          {}
    ;

comma_expr:
    | "," expr                                                  {}
    ;
/* <<< Help */

l_value:
    | ID                                                        {}
    | STRING                                                    {}
    | l_value "[" expr "]"                                      {}
    ;

expr:
    | INTEGER                                                   {}
    | CHARACTER                                                 {}
    | l_value                                                   {}
    | "(" expr ")"                                              {}
    | func_call                                                 {}
    | "+" expr                                                  {}
    | "-" expr                                                  {}
    | expr "+" expr                                             {}
    | expr "-" expr                                             {}
    | expr "*" expr                                             {}
    | expr DIV expr                                             {}
    | expr MOD expr                                             {}
    ;

cond:
    | "(" cond ")"                                              {}
    | NOT cond                                                  {}
    | cond AND cond                                             {}
    | cond OR cond                                              {}
    | expr "=" expr                                             {}
    | expr "#" expr                                             {}
    | expr "<" expr                                             {}
    | expr ">" expr                                             {}
    | expr "<=" expr                                            {}
    | expr ">=" expr                                            {}
    ;
