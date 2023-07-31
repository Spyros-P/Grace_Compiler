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
%left "+" "-"
%left "*" DIV MOD

%nonassoc THEN
%nonassoc ELSE

%start program


program:
    | main=func_def EOF                                             {main}

func_def:
    | header list(local_def) block                                  {}

header:
    | FUN ID "(" separated_list(";", fpar_def) ")" ":" ret_type     {}

fpar_def:
    | REF? separated_nonempty_list(",", ID) ":" fpar_type           {}

data_type:
    | INT                                                           {}
    | CHAR                                                          {}

_integer_brackets:
    | "[" INTEGER "]"                                               {}

_empty_brackets:
    | "[" "]"                                                       {}

type:
    | data_type list(_integer_brackets)                             {}

ret_type:
    | data_type                                                     {}
    | NOTHING                                                       {}

fpar_type:
    | data_type optional(_empty_brackets) list(_integer_brackets)   {}

local_def:
    | func_def                                                      {}
    | func_decl                                                     {}
    | var_def                                                       {}

func_decl:
    | header ";"                                                    {}

var_def:
    | VAR separated_nonempty_list(",", ID) ":" type ";"             {}

stmt:
    | ";"                                                           {}
    | l_value "<-" expr ";"                                         {}
    | block                                                         {}
    | func_call ";"                                                 {}
    | IF cond THEN stmt                                             {}
    | IF cond THEN stmt ELSE stmt                                   {}
    | WHILE cond DO stmt                                            {}
    | RETURN optional(expr) ";"                                     {}

block:
    | "{" list(stmt) "}"                                            {}

func_call:
    | ID "(" separated_list(",", expr) ")"                          {}

l_value:
    | ID                                                            {}
    | STRING                                                        {}
    | l_value "[" expr "]"                                          {}

expr:
    | INTEGER                                                       {}
    | CHARACTER                                                     {}
    | l_value                                                       {}
    | "(" expr ")"                                                  {}
    | func_call                                                     {}
    | "+" expr                                                      {}
    | "-" expr                                                      {}
    | expr "+" expr                                                 {}
    | expr "-" expr                                                 {}
    | expr "*" expr                                                 {}
    | expr DIV expr                                                 {}
    | expr MOD expr                                                 {}

cond:
    | "(" cond ")"                                                  {}
    | NOT cond                                                      {}
    | cond AND cond                                                 {}
    | cond OR cond                                                  {}
    | expr EQUAL expr                                               {}
    | expr NOT_EQUAL expr                                           {}
    | expr LESS expr                                                {}
    | expr GREATER expr                                             {}
    | expr LESS_EQ expr                                             {}
    | expr GREATER_EQ expr                                          {}
