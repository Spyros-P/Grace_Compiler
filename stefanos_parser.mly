%{
    open Ast
    open Scopes
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
%left "+" "-"
%left "*" DIV MOD

%nonassoc THEN
%nonassoc ELSE

%start program


program:
    | main=func_def EOF                                             {main}

func_def:
    | h=header defs=list(local_def) b=block                         {}

(* the header of a function should be in charge of entering the neccessary
   function information into the symbol table. *)
header:
    | FUN id=ID "(" params=separated_list(";", fpar_def) ")" ":" t=ret_type     {}

fpar_def:
    | REF? separated_nonempty_list(",", ID) ":" t=fpar_type         {}

data_type:
    | INT                                                           {EInteger}
    | CHAR                                                          {ECharacter}

_integer_brackets:
    | "[" INTEGER "]"                                               {}

_empty_brackets:
    | "[" "]"                                                       {}

type:
    | t=data_type ints=list(_integer_brackets)                      {{type = t; dims = ints}}

ret_type:
    | data_type                                                     {}
    | NOTHING                                                       {ENothing}

fpar_type:
    | data_type optional(_empty_brackets) list(_integer_brackets)   {}

local_def:
    | f=func_def                                                    {}
    | f=func_decl                                                   {}
    | v=var_def                                                     {}

func_decl:
    | header ";"                                                    {}

(* vars is a list of variables. we must enter all of them into the
   symbol table. in order to do that, we need a symbol table. once we
   have a symbol table with an enter method, we just map the method
   to every variable in the list. *)
var_def:
    | VAR vars=separated_nonempty_list(",", ID) ":" t=type ";"      {List.iter (fun id -> enter id (make_var t.type t.dims curline)) vars}

stmt:
    | ";"                                                           {EEmpty}
    | l=l_value "<-" e=expr ";"                                     {EAssignment(l,e)}
    | b=block                                                       {EBlock(b)}
    | f=func_call ";"                                               {EFuncCall(f.id, f.args)}
    | IF c=cond THEN s=stmt                                         {EIf(c,s)}
    | IF c=cond THEN s1=stmt ELSE s2=stmt                           {EIfElse(c,s1,s2)}
    | WHILE c=cond DO s=stmt                                        {EWhile(c,s)}
    | RETURN e=optional(expr) ";"                                   {EReturn(e)}

block:
    | "{" statements=list(stmt) "}"                                 {EBlock(statements)}

func_call:
    | id=ID "(" args=separated_list(",", expr) ")"                  {EFuncCall(id,args)}

l_value:
    | id=ID                                                         {EAssId(id)}
    | s=STRING                                                      {EAssString(s)}
    | l=l_value "[" e=expr "]"                                      {EAssArrEl(l,e)}

expr:
    | i=INTEGER                                                     {EInt(i)}
    | c=CHARACTER                                                   {EChar(c)}
    | l=l_value                                                     {ELval(l)}
    | "(" e=expr ")"                                                {e}
    | f=func_call                                                   {f}
    | "+" e=expr                                                    {e}
    | "-" e=expr                                                    {EUnOp(UnopMinus,e)}
    | e1=expr "+" e2=expr                                           {EBinOp(BopAdd,e1,e2)}
    | e1=expr "-" e2=expr                                           {EBinOp(BopSub,e1,e2)}
    | e1=expr "*" e2=expr                                           {EBinOp(BopMul,e1,e2)}
    | e1=expr DIV e2=expr                                           {EBinOp(BopDiv,e1,e2)}
    | e1=expr MOD e2=expr                                           {EBinOp(BopMod,e1,e2)}

cond:
    | "(" c=cond ")"                                                {c}
    | NOT c=cond                                                    {ELuop(LuopNot,c)}
    | c1=cond    AND     c2=cond                                    {ELbop(LbopAnd,c1,c2)}
    | c1=cond    OR      c2=cond                                    {ELbop(LbopOr,c1,c2)}
    | e1=expr EQUAL      e2=expr                                    {EComp(CompEq,e1,e2)}
    | e1=expr NOT_EQUAL  e2=expr                                    {EComp(CompNeq,e1,e2)}
    | e1=expr LESS       e2=expr                                    {EComp(CompLs,e1,e2)}
    | e1=expr GREATER    e2=expr                                    {EComp(CompGr,e1,e2)}
    | e1=expr LESS_EQ    e2=expr                                    {EComp(CompLsEq,e1,e2)}
    | e1=expr GREATER_EQ e2=expr                                    {Ecomp(CompGrEq,e1,e2)}
