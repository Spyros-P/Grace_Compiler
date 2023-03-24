%{
    open Ast
    open Printf

    exception MyException of string

    let variable_table = Hashtbl.create 0
    let function_table = Hashtbl.create 0

    let rec add_var x = Hashtbl.add variable_table (x ()) 0

    let my_print= function
        | h::[] -> print_int (h ()); print_newline ()
        | _ -> raise(MyException "oops")

    let replace_or_raise my_hash_table key value =
    if Hashtbl.mem my_hash_table key then
        Hashtbl.replace my_hash_table key value
    else
        raise (MyException "Key not found in hash table")

    let ret = ref 0
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
%type <(unit -> unit)> program
%type <(unit -> int)> expr
%type <(unit -> string)> l_value
%type <(unit -> unit)> func_call
%type <(unit -> unit)> stmt_star
%type <(unit -> unit)> block
%type <(unit -> bool)> cond

%type <(unit -> unit)> var_def
%type <(unit -> unit) list> var_def_star

%type <(unit -> string) list> comma_id_star
%type <(unit -> int) list> fun_args

%%

/*                                                        ALIGN             */
/*                                                          |               */
program:/*                                                  V               */
    | d=var_def_star f=block EOF                            { fun _ -> List.iter (fun x -> x ()) d; f () }
    ;

var_def_star:
    | v=var_def d=var_def_star                              { v::d }
    | /* nothing */                                         { [] }
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
    | list=comma_id_star "," id=ID                          { (fun _ -> id)::list }
    | /* nothing */                                         { [] }
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
    | VAR id=ID lst=comma_id_star ":" tp=ttype ";"          { fun _ -> List.iter add_var ((fun _ -> id)::lst) }
    ;

stmt:
    | ";"                                                   { fun _ -> () }
    | l=l_value "<-" e=expr ";"                             { fun _ -> replace_or_raise variable_table (l ()) (e ()) }
    | b=block                                               { b }
    | call=func_call ";"                                    { fun _ -> call () }
    | IF c=cond THEN s=stmt                                 { fun _ -> if c () then s () else () }
    | IF c=cond THEN s1=stmt ELSE s2=stmt                   { fun _ -> if c () then s1 () else s2 () }
    | WHILE c=cond DO s=stmt                                { fun _ -> while c () do s () done }
    | RETURN ";"                                            { fun _ -> ret := 0 }
    | RETURN e=expr ";"                                     { fun _ -> ret := e () }
    ;

block:
    | "{" list=stmt_star "}"                                { list }
    ;

/* >>> Help */
stmt_star:
    | st=stmt list=stmt_star                                { fun _ -> st (); list () }
    | /* nothing */                                         { fun _ -> () }
    ;
/* <<< Help */

func_call:
    | id=ID "(" arg=fun_args ")"                            { fun _ -> match id with "writeInteger" -> my_print arg | _ -> let f = Hashtbl.find function_table id in f arg }
    ;

/* >>> Help */
fun_args:
    | e=expr "," list=fun_args                              { e::list }
    | e=expr                                                { e::[] }
    | /* nothing */                                         { [] }
    ;
/* <<< Help */

l_value:
    | id=ID                                                 { fun _ -> id }
    /*| s=STRING                                              { EAssString(s) }
    | l=l_value "[" e=expr "]"                              { EAssArrEl(l,e) }*/
    ;

expr:
    | i=INTEGER                                             { fun _ -> i }
    /*| c=CHARACTER                                           { fun _ = i }*/
    | l=l_value                                             { fun _ -> Hashtbl.find variable_table (l ()) }
    | "(" e=expr ")"                                        { e }
    | f=func_call                                           { fun _ -> f (); !ret }
    | "+" e=expr                                            { e }
    | "-" e=expr                                            { fun _ -> - e () }
    | e1=expr "+" e2=expr                                   { fun _ -> e1 () + e2 () }
    | e1=expr "-" e2=expr                                   { fun _ -> e1 () - e2 () }
    | e1=expr "*" e2=expr                                   { fun _ -> e1 () * e2 () }
    | e1=expr DIV e2=expr                                   { fun _ -> e1 () / e2 () }
    | e1=expr MOD e2=expr                                   { fun _ -> e1 () mod e2 () }
    ;

cond:
    | "(" c=cond ")"                                        { fun _ -> c () }
    | NOT c=cond                                            { fun _ -> not (c ()) }
    | c1=cond AND c2=cond                                   { fun _ -> c1 () || c2 () }
    | c1=cond OR c2=cond                                    { fun _ -> c1 () && c2 () }
    | e1=expr "=" e2=expr                                   { fun _ -> e1 () = e2 () }
    | e1=expr "#" e2=expr                                   { fun _ -> e1 () != e2 () }
    | e1=expr "<" e2=expr                                   { fun _ -> e1 () < e2 () }
    | e1=expr ">" e2=expr                                   { fun _ -> e1 () > e2 () }
    | e1=expr "<=" e2=expr                                  { fun _ -> e1 () <= e2 () }
    | e1=expr ">=" e2=expr                                  { fun _ -> e1 () >= e2 () }
    ;
