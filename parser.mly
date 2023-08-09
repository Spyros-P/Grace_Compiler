%{
    open Ast
    open Read
    open Printf
    open Error
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
%type <types> data_type
%type <types> ttype
%type <int list> brack_integer_star
%type <types> ret_type
%type <types> fpar_type
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
%type <bool> semicol
%type <int> line

%%

/*                                                        ALIGN             */
/*                                                          |               */
program:/*                                                  V               */
    | f=func_def EOF                                        { f }
    | EOF                                                   { error "No code found!\n"; exit 1 }
    ;

func_def:
    | h=header l_def=local_def_star b=block                 { { id = h.id; args = h.args; local_defs = l_def; body = b; ret = h.ret; pos=h.pos } }
    ;

local_def_star:
    | l=local_def lst=local_def_star                        { List.append l lst }
    | /* nothing */                                         { [] }
    ;

header:
    | FUN ln1=line id=ID "(" p=fparams_def ")" ":" r=ret_type ln2=line         { { id = id; args = p; ret = r; pos={line_start=ln1;line_end=ln2;char_start=0;char_end=0}} }
    | FUN ln1=line id=ID "(" ")" ":" r=ret_type ln2=line                      { { id = id; args = []; ret = r; pos={line_start=ln1;line_end=ln2;char_start=0;char_end=0}} }
    ;

/* >>> Help */
fparams_def:
    | lst1=fpar_def lst2=semic_fpar_def_star                { List.append lst1 lst2 }
    ;

semic_fpar_def_star:
    | lst1=semic_fpar_def_star ";" lst2=fpar_def    { List.append lst1 lst2 }
    | /* nothing */                                         { [] }
    ;
/* <<< Help */

fpar_def:
    | REF ln1=line id=ID ids=comma_id_star ":" t=fpar_type ln2=line           { List.map (fun name -> {id = name; atype = t; ref = true;  pos={line_start=ln1;line_end=ln2;char_start=0;char_end=0} }) (id::ids) }
    | id=ID ln1=line ids=comma_id_star ":" t=fpar_type ln2=line               { List.map (fun name -> {id = name; atype = t; ref = false; pos={line_start=ln1;line_end=ln2;char_start=0;char_end=0} }) (id::ids) }
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
    | d=data_type lst=brack_integer_star                    { match d with EInteger([]) -> EInteger(lst) | ECharacter([]) -> ECharacter(lst) | _ -> (error "Internal error :(  {error code: ttype in parser}\n"; exit 1) }
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
    | d=data_type lst=brack_integer_star                    { match d with EInteger([]) -> EInteger(lst)     | ECharacter([]) -> ECharacter(lst)     | _ -> (error "Internal error :(  {error code: fpar_type in parser}\n"; exit 1) }
    | d=data_type "[" "]" lst=brack_integer_star            { match d with EInteger([]) -> EInteger(-1::lst) | ECharacter([]) -> ECharacter(-1::lst) | _ -> (error "Internal error :(  {error code: fpar_type in parser}\n"; exit 1) }
    ;

local_def:
    | f=func_def                                            { EFuncDef(f)::[] }
    | f=func_decl                                           { EFuncDecl(f)::[] }
    | v=var_def                                             { List.map (fun n -> EVarDef(n)) v }
    ;

func_decl:
    | h=header sem=semicol                          { if sem=false then (error "Missing semicolon at line %d\n" !prev_line; print_file_lines filename !prev_line !prev_line; print_carat_with_spaces (!prev_char - !prev_start_line_char + !prev_line/10 + 5)); h }
    ;

var_def:
    | VAR ln1=line id=ID ids=comma_id_star ":" t=ttype ln2=line sem=semicol           { if sem=false then (error "Missing semicolon at line %d\n" ln2; print_file_lines filename ln1 ln2; print_carat_with_spaces (!prev_char - !prev_start_line_char + ln2/10 + 5)); List.map (fun name -> {id = name; atype = t; pos={line_start=ln1;line_end=ln2;char_start=0;char_end=0} }) (id::ids) }
    ;

stmt:
    | ";"                                                   { EEmpty({line_start=(!prev_line);line_end=(!prev_line);char_start=0;char_end=0}) }
    | l=l_value ln1=line "<-" e=expr ln2=line sem=semicol   { if sem=false then (error "Missing semicolon at line %d\n" ln2; print_file_lines filename ln1 ln2; print_carat_with_spaces (!prev_char - !prev_start_line_char + ln2/10 + 5)); EAss(l,e,{line_start=ln1;line_end=(!prev_line);char_start=0;char_end=0}) }
    | b=block                                               { EBlock(b,{line_start=0;line_end=0;char_start=0;char_end=0}) }
    | call=func_call ln=line sem=semicol                    { if sem=false then (error "Missing semicolon at line %d\n" ln; print_file_lines filename ln ln; print_carat_with_spaces (!prev_char - !prev_start_line_char + ln/10 + 5)); match call with EFuncCall(id,list,pos) -> ECallFunc(id,list,pos) | _ -> (error "Internal error :(  {error code: stmt in parser}\n"; exit 1) }
    | IF c=cond THEN s=stmt                                 { EIf(c,s,{line_start=0;line_end=0;char_start=0;char_end=0}) }
    | IF c=cond THEN s1=stmt ELSE s2=stmt                   { EIfElse(c,s1,s2,{line_start=0;line_end=0;char_start=0;char_end=0}) }
    | WHILE c=cond DO s=stmt                                { EWhile(c,s,{line_start=0;line_end=0;char_start=0;char_end=0}) }
    | RETURN sem=semicol                                    { if sem=false then (error "Missing semicolon at line %d\n" !prev_line; print_file_lines filename !prev_line !prev_line; print_carat_with_spaces (!prev_char - !prev_start_line_char + !prev_line/10 + 5)); ERet({line_start=0;line_end=0;char_start=0;char_end=0}) }
    | RETURN e=expr sem=semicol                             { if sem=false then (error "Missing semicolon at line %d\n" !prev_line; print_file_lines filename !prev_line !prev_line; print_carat_with_spaces (!prev_char - !prev_start_line_char + !prev_line/10 + 5)); ERetVal(e,{line_start=0;line_end=0;char_start=0;char_end=0}) }
    ;

block:
    | "{" ln1=line list=stmt_star "}" ln2=line                                { EListStmt(list,{line_start=ln1;line_end=ln2;char_start=0;char_end=0}) }
    ;

/* >>> Help */
stmt_star:
    | st=stmt list=stmt_star                                { st::list }
    | /* nothing */                                         { [] }
    ;
/* <<< Help */

func_call:
    | id=ID ln1=line "(" arg=fun_args ")" ln2=line          { EFuncCall(id,arg,{line_start=ln1;line_end=ln2;char_start=0;char_end=0}) }
    ;

/* >>> Help */
fun_args:
    | e=expr "," list=fun_args                              { e::list }
    | e=expr                                                { e::[] }
    | /* nothing */                                         { [] }
    ;
/* <<< Help */

l_value:
    | id=ID ln=line                                         { EAssId(id,{line_start=ln;line_end=ln;char_start=0;char_end=0}) }
    | s=STRING ln=line                                      { EAssString(s,{line_start=ln;line_end=ln;char_start=0;char_end=0}) }
    | l=l_value ln1=line "[" e=expr "]" ln2=line            { EAssArrEl(l,e,{line_start=ln1;line_end=ln2;char_start=0;char_end=0}) }
    ;

expr:
    | i=INTEGER ln=line                                     { EInt(i,{line_start=ln;line_end=ln;char_start=0;char_end=0}) }
    | c=CHARACTER ln=line                                   { EChar(c,{line_start=ln;line_end=ln;char_start=0;char_end=0}) }
    | l=l_value                                             { let pos=get_lval_pos l in ELVal(l,pos) }
    | "(" e=expr ")"                                        { e }
    | f=func_call                                           { f }
    | "+" ln=line e=expr                                    { let pos=get_expr_pos e in EUnOp(UnopPlus,e,{line_start=ln;line_end=pos.line_end;char_start=0;char_end=0}) }
    | "-" ln=line e=expr                                    { let pos=get_expr_pos e in EUnOp(UnopMinus,e,{line_start=ln;line_end=pos.line_end;char_start=0;char_end=0}) }
    | e1=expr "+" e2=expr                                   { let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EBinOp(BopAdd,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e1=expr "-" e2=expr                                   { let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EBinOp(BopSub,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e1=expr "*" e2=expr                                   { let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EBinOp(BopMul,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e1=expr DIV e2=expr                                   { let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EBinOp(BopDiv,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e1=expr MOD e2=expr                                   { let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EBinOp(BopMod,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    ;

cond:
    | "(" c=cond ")"                                        { c }
    | NOT ln=line c=cond                                    { let pos= get_cond_pos c in ELuop(LuopNot,c,pos) }
    | c1=cond AND c2=cond                                   { let pos1=get_cond_pos c1 and pos2=get_cond_pos c2 in ELbop(LbopAnd,c1,c2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | c1=cond OR c2=cond                                    { let pos1=get_cond_pos c1 and pos2=get_cond_pos c2 in ELbop(LbopOr,c1,c2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e1=expr "=" e2=expr                                   { let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EComp(CompEq,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e1=expr "#" e2=expr                                   { let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EComp(CompNeq,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e1=expr "<" e2=expr                                   { let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EComp(CompLs,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e1=expr ">" e2=expr                                   { let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EComp(CompGr,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e1=expr "<=" e2=expr                                  { let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EComp(CompLsEq,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e1=expr ">=" e2=expr                                  { let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EComp(CompGrEq,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    ;


semicol:
    | /* nothing */                                         { false }
    | ";"                                                   { true }
    ;

line:
    | /* nothing */                                         { !prev_line }
    ;