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

%right ";"          (* used for shift/reduce conflicts *)
%right ID           (* used for shift/reduce conflict : return involed *)
%right STRING       (* used for shift/reduce conflict : return involed *)


%start program
%type <func> program
%type <func> func_def
%type <func_decl> header
%type <func_args list> fpar_def
%type <types> data_type
%type <types> ttype
%type <types> ret_type
%type <types> fpar_type
%type <local_def list> local_def
%type <func_decl> func_decl
%type <var list> var_def
%type <stmt> stmt
%type <block> block
%type <expr> func_call
%type <expr list> fun_args
%type <lvalue> l_value
%type <expr> expr
%type <cond> cond
%type <bool> semicol
%type <int> line

%%


program:
    | f=func_def EOF                                                            { f }
    | EOF                                                                       { error "No code found!\n"; exit 0 }
    ;

func_def:
    | h=header l_def=list(local_def) b=block                                    { { id = h.id; args = h.args; local_defs = List.concat l_def; body = b; ret = h.ret; pass_acc_link=ref false; gen_acc_link=ref false; depend=ref None; father_func=ref None; pos=h.pos } }
    ;


header:
    | FUN ln1=line id=ID  p=delimited("(", separated_list(";", fpar_def), ")") ":" r=ret_type ln2=line          { { id = id; args = List.concat p; ret = r; depend=ref (ref None); father_func=ref (ref None); pos={line_start=ln1;line_end=ln2;char_start=0;char_end=0}} }
    ;

fpar_def:
    | r=boption(REF) ln1=line ids=separated_nonempty_list(",", ID) ":" t=fpar_type ln2=line       { List.map (fun name -> {id = name; atype = t; ref = r;  to_ac_rec=ref false; pos={line_start=ln1;line_end=ln2;char_start=0;char_end=0} }) ids }
    ;

data_type:
    | INT                                                                       { EInteger([]) }
    | CHAR                                                                      { ECharacter([]) }
    ;

ttype:
    | d=data_type lst=list(delimited("[", INTEGER, "]"))                        { match d with EInteger([]) -> EInteger(lst) | ECharacter([]) -> ECharacter(lst) | _ -> (error "Internal error :(  {error code: ttype in parser}\n"; exit 0) }
    ;

ret_type:
    | d=data_type                                                               { d }
    | NOTHING                                                                   { ENothing }
    ;

fpar_type:
    | d=data_type lst=list(delimited("[", INTEGER, "]"))                        { match d with EInteger([]) -> EInteger(lst)     | ECharacter([]) -> ECharacter(lst)     | _ -> (error "Internal error :(  {error code: fpar_type in parser}\n"; exit 0) }
    | d=data_type "[" "]" lst=list(delimited("[", INTEGER, "]"))                { match d with EInteger([]) -> EInteger(-1::lst) | ECharacter([]) -> ECharacter(-1::lst) | _ -> (error "Internal error :(  {error code: fpar_type in parser}\n"; exit 0) }
    ;

local_def:
    | f=func_def                                                                { EFuncDef(f)::[] }
    | f=func_decl                                                               { EFuncDecl(f)::[] }
    | v=var_def                                                                 { List.map (fun n -> EVarDef(n)) v }
    ;

func_decl:
    | h=terminated(header,";")                                                  { h }
    ;

var_def:
    | VAR ln1=line ids=separated_nonempty_list(",", ID) ":" t=ttype ln2=line sem=semicol          { if sem=false then (error "Missing semicolon at line %d\n" ln2; print_file_lines filename ln1 ln2; print_carat_with_spaces (!prev_char - !prev_start_line_char + ln2/10 + 5)); List.map (fun name -> {id = name; atype = t; to_ac_rec=ref false; pos={line_start=ln1;line_end=ln2;char_start=0;char_end=0} }) ids }
    ;

stmt:
    | ";"                                                                       { EEmpty({line_start=(!prev_line);line_end=(!prev_line);char_start=0;char_end=0}) }
    | l=l_value ln1=line "<-" e=expr ln2=line sem=semicol                       { if sem=false then (error "Missing semicolon at line %d\n" ln2; print_file_lines filename ln1 ln2; print_carat_with_spaces (!prev_char - !prev_start_line_char + ln2/10 + 5)); EAss(l,e,{line_start=ln1;line_end=(!prev_line);char_start=0;char_end=0}) }
    | b=block                                                                   { match b with EListStmt(_,pos) -> EBlock(b,pos) }
    | call=func_call ln=line sem=semicol                                        { if sem=false then (error "Missing semicolon at line %d\n" ln; print_file_lines filename ln ln; print_carat_with_spaces (!prev_char - !prev_start_line_char + ln/10 + 5)); match call with EFuncCall(id,list,pos) -> ECallFunc(id,list,pos) | _ -> (error "Internal error :(  {error code: stmt in parser}\n"; exit 0) }
    | IF c=cond THEN s=stmt                                                     { EIf(c,s,{line_start=0;line_end=0;char_start=0;char_end=0}) }
    | IF c=cond THEN s1=stmt ELSE s2=stmt                                       { EIfElse(c,s1,s2,{line_start=0;line_end=0;char_start=0;char_end=0}) }
    | WHILE c=cond DO s=stmt                                                    { EWhile(c,s,{line_start=0;line_end=0;char_start=0;char_end=0}) }
    | RETURN sem=semicol                                                        { if sem=false then (error "Missing semicolon at line %d\n" !prev_line; print_file_lines filename !prev_line !prev_line; print_carat_with_spaces (!prev_char - !prev_start_line_char + !prev_line/10 + 5)); ERet({line_start=0;line_end=0;char_start=0;char_end=0}) }
    | RETURN e=expr sem=semicol                                                 { if sem=false then (error "Missing semicolon at line %d\n" !prev_line; print_file_lines filename !prev_line !prev_line; print_carat_with_spaces (!prev_char - !prev_start_line_char + !prev_line/10 + 5)); ERetVal(e,{line_start=0;line_end=0;char_start=0;char_end=0}) }
    ;

block:
    | "{" ln1=line list=list(stmt) "}" ln2=line                                 { EListStmt(list,{line_start=ln1;line_end=ln2;char_start=0;char_end=0}) }
    ;


func_call:
    | id=ID ln1=line arg=delimited("(", fun_args, ")") ln2=line                 { EFuncCall(id,arg,{line_start=ln1;line_end=ln2;char_start=0;char_end=0}) }
    ;

fun_args:
    | list=separated_list(",", expr)                                            { list }
    ;

l_value:
    | id=ID ln=line                                                             { EAssId(id,{line_start=ln;line_end=ln;char_start=0;char_end=0}) }
    | s=STRING ln=line                                                          { EAssString(s,{line_start=ln;line_end=ln;char_start=0;char_end=0}) }
    | l=l_value ln1=line e=delimited("[", expr, "]") ln2=line                   { EAssArrEl(l,e,{line_start=ln1;line_end=ln2;char_start=0;char_end=0}) }
    ;

expr:
    | i=INTEGER ln=line                                                         { EInt(i,{line_start=ln;line_end=ln;char_start=0;char_end=0}) }
    | c=CHARACTER ln=line                                                       { EChar(c,{line_start=ln;line_end=ln;char_start=0;char_end=0}) }
    | l=l_value                                                                 { let pos=get_lval_pos l in ELVal(l,pos) }
    | e = delimited("(", expr, ")")                                             { e }
    | f=func_call                                                               { f }
    | "+" ln=line e=expr                                                        { let pos=get_expr_pos e in EUnOp(UnopPlus,e,{line_start=ln;line_end=pos.line_end;char_start=0;char_end=0}) }
    | "-" ln=line e=expr                                                        { let pos=get_expr_pos e in EUnOp(UnopMinus,e,{line_start=ln;line_end=pos.line_end;char_start=0;char_end=0}) }
    | e = separated_pair(expr, "+", expr)                                       { let (e1, e2) = e in let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EBinOp(BopAdd,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e = separated_pair(expr, "-", expr)                                       { let (e1, e2) = e in let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EBinOp(BopSub,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e = separated_pair(expr, "*", expr)                                       { let (e1, e2) = e in let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EBinOp(BopMul,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e = separated_pair(expr, DIV, expr)                                       { let (e1, e2) = e in let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EBinOp(BopDiv,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e = separated_pair(expr, MOD, expr)                                       { let (e1, e2) = e in let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EBinOp(BopMod,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    ;

cond:
    | c = delimited("(", cond, ")")                                             { c }
    | NOT ln=line c=cond                                                        { let pos= get_cond_pos c in ELuop(LuopNot,c,pos) }
    | c = separated_pair(cond, AND, cond)                                       { let (c1, c2) = c in let pos1=get_cond_pos c1 and pos2=get_cond_pos c2 in ELbop(LbopAnd,c1,c2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | c = separated_pair(cond, OR, cond)                                        { let (c1, c2) = c in let pos1=get_cond_pos c1 and pos2=get_cond_pos c2 in ELbop(LbopOr,c1,c2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e = separated_pair(expr, "=", expr)                                       { let (e1, e2) = e in let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EComp(CompEq,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e = separated_pair(expr, "#", expr)                                       { let (e1, e2) = e in let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EComp(CompNeq,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e = separated_pair(expr, "<", expr)                                       { let (e1, e2) = e in let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EComp(CompLs,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e = separated_pair(expr, ">", expr)                                       { let (e1, e2) = e in let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EComp(CompGr,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e = separated_pair(expr, "<=", expr)                                      { let (e1, e2) = e in let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EComp(CompLsEq,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    | e = separated_pair(expr, ">=", expr)                                      { let (e1, e2) = e in let pos1=get_expr_pos e1 and pos2=get_expr_pos e2 in EComp(CompGrEq,e1,e2,{line_start=pos1.line_start;line_end=pos2.line_end;char_start=0;char_end=0}) }
    ;

semicol:
    | /* nothing */ %prec PUNCT                                                 { false }
    | ";"                                                                       { true }
    ;

line:
    | /* nothing */                                                             { !prev_line }
    ;