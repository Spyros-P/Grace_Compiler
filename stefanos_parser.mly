%{
    open Ast
    (* scopes are implemented by a list. the newest scope
       is the head of the list and the oldest scope is
       at the end of the list. Each element of the list will
       be a symbol table. *)
    let scopes = []

    (* define a function that adds a new scope to the list. *)
    let push_scope () = scopes := (Hashtbl.create 10) :: !scopes

    (* define a function that removes the newest scope from the list. *)
    let pop_scope () = scopes := List.tl !scopes
    
    (* define a function that returns the newest scope. *)
    let current_scope () = List.hd !scopes

    (* define a function that returns the oldest scope. *)
    let global_scope () = List.hd (List.rev !scopes)

    (* define a function that returns the info of a symbol in the current scope. *)
    let lookup id =
        try
            Hashtbl.find (current_scope ()) id
        with Not_found -> raise (Failure ("Symbol " ^ id ^ " not found."))

    (* define a function that enters a symbol into the current scope. if the
       symbol already exists, it raises an appropriate error *)
    let enter id info =
        if Hashtbl.mem (current_scope ()) id then
            raise (Failure ("Symbol " ^ id ^ " already defined."))
        else
            Hashtbl.add (current_scope ()) id info
    
    (* define a function that enters a symbol into the global scope. if the
       symbol already exists, it raises an appropriate error *)
    let remove id =
        Hashtbl.remove (current_scope ()) id

    (* Define a type for the info of an entry in the symbol table. *)
    (* the name of the type coincides with the name of a symbol
       of the parser. I'm not sure if this creates a problem or not. *)
    type data_type = Int | Char | Void

    type entry = {
        _id:                string;
        _is_variable:       bool;
        _is_reference:      bool;
        _type:              data_type option; (* None if function, Some data_type if a var or ref *)
        _dimensions:        int list;
        _is_function:       bool;
        _parameters:        (string * data_type * bool * int list) list;
        _return_type:       data_type option; (* None if not a function, Some data_type if a function *)
        _declaration_line:  int;
    }

    let make_var id data_type is_reference dimensions declaration_line =
    {
        _id                 = id;
        _is_variable        = true;
        _is_reference       = is_reference;
        _type               = data_type;
        _dimensions         = dimensions;
        _is_function        = false;
        _parameters         = [];
        _return_type        = None;
        _declaration_line   = declaration_line;
    }

    let make_fun id return_type parameters declaration_line =
    {
        _id                 = id;
        _is_variable        = false;
        _is_reference       = false; (* Functions cannot be references *)
        _type               = None; (* Functions have no type in the variable sense *)
        _dimensions         = [];
        _is_function        = true;
        _parameters         = parameters;
        _return_type        = Some return_type;
        _declaration_line   = declaration_line;
    }

    let int_type = Int
    let char_type = Char

    let sample_variable_info = make_var "x" int_type false [] 10
    let sample_function_info = make_fun "add" int_type [("a", int_type, false, []); ("b", int_type, false, [])] 20

    (* I think that we can add many functions that
       perform checks on the symbol table entries
       and discover errors. *)

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
    | h=header defs=list(local_def) b=block                         {current_scope ()}

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
    | "[" INTEGER "]"                                               {(* not sure *)}

_empty_brackets:
    | "[" "]"                                                       {(* not sure *)}

type:
    | data_type list(_integer_brackets)                             {(* not sure *)}

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
    | VAR vars=separated_nonempty_list(",", ID) ":" t=type ";"      {(*map an enter method on every element of the list*)}

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
