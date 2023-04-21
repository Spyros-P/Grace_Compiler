{
    open Parser
    exception Error of char
}

(* definitions section *)

let digit  = ['0'-'9']
let letter = ['a'-'z''A'-'Z']
let white  = [' ' '\t' '\r']
let hex    = ['0'-'9''a'-'f''A'-'F']
let escape = ['\"' '\'' '0' 'r' 't' 'n' '\\']

(* rules section *)

rule consume_single_comment = parse
    | '\n' { Lexing.new_line lexbuf; lexer lexbuf }
    | eof  { EOF }
    | [^'\n']+    { consume_single_comment lexbuf } (* consume large chunks for speed up *)

and consume_multi_comment = parse
    | "$$" { lexer lexbuf }
    | '$'  { consume_multi_comment lexbuf }
    | '\n' { Lexing.new_line lexbuf; consume_multi_comment lexbuf }
    | [^'$' '\n']+    { consume_multi_comment lexbuf } (* consume large chunks for speed up *)
    | eof  { Printf.eprintf "Multiline comment started but never closed\n"; exit 1 }

and lexer = parse
    | letter(letter | digit | '_')* as str
        { match str with
        | "and"     ->  AND
        | "char"    ->  CHAR
        | "div"     ->  DIV
        | "do"      ->  DO
        | "else"    ->  ELSE
        | "fun"     ->  FUN
        | "if"      ->  IF
        | "int"     ->  INT
        | "mod"     ->  MOD
        | "not"     ->  NOT
        | "nothing" ->  NOTHING
        | "or"      ->  OR
        | "ref"     ->  REF
        | "return"  ->  RETURN
        | "then"    ->  THEN
        | "var"     ->  VAR
        | "while"   ->  WHILE
        | _         ->  ID(str)
        }
    | '('           { L_PAREN } (* then, splitters *)
    | ')'           { R_PAREN }
    | '['           { L_BRACK }
    | ']'           { R_BRACK }
    | '{'           { L_BRACE }
    | '}'           { R_BRACE }
    | ','           { COMMA }
    | ';'           { PUNCT }
    | ':'           { SPLIT }
    | "<-"          { ASSIGN }
    | "<="          { LESS_EQ } (* then, operators *)
    | ">="          { GREATER_EQ }
    | '+'           { PLUS }
    | '-'           { MINUS }
    | '*'           { MUL }
    | '='           { EQUAL }
    | '#'           { NOT_EQUAL }
    | '<'           { LESS }
    | '>'           { GREATER }
    | digit+ as str { INTEGER(int_of_string str) }
    | '\n'          { Lexing.new_line lexbuf; lexer lexbuf } (* count new lines *)
    | white+        { lexer lexbuf } (* consume whitespaces *)
    | '$'           { consume_single_comment lexbuf } (* consume single line comment *)
    | "$$"          { consume_multi_comment lexbuf  } (* consume multi  line comment *)
    | '\''('\\' escape | "\\x" hex hex | [^'\'' '\\' '\n'])'\'' as str
        { match str.[0] with
        | '\\' ->
            ( match String.sub str 1 1 with
            | "n"  -> CHARACTER('\n')
            | "r"  -> CHARACTER('\r')
            | "t"  -> CHARACTER('\t')
            | "\\" -> CHARACTER('\\')
            | "x"  ->
                (try
                    let code = int_of_string ("0x" ^ String.sub str 2 2) in
                    CHARACTER(Char.chr code)
                with
                    _ -> failwith "Invalid hexadecimal escape sequence"
                )
                | _ -> failwith "Invalid escape sequence"
            )
        | c -> CHARACTER(c)
        }
    | '\"'('\\' escape | "\\x" hex hex | [^'\"' '\\' '\n'])*'\"' as str 
        { STRING(String.sub str 1 (String.length str-2)) }   (* remove leading and lasting double quotes *)
    | eof           { EOF }  (* lastly, eof and error *)
    | _ as chr      { raise (Error chr) }