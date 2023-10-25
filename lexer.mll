{
    open Parser
    open Read
    exception Error of char

    let read_char str =
        match str.[0] with
        | '\\' ->
            ( match String.sub str 1 1 with
            | "\""  ->  '\"'
            | "\'"  ->  '\''
            | "0"   ->  '\000'
            | "r"   ->  '\r'
            | "t"   ->  '\t'
            | "n"   ->  '\n'
            | "\\"  ->  '\\'
            | "x"   ->
                (try
                    let code = int_of_string ("0x" ^ String.sub str 2 2) in
                    Char.chr code
                with
                    _ -> failwith "Invalid hexadecimal escape sequence"
                )
                | _ -> failwith "Invalid escape sequence"
            )
        | c -> c
    
    let rec read_str str =
        try match str.[0] with
            | '\\' ->
                ( match String.sub str 1 1 with
                | "\""  ->  "\""    ^ (read_str (String.sub str 2 (String.length str-2)))
                | "\'"  ->  "\'"    ^ (read_str (String.sub str 2 (String.length str-2)))
                | "0"   ->  "\000"  ^ (read_str (String.sub str 2 (String.length str-2)))
                | "r"   ->  "\r"    ^ (read_str (String.sub str 2 (String.length str-2)))
                | "t"   ->  "\t"    ^ (read_str (String.sub str 2 (String.length str-2)))
                | "n"   ->  "\n"    ^ (read_str (String.sub str 2 (String.length str-2)))
                | "\\"  ->  "\\"    ^ (read_str (String.sub str 2 (String.length str-2)))
                | "x"   ->
                    (try
                        let code = int_of_string ("0x" ^ String.sub str 2 2) in
                        (String.make 1 (Char.chr code)) ^ (read_str (String.sub str 3 (String.length str-3)))
                    with
                        _ -> failwith "Invalid hexadecimal escape sequence"
                    )
                    | _ -> failwith "Invalid escape sequence"
                )
            | c -> (String.make 1 c) ^ (read_str (String.sub str 1 (String.length str-1)))
        with _ -> ""
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
    | '\n' { start_line_char := lexbuf.lex_curr_p.pos_cnum; Lexing.new_line lexbuf; consume_multi_comment lexbuf }
    | [^'$' '\n']+    { consume_multi_comment lexbuf } (* consume large chunks for speed up *)
    | eof  { Printf.eprintf "Multiline comment started but never closed\n"; exit 1 }

and lexer = parse
    | letter(letter | digit | '_')* as str
        { update_status ();
        match str with
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
    | '('           { update_status (); L_PAREN } (* then, splitters *)
    | ')'           { update_status (); R_PAREN }
    | '['           { update_status (); L_BRACK }
    | ']'           { update_status (); R_BRACK }
    | '{'           { update_status (); L_BRACE }
    | '}'           { update_status (); R_BRACE }
    | ','           { update_status (); COMMA }
    | ';'           { update_status (); PUNCT }
    | ':'           { update_status (); SPLIT }
    | "<-"          { update_status (); ASSIGN }
    | "<="          { update_status (); LESS_EQ } (* then, operators *)
    | ">="          { update_status (); GREATER_EQ }
    | '+'           { update_status (); PLUS }
    | '-'           { update_status (); MINUS }
    | '*'           { update_status (); MUL }
    | '='           { update_status (); EQUAL }
    | '#'           { update_status (); NOT_EQUAL }
    | '<'           { update_status (); LESS }
    | '>'           { update_status (); GREATER }
    | digit+ as str { update_status (); INTEGER(int_of_string str) }
    | '\n'          { start_line_char := lexbuf.lex_curr_p.pos_cnum; Lexing.new_line lexbuf; lexer lexbuf } (* count new lines *)
    | white+        { lexer lexbuf } (* consume whitespaces *)
    | '$'           { consume_single_comment lexbuf } (* consume single line comment *)
    | "$$"          { consume_multi_comment lexbuf  } (* consume multi  line comment *)
    | '\''('\\' escape | "\\x" hex hex | [^'\'' '\\' '\n'])'\'' as str
        { update_status ();
        let str = (String.sub str 1 (String.length str-2)) in
        CHARACTER(read_char str)
        }
    | '\"'('\\' escape | "\\x" hex hex | [^'\"' '\\' '\n'])*'\"' as str 
        { update_status ();
        let str = String.sub str 1 (String.length str-2) in
        STRING(read_str str) }   (* remove leading and lasting double quotes *)
    | eof           { update_status (); EOF }  (* lastly, eof and error *)
    | _ as chr      { update_status (); raise (Error chr) }