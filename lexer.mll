(* header section *)

{
    type token =
        | T_eof | T_id
        | T_constint | T_constchar | T_conststring
        | T_and | T_char | T_div | T_do | T_else | T_fun | T_if
        | T_int | T_mod | T_not | T_nothing | T_or | T_ref | T_return
        | T_then | T_var | T_while
        | T_lpar | T_rpar | T_lbrack | T_rbrack | T_lbrace | T_rbrace
        | T_comma | T_punct | T_split | T_assign
        | T_plus | T_minus | T_mul | T_eq | T_neq | T_less | T_gr | T_lessth | T_grth
}

(* definitions section *)

    let digit  = ['0'-'9']
    let letter = ['a'-'z''A'-'Z']
    let white  = [' ' '\t' '\r' '\n']
    let escape = ['\n' '\t' '\r' '\'' '\\' '\"']

    (* rules section *)

    rule lexer = parse
    "and"       { T_and } (* firstly, keywords *)
  | "char"      { T_char }
  | "div"       { T_div }
  | "do"        { T_do }
  | "else"      { T_else }
  | "fun"       { T_fun }
  | "if"        { T_if }
  | "int"       { T_int }
  | "mod"       { T_mod }
  | "not"       { T_not }
  | "nothing"   { T_nothing }
  | "or"        { T_or }
  | "ref"       { T_ref }
  | "return"    { T_return }
  | "then"      { T_then }
  | "var"       { T_var }
  | "while"     { T_while }
  | '('         { T_lpar } (* then, splitters *)
  | ')'         { T_rpar }
  | '['         { T_lbrack }
  | ']'         { T_rbrack }
  | '{'         { T_lbrace }
  | '}'         { T_rbrace }
  | ','         { T_comma }
  | ';'         { T_punct }
  | ':'         { T_split }
  | "<-"        { T_assign }
  | "<="        { T_lessth } (* then, operators *)
  | ">="        { T_grth }
  | '+'         { T_plus }
  | '-'         { T_minus }
  | '*'         { T_mul }
  | '='         { T_eq }
  | '#'         { T_neq }
  | '<'         { T_less }
  | '>'         { T_gr }
  | white+              { lexer lexbuf } (* consume whitespaces *)
  | '$'[^'\n']*         { lexer lexbuf } (* consume single line comment *)
  | "$$"(_*)"$$"        { lexer lexbuf } (* consume multi  line comment *)
  | letter(letter | digit | '_')* { T_id }
  | digit+                        { T_constint }
  | '\"'('\\'_ | [^'\"'])*'\"'   { T_conststring} (* this is obviously lacking *)

  | eof         { T_eof }  (* lastly, eof and error *)
  | _ as chr    { Printf.eprintf "invalid character : '%c' (ascii: %d)"
                    chr (Char.code chr);
                  lexer lexbuf }

  (* trailer section *)

  {
    let string_of_token token =
        match token with
        | T_eof -> "T_eof"
        | T_id -> "T_id"
        | T_constint -> "T_constint"
        | T_constchar -> "T_constchar"
        | T_conststring -> "T_conststring"
        | T_and -> "T_and"
        | T_char -> "T_char"
        | T_div -> "T_div"
        | T_do -> "T_do"
        | T_else -> "T_else"
        | T_fun -> "T_fun"
        | T_if -> "T_if"
        | T_int -> "T_int"
        | T_mod -> "T_mod"
        | T_not -> "T_not"
        | T_nothing -> "T_nothing"
        | T_or -> "T_or"
        | T_ref -> "T_ref"
        | T_return -> "T_return"
        | T_then -> "T_then"
        | T_var -> "T_var"
        | T_while -> "T_while"
        | T_lpar -> "T_lpar"
        | T_rpar -> "T_rpar"
        | T_lbrack -> "T_lbrack"
        | T_rbrack -> "T_rbrack"
        | T_lbrace -> "T_lbrace"
        | T_rbrace -> "T_rbrace"
        | T_comma -> "T_comma"
        | T_punct -> "T_punct"
        | T_split -> "T_split"
        | T_assign -> "T_assign"
        | T_plus -> "T_plus"
        | T_minus -> "T_minus"
        | T_mul -> "T_mul"
        | T_eq -> "T_eq"
        | T_neq -> "T_neq"
        | T_less -> "T_less"
        | T_gr -> "T_gr"
        | T_lessth -> "T_lessth"
        | T_grth -> "T_grth"

    let main = 
        let lexbuf = Lexing.from_channel stdin in
        let rec loop () =
            let token = lexer lexbuf in 
            Printf.printf "token=%s, lexeme=\"%s\"\n"
            (string_of_token token) (Lexing.lexeme lexbuf);
            if token <> T_eof then loop () in
        loop ()
  }
