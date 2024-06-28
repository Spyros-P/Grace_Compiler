{
  open Parser
  exception SyntaxError of string
}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ['0'-'9']+ as lxm { INT (int_of_string lxm) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _ as c { raise (SyntaxError (Printf.sprintf "Unexpected character: %c" c)) }
