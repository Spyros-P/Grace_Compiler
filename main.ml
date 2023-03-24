open Printf

let main =
  let lexbuf = Lexing.from_channel stdin in
  let res =
      try
        Parser.program Lexer.lexer lexbuf
      with
      | Lexer.Error(c) ->
          fprintf stderr "Lexical error at line %d: Unknown character '%c'\n"
            lexbuf.lex_curr_p.pos_lnum c;
          exit 1
      | Parser.Error ->
          fprintf stderr "Parse error at line %d.\n"
            lexbuf.lex_curr_p.pos_lnum;
          exit 1
  in res ()
  (*
  if res=1 then Printf.printf "Success\n"
  else Printf.printf "Fail\n"
*)