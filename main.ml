open Printf
open Read
open Error
open Semantic

let print_ascii_code c =
  let ascii_code = int_of_char c in
  string_of_int ascii_code

let main =
  let main_func =
      try
        Parser.program Lexer.lexer lexbuf
      with
      | Lexer.Error(c) ->
          fprintf stderr "Lexical error at line %d: Unknown character '%c' with ascii code %s\n"
            lexbuf.lex_curr_p.pos_lnum c (print_ascii_code c);
          exit 1
      | Parser.Error ->
          fprintf stderr "Parse error at line %d.\n"
            lexbuf.lex_curr_p.pos_lnum;
          exit 1
  in
    sem_main main_func;
    close_in channel;
    if !errors_detected=false then printf "\027[1;32mSuccessful Compilation!\027[0m\n"