(* diagnostics.ml *)
open Lexer
open Parser
open Lexing

type diagnostic = {
  severity: string;
  message: string;
  start_pos: int;
  end_pos: int;
}

let check_code (code: string) : diagnostic list =
  let lexbuf = from_string code in
  try
    let _ = Parser.main Lexer.token lexbuf in
    []
  with
  | Lexer.SyntaxError msg ->
    [{
      severity = "error";
      message = msg;
      start_pos = lexbuf.lex_start_p.pos_cnum;
      end_pos = lexbuf.lex_curr_p.pos_cnum;
    }]
  | Parser.Error ->
    [{
      severity = "error";
      message = "Syntax error";
      start_pos = lexbuf.lex_start_p.pos_cnum;
      end_pos = lexbuf.lex_curr_p.pos_cnum;
    }]
