open Printf
open Read
open Error
open Semantic
open Codegen

let print_ascii_code c =
  let ascii_code = int_of_char c in
  string_of_int ascii_code

let main =
  let main_func =
      try
        Parser.program Lexer.lexer lexbuf
      with
      | Lexer.Error(c)  ->
          error "Unknown character '%c' with ascii code %s\n" c (print_ascii_code c);
          Read.print_file_lines filename lexbuf.lex_curr_p.pos_lnum lexbuf.lex_curr_p.pos_lnum;
          Read.print_carat_with_spaces (!curr_char - !curr_start_line_char + !curr_line/10 + 4);
          exit 1
      | Parser.Error    ->
          Error.error "Parse error at line %d.\n" lexbuf.lex_curr_p.pos_lnum;
          Read.print_file_lines filename !prev_line !prev_line;
          Read.print_carat_with_spaces (!prev_char - !prev_start_line_char + !prev_line/10 + 5);
          exit 1
      | Sys_error(str)  ->  error "%s: %s\n" filename str; exit 1
  in
    Semantic.sem_main main_func;
    close_in channel;
    let optimizations_enable = Array.exists (fun arg -> arg = "-O") Sys.argv in
    if !Error.errors_detected=true then exit 1
    else
      let temp_file =
        match Array.to_list Sys.argv with
        | _ :: _ :: filename :: _ -> filename
        | _ -> error "Please provide a filename as an argument\n"; exit 1
      in Codegen.llvm_compile_and_dump main_func optimizations_enable temp_file
