open Printf
open Error


type position = {
  line_start  : int;
  line_end    : int;
  char_start  : int;
  char_end    : int
}

let pos_zero = {line_start=0;line_end=0;char_start=0;char_end=0}

let prev_line = ref 1
let curr_line = ref 1

let prev_start_line_char = ref 1

let curr_start_line_char = ref 1

let start_line_char = ref 0

let prev_char = ref 1

let curr_char = ref 1


let filename =
  match Array.to_list Sys.argv with
  | _ :: filename :: _ -> filename
  | _ -> error "Please provide a filename as an argument\n"; exit 0

let channel =
  try open_in filename
  with Sys_error(str) -> error "%s\n" str; exit 0

let lexbuf = Lexing.from_channel channel

let update_status () =
  prev_start_line_char := !curr_start_line_char;
  curr_start_line_char := !start_line_char;
  prev_char := !curr_char;
  curr_char := lexbuf.lex_curr_p.pos_cnum;
  prev_line := !curr_line;
  curr_line := lexbuf.lex_curr_p.pos_lnum

let rec count_digits n =
  if n < 10 then
    1
  else
    1 + count_digits (n / 10)

      let print_spaces n =
  let rec print_spaces_helper count =
    if count > 0 then (
      print_char ' ';
      print_spaces_helper (count - 1)
    )
  in
  print_spaces_helper n

let print_file_line filename n =
  let rec print_lines ch count =
    if count = n then
      match input_line ch with
      | line -> print_endline line
      | exception End_of_file -> ()
    else
      match input_line ch with
      | _ -> print_lines ch (count + 1)
      | exception End_of_file -> ()
  in
  let ch = open_in filename in
  print_lines ch 1;
  close_in ch

let print_file_lines filename n m =
  let rec print_lines ch count =
    if count >= n && count <= m then
      match input_line ch with
      | line -> print_spaces ((count_digits m) - (count_digits count)); printf " %d | " count; print_endline line; print_lines ch (count + 1)
      | exception End_of_file -> ()
    else if count < n then
      (ignore (input_line ch); print_lines ch (count + 1))
    else
      ()
  in
  let ch = open_in filename in
  print_lines ch 1;
  close_in ch

let print_carat_with_spaces n =
  let rec print_spaces count =
    if count > 0 then
      (print_string " "; print_spaces (count - 1))
    else
      ()
  in
  print_spaces n;
  print_endline "^"