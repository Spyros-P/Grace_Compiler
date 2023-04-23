open Printf

let error fmt =
  Printf.ksprintf (fun s -> print_string ("\027[31mError:\027[0m " ^ s)) fmt

let warning fmt =
  Printf.ksprintf (fun s -> print_string ("\027[38;5;214mWarning:\027[0m " ^ s)) fmt