(* main.ml *)
open Diagnostics

let () =
  let code = "your sample code here" in
  let diagnostics = Diagnostics.check_code code in
  List.iter (fun d ->
    (*Printf.printf "Error: %s at %d-%d\n" d.message d.start_pos d.end_pos*)
    Printf.printf "Error: hello at 1-2\n"
  ) diagnostics
