open Printf
open Error
open Read
open Ast


(* ------------------------------------------------- *)


type entry =
(*                        used           *)
| Efundecl of func_decl * bool ref
| Efuncdef of func_decl * bool ref
(*                used       initialized *)
| Evar of var * bool ref * bool ref


(* ------------------------------------------------- *)


let build_in_table =
  let table =
    Hashtbl.create 10
  and defs =
    { id = "writeString"; args = { id="str"; atype=ECharacter([-1]); ref=false; pos={line_start=0;line_end=0;char_start=0;char_end=0} }::[]; ret = ENothing; pos={line_start=0;line_end=0;char_start=0;char_end=0} }::
    { id = "writeInteger"; args = { id="i"; atype=EInteger([]); ref=false; pos={line_start=0;line_end=0;char_start=0;char_end=0} }::[]; ret = ENothing; pos={line_start=0;line_end=0;char_start=0;char_end=0} }::
    { id = "readInteger"; args = []; ret = EInteger([]); pos={line_start=0;line_end=0;char_start=0;char_end=0} }::
    { id = "strlen"; args = { id="str"; atype=ECharacter([-1]); ref=false; pos={line_start=0;line_end=0;char_start=0;char_end=0} }::[]; ret = EInteger([]); pos={line_start=0;line_end=0;char_start=0;char_end=0} }::
    []
  in List.iter (fun (def:func_decl) -> Hashtbl.add table def.id (Efuncdef(def, ref false)) ) defs; table


let symbol_table : (string, entry) Hashtbl.t list ref = ref []


(* ------------------------------------------------- *)


let open_scope () =
  symbol_table :=  (Hashtbl.create 10) :: !symbol_table

let close_scope () = symbol_table := 
  List.tl !symbol_table

let current_scope () =
  List.hd !symbol_table

let lookup_head id =
  try Some(Hashtbl.find build_in_table id)
  with Not_found ->
    try
      Some (Hashtbl.find (current_scope ()) id)
    with Not_found -> None

let lookup id =
  let rec walk id st =
    match st with
    | [] -> None
    | cs :: scopes -> try
                        Some (Hashtbl.find cs id)
                      with Not_found -> walk id scopes
  in 
    try Some(Hashtbl.find build_in_table id)
    with Not_found -> walk id !symbol_table


(* REMEMBER: check that ids dont confict with fix fun ids eg print *)
let insert id info =
  if Hashtbl.mem (current_scope ()) id then
    failwith "insert"
  else
    Hashtbl.add (current_scope ()) id info

let remove_head id =
  Hashtbl.remove (current_scope ()) id
