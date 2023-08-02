
(* scopes are implemented by a list. the newest scope
    is the head of the list and the oldest scope is
    at the end of the list. Each element of the list will
    be a symbol table. *)
let scopes = []

(* define a function that adds a new scope to the list. *)
let push_scope () = scopes := (Hashtbl.create 10) :: !scopes

(* define a function that removes the newest scope from the list. *)
let pop_scope () = scopes := List.tl !scopes

(* define a function that returns the newest scope. *)
let current_scope () = List.hd !scopes

(* define a function that returns the oldest scope. *)
let global_scope () = List.hd (List.rev !scopes)

(* define a function that returns the info of a symbol in the current scope. *)
let lookup id =
    try
        Hashtbl.find (current_scope ()) id
    with Not_found -> raise (Failure ("Symbol " ^ id ^ " not found."))

(* define a function that enters a symbol into the current scope. if the
  symbol already exists, it raises an appropriate error *)
let enter id info =
    if Hashtbl.mem (current_scope ()) id then
        raise (Failure ("Symbol " ^ id ^ " already defined at line " ^ (lookup id)._declaration_line "."))
    else
        Hashtbl.add (current_scope ()) id info

(* define a function that enters a symbol into the global scope. if the
  symbol already exists, it raises an appropriate error *)
let remove id =
    Hashtbl.remove (current_scope ()) id

(* Define a type for the info of an entry in the symbol table. *)
(* the name of the type coincides with the name of a symbol
  of the parser. I'm not sure if this creates a problem or not. *)
type data_type = Int | Char | Void

type entry =
{
    _is_variable:       bool;
    _type:              data_type option; (* None if function, Some data_type if a var or ref *)
    _dimensions:        int list;
    _is_function:       bool;
    _parameters:        (string * data_type * bool * int list) list;
    _return_type:       data_type option; (* None if not a function, Some data_type if a function *)
    _declaration_line:  int;
}

let make_var id data_type dimensions declaration_line =
{
    _is_variable        = true;
    _type               = data_type;
    _dimensions         = dimensions;
    _is_function        = false;
    _parameters         = [];
    _return_type        = None;
    _declaration_line   = declaration_line;
}

let make_fun id return_type parameters declaration_line =
{
    _is_variable        = false;
    _type               = None; (* Functions have no type in the variable sense *)
    _dimensions         = [];
    _is_function        = true;
    _parameters         = parameters;
    _return_type        = Some return_type;
    _declaration_line   = declaration_line;
}

(* I think that we can add many functions that
  perform checks on the symbol table entries
  and discover errors. *)