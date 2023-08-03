
(* scopes are implemented by a list. the newest scope
    is the head of the list and the oldest scope is
    at the end of the list. Each element of the list will
    be a symbol table. *)
let scopes = []

(* define a function that adds a new scope to the list. *)
let push_scope () = scopes := (Hashtbl.create 10) :: !scopes

(* define a function that removes the newest scope from the list. *)
exception InternalError of string
let pop_scope () = scopes := 
    try
        List.tl !scopes
    with
        Failure "tl" -> raise (InternalError "Attempted to pop from empty scope stack.")

(* define a function that returns the newest scope. *)
let current_scope () = 
    try
        List.hd !scopes
    with
        Failure "hd" -> raise (InternalError "Attempted to get current scope from empty scope stack.")

(* define a function that returns the oldest scope. *)
let global_scope () =
    try
        List.hd (List.rev !scopes)
    with
        Failure "hd" -> raise (InternalError "Attempted to get global scope from empty scope stack.")

(* define a function that returns the info of a symbol in the current scope. *)
exception SymbolDefinitionError of string
let lookup id =
    try
        Hashtbl.find (current_scope ()) id
    with Not_found -> raise (SymbolDefinitionError ("Symbol " ^ id ^ " not defined."))

(* define a function that enters a symbol into the current scope. if the
  symbol already exists, it raises an appropriate error *)
let enter id info =
    if Hashtbl.mem (current_scope ()) id then
        raise (SymbolDefinitionError ("Symbol " ^ id ^ " already defined at line " ^ (lookup id)._declaration_line "."))
    else
        Hashtbl.add (current_scope ()) id info

(* define a function that removes a symbol from the current scope. *)
let remove id =
    Hashtbl.remove (current_scope ()) id

(* Define a type for the info of an entry in the symbol table. *)
type data_type = Int | Char

let string_of_data_type = function
    | Int -> "int"
    | Char -> "char"

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
    _type               = Some data_type;
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

(* define a function that checks if a function call
   happens with the appropriate parameters. It should
   check the number of parameters, their type, their
   dimensionality and whether they are references or not.*)
exception FunctionCallError of string

let check_parameters id parameters =
    let info = lookup id in
    let rec check_parameters' parameters' parameters'' =
        match parameters', parameters'' with
        | [], [] -> true
        | (id1, data_type1, dimensions1) :: t1, (id2, data_type2, dimensions2) :: t2 ->
            if data_type1 = data_type2 && dimensions1 = dimensions2 then
                check_parameters' t1 t2
            else
                raise (FunctionCallError ("Function " ^ id ^ " called with wrong parameters at line " ^ (string_of_int !line_num) ^ "."))
        | _ -> raise (FunctionCallError ("Function " ^ id ^ " called with wrong number of parameters at line " ^ (string_of_int !line_num) ^ "."))
    in
    check_parameters' info._parameters parameters

(* define a function that checks whether the declaration of a function
   agrees with the definition of a function *)

let check_func_decl_def id return_type parameters =
    let info = lookup id in
    if info._return_type = return_type && info._parameters = parameters then
        ()
    else
        raise (FunctionCallError ("Function " ^ id ^ " declared differently at line " ^ info._declaration_line ^ " than defined at line " ^ (string_of_int !line_num) ^ "."))

(* define a function that takes a list and checks if duplicate elements exist.
   this is fast O(N) with some memory overhead, but at least not O(N^2). *)
let duplicates lst =
    let rec auxilary seen rest =
        match rest with
        | [] -> false
        | head :: tail ->
            if Hashtbl.mem seen head
            then raise (InternalError ("Duplicate element was found in list."))
            else (Hashtbl.add seen head true; auxilary seen tail)
    in
        auxilary (Hashtbl.create 10) lst
      

