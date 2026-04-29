(*
  This module provides helper functions for converting lists and runtime values
  into strings, as well as printing values and environments in a readable format.
  These functions are primarily used for debugging, testing, and program output.
*)


(*
  Converts a list into a string using a provided element-to-string function.
  Used for debugging token streams and parser errors.
*)
let string_of_list (f : 'a -> string) (lst : 'a list) : string =
  let rec aux = function
    | [] -> ""
    | [x] -> f x
    | x :: xs -> f x ^ " " ^ aux xs
  in
  aux lst

(*Converts a runtime value into its string reprsentation.*)
let string_of_value v =
  match v with
  | Int_Val n -> string_of_int n
  | Bool_Val b -> string_of_bool b

(*Prints the current environment into a readable format.*)
let print_env (env : (string * value) list) =
  List.iter (fun (id, v) ->
    print_string (id ^ " = " ^ string_of_value v);
    print_newline ()
  ) env

(* Print an integer *)
let print_output_int (n : int) : unit =
  print_string (string_of_int n)

(* Print a boolean *)
let print_output_bool (b : bool) : unit =
  print_string (string_of_bool b)

(* Print a newline *)
let print_output_newline () : unit =
  print_newline ()