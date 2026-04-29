(*
  Description: This file implements an interpreter for the SmallC language.
  It evaluates expressions and executes statements using an environment
  that maps variable names to runtime values.

  The interpreter supports:
  - Arithmetic and boolean expressions
  - Variable declaration and assignment
  - Control flow (if, while, for)
  - Printing values
  - Runtime error handling (type errors, undeclared variables, divide by zero)
  @author Griffin Shay
*)


open SmallCTypes
open Utils
open TokenTypes

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

(*Evaluate an expression in a given environment.*)
let rec eval_expr env t =
  match t with
  (*Literal values.*)
  | Int n -> Int_Val n
  | Bool b -> Bool_Val b

  (*Variable lookup.*)
  | ID x ->
      (try
        List.assoc x env
      with Not_found ->
        raise (DeclareError ("Variable " ^ x ^ " not declared")))
  
  (*Arithematic operations.*)
  | Add (e1, e2) ->
      (match eval_expr env e1, eval_expr env e2 with
      | Int_Val v1, Int_Val v2 -> Int_Val (v1 + v2)
      | _ -> raise (TypeError "Add expects two integers"))

  | Sub (e1, e2) ->
      (match eval_expr env e1, eval_expr env e2 with
      | Int_Val v1, Int_Val v2 -> Int_Val (v1 - v2)
      | _ -> raise (TypeError "Sub expects two integers"))

  | Mult (e1, e2) ->
      (match eval_expr env e1, eval_expr env e2 with
      | Int_Val v1, Int_Val v2 -> Int_Val (v1 * v2)
      | _ -> raise (TypeError "Mult expects two integers"))

  | Div (e1, e2) ->
      (match eval_expr env e1, eval_expr env e2 with
      | Int_Val _, Int_Val 0 -> raise DivByZeroError
      | Int_Val v1, Int_Val v2 -> Int_Val (v1 / v2)
      | _ -> raise (TypeError "Div expects two integers"))

  | Pow (e1, e2) ->
      (match eval_expr env e1, eval_expr env e2 with
      | Int_Val base, Int_Val exp when exp >= 0 -> 
        Int_Val (int_of_float (floor (float_of_int base ** float_of_int exp)))
      | Int_Val _, Int_Val _ -> raise (TypeError "Pow expects non-negative exponent")
      | _ -> raise (TypeError "Pow expects two integers"))
  
  (*Logical OR with short-circuiting.*)
  | Or (e1, e2) ->
      (match eval_expr env e1 with
      | Bool_Val true -> Bool_Val true
      | Bool_Val false ->
        (match eval_expr env e2 with
        | Bool_Val b2 -> Bool_Val b2
        | _ -> raise (TypeError "Or expects two booleans"))
      | _ -> raise (TypeError "Or expects two booleans"))
  
  (*Logical AND with short-circuiting.*)
  | And (e1, e2) ->
      (match eval_expr env e1 with
       | Bool_Val false -> Bool_Val false
       | Bool_Val true ->
        (match eval_expr env e2 with
        | Bool_Val b2 -> Bool_Val b2
        | _ -> raise (TypeError "And expects two booleans"))
       | _ -> raise (TypeError "And expects two booleans"))

  (*Logical NOT.*)
  | Not e ->
      (match eval_expr env e with
       | Bool_Val b -> Bool_Val (not b)
       | _ -> raise (TypeError "Not expects a boolean"))

  (*Comparison operations.*)
  | Greater (e1, e2) ->
      (match eval_expr env e1, eval_expr env e2 with
       | Int_Val v1, Int_Val v2 -> Bool_Val (v1 > v2)
       | _ -> raise (TypeError "Greater expects two integers"))

  | Less (e1, e2) ->
      (match eval_expr env e1, eval_expr env e2 with
       | Int_Val v1, Int_Val v2 -> Bool_Val (v1 < v2)
       | _ -> raise (TypeError "Less expects two integers"))

  | GreaterEqual (e1, e2) ->
      (match eval_expr env e1, eval_expr env e2 with
       | Int_Val v1, Int_Val v2 -> Bool_Val (v1 >= v2)
       | _ -> raise (TypeError "GreaterEqual expects two integers"))

  | LessEqual (e1, e2) ->
      (match eval_expr env e1, eval_expr env e2 with
       | Int_Val v1, Int_Val v2 -> Bool_Val (v1 <= v2)
       | _ -> raise (TypeError "LessEqual expects two integers"))

  (*Type-safe equality checks.*)
  | Equal (e1, e2) ->
      (match eval_expr env e1, eval_expr env e2 with
       | Int_Val v1, Int_Val v2 -> Bool_Val (v1 = v2)
       | Bool_Val b1, Bool_Val b2 -> Bool_Val (b1 = b2)
       | _ -> raise (TypeError "Equal expects two values of the same type"))

  | NotEqual (e1, e2) ->
      (match eval_expr env e1, eval_expr env e2 with
       | Int_Val v1, Int_Val v2 -> Bool_Val (v1 <> v2)
       | Bool_Val b1, Bool_Val b2 -> Bool_Val (b1 <> b2)
       | _ -> raise (TypeError "NotEqual expects two values of the same type"))
;;


(*Execute a statement and return the updated environment.*)
let rec eval_stmt env s =
  match s with
  (*Do nothing.*)
  | NoOp ->
      env

  (*Execute statements sequentially.*)
  | Seq (s1, s2) ->
      let env' = eval_stmt env s1 in
      eval_stmt env' s2

  (*Declare a new variable with default values.*)
  | Declare (typ, id) ->
      if List.mem_assoc id env then
        raise (DeclareError ("Variable " ^ id ^ " already declared"))
      else
        let init_val =
          match typ with
          | Int_Type -> Int_Val 0
          | Bool_Type -> Bool_Val false
        in
        (id, init_val) :: env

  (*Assign a value to an existing variable.*)
  | Assign (id, expr) ->
      if not (List.mem_assoc id env) then
        raise (DeclareError ("Undeclared variable " ^ id))
      else
        let new_val = eval_expr env expr in
        let old_val = List.assoc id env in
        let type_match =
          match old_val, new_val with
          | Int_Val _, Int_Val _ -> true
          | Bool_Val _, Bool_Val _ -> true
          | _ -> false
        in
        if not type_match then
          raise (TypeError ("Type mismatch in assignment to " ^ id))
        else
          (id, new_val) :: List.filter (fun (x, _) -> x <> id) env

  (*Control flow, conditional execution and loops (if, while, for).*)
  | If (guard, s_then, s_else) ->
      let g_val = eval_expr env guard in
      (match g_val with
       | Bool_Val true -> eval_stmt env s_then
       | Bool_Val false -> eval_stmt env s_else
       | _ -> raise (TypeError "If guard must be boolean"))

  | While (guard, body) ->
      let rec loop env_current =
        let g_val = eval_expr env_current guard in
        match g_val with
        | Bool_Val true ->
            let env_after_body = eval_stmt env_current body in
            loop env_after_body
        | Bool_Val false ->
            env_current
        | _ -> raise (TypeError "While guard must be boolean")
      in
      loop env

  | For (id, start_expr, end_expr, body) ->
    if not(List.mem_assoc id env) then raise (DeclareError ("Undeclared variable " ^ id))
    else
      let start_val = eval_expr env start_expr in
      let end_val = eval_expr env end_expr in
      (match start_val, end_val with
      | Int_Val s_val, Int_Val e_val ->
          let env1 = (id, Int_Val s_val) :: List.filter (fun (x, _) -> x <> id) env in
          let rec for_loop env_current =
            let i =
              match List.assoc id env_current with
              | Int_Val n -> n
              | _ -> raise (TypeError "Index must be an integer")
            in
            if (s_val <= e_val && i > e_val) || (s_val > e_val && i < e_val)
            then env_current
            else
            let env_body = eval_stmt env_current body in
            let next_i = if (s_val <= e_val) then i + 1 else i - 1 in
            let env_after_body = (id, Int_Val next_i) :: List.filter (fun (x, _) -> x <> id) env_body in
            for_loop env_after_body
          in
          for_loop env1
      | _ -> raise (TypeError "For loop bounds must be integers"))

  (*Evaluate and print result.*)
  | Print expr ->
      let v = eval_expr env expr in
      (match v with
       | Int_Val n ->
           print_output_int n;
           print_output_newline ()
       | Bool_Val b ->
           print_output_bool b;
           print_output_newline ());
      env
;;
