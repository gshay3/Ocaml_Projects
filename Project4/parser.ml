(*
  Descrption: This file implements a parser for the SmallC language using
  recursive descent parsing. It consumes a list of tokens and
  produces an abstract syntax tree (AST) consisting of expressions
  and statements.

  The parser respects operator precedence and associativity by
  structuring expression parsing into multiple levels (e.g., or, and,
  equality, relational, arithmetic). It also handles statements such
  as declarations, assignments, conditionals, loops, and blocks.
  @author Griffin Shay
*)


open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

(*Entry point for expression parsing.*)
let rec parse_expr toks : expr_result =
  let (remaining, expr) = parse_or toks in
  (remaining, expr)

(*Parse logical OR expressions.*)
and parse_or toks =
  let (tokens_after_and, left) = parse_and toks in
  match lookahead tokens_after_and with
  | Tok_Or ->
    let tokens_next = match_token tokens_after_and Tok_Or in
    let (tokens_final, right) = parse_or tokens_next in
    (tokens_final, Or(left, right))
  | _ -> (tokens_after_and, left)

(*Parse logical AND expressions.*)
and parse_and toks =
  let (tokens_after_eq, left) = parse_eq toks in
  match lookahead tokens_after_eq with
  | Tok_And ->
    let tokens_next = match_token tokens_after_eq Tok_And in
    let (tokens_final, right) = parse_and tokens_next in
    (tokens_final, And(left, right))
  | _ -> (tokens_after_eq, left)

(*Parse equality (==, !=).*)
and parse_eq toks =
  let (tokens_after_rel, left) = parse_rel toks in
  match lookahead tokens_after_rel with
  | Tok_Equal ->
    let tokens_next = match_token tokens_after_rel Tok_Equal in
    let (tokens_final, right) = parse_eq tokens_next in
    (tokens_final, Equal(left, right))
  | Tok_NotEqual ->
    let tokens_next = match_token tokens_after_rel Tok_NotEqual in
    let (tokens_final, right) = parse_eq tokens_next in
    (tokens_final, NotEqual(left, right))
  | _ -> (tokens_after_rel, left)

(*Parse comparison operations*)
and parse_rel toks =
  let (tokens_after_add, left) = parse_add toks in
  match lookahead tokens_after_add with
  | Tok_Less ->
    let rest = match_token tokens_after_add Tok_Less in
    let (tokens_final, right) = parse_rel rest in
    (tokens_final, Less(left, right))
  | Tok_Greater ->
    let rest = match_token tokens_after_add Tok_Greater in
    let (tokens_final, right) = parse_rel rest in
    (tokens_final, Greater(left, right))
  | Tok_LessEqual ->
    let rest = match_token tokens_after_add Tok_LessEqual in
    let (tokens_final, right) = parse_rel rest in
    (tokens_final, LessEqual(left, right))
  | Tok_GreaterEqual ->
    let rest = match_token tokens_after_add Tok_GreaterEqual in
    let (tokens_final, right) = parse_rel rest in
    (tokens_final, GreaterEqual(left, right))
  | _ -> (tokens_after_add, left)

(*Parse addition and subtraction.*)
and parse_add toks =
  let (tokens_after_mult, left) = parse_mult toks in
  match lookahead tokens_after_mult with
  | Tok_Add ->
    let rest = match_token tokens_after_mult Tok_Add in
    let (tokens_final, right) = parse_add rest in
    (tokens_final, Add(left, right))
  | Tok_Sub ->
    let rest = match_token tokens_after_mult Tok_Sub in
    let (tokens_final, right) = parse_add rest in
    (tokens_final, Sub(left, right))
  | _ -> (tokens_after_mult, left)

(*Parse mulitplication and division.*)
and parse_mult toks =
  let (tokens_after_pow, left) = parse_pow toks in
  match lookahead tokens_after_pow with
  | Tok_Mult ->
    let rest = match_token tokens_after_pow Tok_Mult in
    let (tokens_final, right) = parse_mult rest in
    (tokens_final, Mult(left, right))
  | Tok_Div ->
    let rest = match_token tokens_after_pow Tok_Div in
    let (tokens_final, right) = parse_mult rest in
    (tokens_final, Div(left, right))
  | _ -> (tokens_after_pow, left)

(*Parse exponentiation (right-associative).*)
and parse_pow toks =
  let (tokens_after_un, left) = parse_unary toks in
  match lookahead tokens_after_un with
  | Tok_Pow ->
    let rest = match_token tokens_after_un Tok_Pow in
    let (tokens_final, right) = parse_pow rest in
    (tokens_final, Pow(left, right))
  | _ -> (tokens_after_un, left)

(*Parse unary operations.*)
and parse_unary toks =
  match lookahead toks with
  | Tok_Not ->
    let rest = match_token toks Tok_Not in
    let (tokens_final, expr) = parse_unary rest in
    (tokens_final, Not(expr))
  | _ -> parse_primary toks

(*Parse literals, identifiers, and parenthesized expressions.*)
and parse_primary toks =
  match lookahead toks with
  | Tok_Int i ->
    let rest = match_token toks (Tok_Int i) in
    (rest, Int i)
  | Tok_Bool b ->
    let rest = match_token toks (Tok_Bool b) in
    (rest, Bool b)
  | Tok_ID id ->
    let rest = match_token toks (Tok_ID id) in
    (rest, ID id)
  | Tok_LParen ->
    let after_lparen = match_token toks Tok_LParen in
    let (after_expr, inner) = parse_expr after_lparen in
    let final = match_token after_expr Tok_RParen in
    (final, inner)
  | _ -> raise (InvalidInputException "Invalid primary expression")

(*Parse sequence of statements.*)
let rec parse_stmt toks : stmt_result =
  match lookahead toks with
  | Tok_RBrace -> (toks, NoOp)
  | EOF -> (toks, NoOp)
  | Tok_LBrace -> parse_block toks
  | Tok_Int_Type 
  | Tok_Bool_Type
  | Tok_ID _ 
  | Tok_Print 
  | Tok_If 
  | Tok_For 
  | Tok_While ->
    let (after_one, st) = parse_single_stmt toks in
      begin match lookahead after_one with
      | Tok_Int_Type 
      | Tok_Bool_Type
      | Tok_ID _ 
      | Tok_Print 
      | Tok_If 
      | Tok_For 
      | Tok_While 
      | Tok_LBrace ->
        let (after_seq, rest) = parse_stmt after_one in
        (after_seq, Seq(st, rest))
      | _ -> (after_one, Seq(st, NoOp))
      end
    | _ -> raise (InvalidInputException "Invalid start statement")

(*Dispatch to specific statement parsers.*)
and parse_single_stmt toks =
  match lookahead toks with
  | Tok_Int_Type | Tok_Bool_Type -> parse_declaration toks
  | Tok_ID _ -> parse_assignment toks
  | Tok_Print -> parse_print toks
  | Tok_If -> parse_if toks
  | Tok_For -> parse_for toks
  | Tok_While -> parse_while toks
  | _ -> raise (InvalidInputException "Invalid start of statement")

(*Parse variable declarations.*)
and parse_declaration toks =
  match lookahead toks with
  | Tok_Int_Type ->
    let after_type = match_token toks Tok_Int_Type in
    (match lookahead after_type with
     | Tok_ID name ->
       let after_id = match_token after_type (Tok_ID name) in
       let final = match_token after_id Tok_Semi in
       (final, Declare(Int_Type, name))
     | _ -> raise (InvalidInputException "Expected ID after int"))
  | Tok_Bool_Type ->
    let after_type = match_token toks Tok_Bool_Type in
    (match lookahead after_type with
     | Tok_ID name ->
       let after_id = match_token after_type (Tok_ID name) in
       let final = match_token after_id Tok_Semi in
       (final, Declare(Bool_Type, name))
     | _ -> raise (InvalidInputException "Expected ID after bool"))
  | _ -> raise (InvalidInputException "Invalid declaration")

(*Parse assignments.*)
and parse_assignment toks =
  match lookahead toks with
  | Tok_ID name ->
    let after_id = match_token toks (Tok_ID name) in
    let after_assign = match_token after_id Tok_Assign in
    let (after_expr, expr_val) = parse_expr after_assign in
    let final = match_token after_expr Tok_Semi in
    (final, Assign(name, expr_val))
  | _ -> raise (InvalidInputException "Expected ID for assignment")

(*Parse print statements.*)
and parse_print toks =
  let after_print = match_token toks Tok_Print in
  let after_lparen = match_token after_print Tok_LParen in
  let (after_expr, expr_val) = parse_expr after_lparen in
  let after_rparen = match_token after_expr Tok_RParen in
  let final = match_token after_rparen Tok_Semi in
  (final, Print(expr_val))

(*Parse if/else statements.*)
and parse_if toks =
  let after_if = match_token toks Tok_If in
  let after_lparen = match_token after_if Tok_LParen in
  let (after_cond, cond) = parse_expr after_lparen in
  let after_rparen = match_token after_cond Tok_RParen in
  let (after_then, then_branch) = parse_block after_rparen in
  if lookahead after_then = Tok_Else then
    let after_else_kw = match_token after_then Tok_Else in
    let (after_else, else_branch) = parse_block after_else_kw in
    (after_else, If(cond, then_branch, else_branch))
  else
    (after_then, If(cond, then_branch, NoOp))

(*Parse a block of statements enclosed in braces.*)
and parse_block toks = 
  let after_lbrace = match_token toks Tok_LBrace in
  let (after_stmts, stmts) = parse_stmt_list after_lbrace in
  let after_rbrace = match_token after_stmts Tok_RBrace in
  let body = List.fold_right (fun s acc -> Seq(s, acc)) stmts NoOp in
  (after_rbrace, body)

(*Parse a list of statements.*)
and parse_stmt_list toks =
  match lookahead toks with
  | Tok_RBrace -> (toks, [])
  | _ -> 
    let (toks', first) = parse_stmt toks in
    let (toks'', rest) = parse_stmt_list toks' in
    (toks'', first :: rest)

(*Parse for loops.*)
and parse_for toks =
  let after_for = match_token toks Tok_For in
  let after_lparen = match_token after_for Tok_LParen in
  match lookahead after_lparen with
  | Tok_ID x -> 
    let after_id = match_token after_lparen (Tok_ID x) in
    let after_from = match_token after_id Tok_From in
    let (after_start, start_e) = parse_expr after_from in
    let after_to = match_token after_start Tok_To in
    let (after_end, end_e) = parse_expr after_to in
    let after_rparen = match_token after_end Tok_RParen in
    let (after_body, body) = parse_block after_rparen in
    (after_body, For(x,start_e,end_e,body))
  | _ -> raise (InvalidInputException "Expected ID in for loop")

(*Parse while loops.*)
and parse_while toks =
  let after_w = match_token toks Tok_While in
  let after_lp = match_token after_w Tok_LParen in
  let (after_cond, cond) = parse_expr after_lp in
  let after_rp = match_token after_cond Tok_RParen in
  let (body_tok, body) = parse_block after_rp in
  (body_tok, While(cond,body))

(*Entry point for parsing a full program.*)
let parse_main toks : stmt =
  let tokens = match_token toks Tok_Int_Type in
  let tokens = match_token tokens Tok_Main in
  let tokens = match_token tokens Tok_LParen in
  let tokens = match_token tokens Tok_RParen in
  let tokens = match_token tokens Tok_LBrace in
  let (tokens, main_body) = parse_stmt tokens in
  let tokens = match_token tokens Tok_RBrace in
  let _ = match_token tokens EOF in
  main_body
;;
