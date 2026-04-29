(*
  This module converts an input string into a list of tokens defined in TokenTypes.

  The lexer processes the input character-by-character and recognizes:
  - integer literals
  - identifiers and reserved keywords
  - operators (single and multi-character)
  - punctuation symbols
  - whitespace (ignored)

  It uses helper functions to scan integers and identifiers, and a recursive
  tokenize function to build the final token stream.
  @author Griffin Shay
*)

open TokenTypes

let is_digit c = '0' <= c && c <= '9'
let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
let digit_or_letter c = is_digit c || is_letter c

(*Scans an integer starting at index i.*)
let scan_int input i = 
  let len = String.length input in
  let neg = if i < len && input.[i] = '-' then i+1 else i in
  let j =
    let rec loop k =
      if k < len && is_digit input.[k] then loop (k+1) else k 
    in
    loop neg
  in
  if j = neg then None
  else
    let txt = String.sub input i (j-i) in
    Some (Tok_Int (int_of_string txt), j)

(*Scans an identifier or keyword starting at index i.*)
let scan_id input i s_table =
  let len = String.length input in
  if i < len && is_letter input.[i] then
    let rec loop k =
      if k < len && digit_or_letter input.[k] then loop (k+1)
      else k 
    in
    let j = loop (i+1) in
    let txt = String.sub input i (j-i) in
    let tok = 
      try List.assoc txt s_table
      with Not_found -> Tok_ID txt
    in 
    Some(tok, j)
  else None

  (*Maps reserved keywords to their corresponding tokens.*)
let s = [
  ("if", Tok_If);
  ("to", Tok_To);
  ("int", Tok_Int_Type);
  ("for",Tok_For);
  ("bool", Tok_Bool_Type);
  ("main", Tok_Main);
  ("else", Tok_Else);
  ("from", Tok_From);
  ("true", Tok_Bool true);
  ("while", Tok_While);
  ("false", Tok_Bool false);
  ("printf", Tok_Print)
]

(*Converts a single character into a token if it is a valid symbol.*)
let single_c c =
  match c with
  | ' ' -> None
  | '\t' -> None
  | '\n' -> None
  | '(' -> Some Tok_LParen
  | ')' -> Some Tok_RParen
  | '{' -> Some Tok_LBrace
  | '}' -> Some Tok_RBrace
  | '=' -> Some Tok_Assign
  | '>' -> Some Tok_Greater
  | '<' -> Some Tok_Less
  | '!' -> Some Tok_Not
  | ';' -> Some Tok_Semi
  | '+' -> Some Tok_Add
  | '-' -> Some Tok_Sub
  | '*' -> Some Tok_Mult
  | '/' -> Some Tok_Div
  | '^' -> Some Tok_Pow
  | _ -> raise (InvalidInputException "Lexer Issue")

(*Tokenizes the full input string into a list of tokens ending with EOF.*)
let tokenize input =
  let len = String.length input in
  let rec helper i tokens =
    if i >= len then List.rev (EOF :: tokens)
    else
      match scan_int input i with
      | Some (tok, j) -> helper j (tok :: tokens)
      | None ->
        (match scan_id input i s with
        | Some (tok, j) -> helper j (tok :: tokens)
        | None -> 
          if i + 1 < String.length input then
          let two = String.sub input i 2 in
          match two with
          | "==" -> helper (i + 2) (Tok_Equal :: tokens)
          | "!=" -> helper (i + 2) (Tok_NotEqual :: tokens)
          | ">=" -> helper (i + 2) (Tok_GreaterEqual :: tokens)
          | "<=" -> helper (i + 2) (Tok_LessEqual :: tokens)
          | "||" -> helper (i + 2) (Tok_Or :: tokens)
          | "&&" -> helper (i + 2) (Tok_And :: tokens)
          | _ ->
            (match single_c input.[i] with
            | Some t -> helper (i + 1) (t :: tokens)
            | None -> helper (i + 1) tokens)
          else
          (match single_c input.[i] with
          | Some t -> helper (i + 1) (t :: tokens)
          | None -> helper (i + 1) tokens))
  in
  helper 0 []
;;
