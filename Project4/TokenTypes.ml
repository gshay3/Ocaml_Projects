(*
  Description: This file defines the token type used by the lexer and parser
  @author Griffin Shay
*)

exception InvalidInputException of string

(* Token type definition *)
type token =
  (* literals *)
  | Tok_Int of int
  | Tok_Bool of bool
  | Tok_ID of string

  (* keywords *)
  | Tok_If
  | Tok_To
  | Tok_Int_Type
  | Tok_For
  | Tok_Bool_Type
  | Tok_Main
  | Tok_Else
  | Tok_From
  | Tok_While
  | Tok_Print

  (* punctuation *)
  | Tok_LParen
  | Tok_RParen
  | Tok_LBrace
  | Tok_RBrace
  | Tok_Semi

  (* arithmetic operators *)
  | Tok_Add
  | Tok_Sub
  | Tok_Mult
  | Tok_Div
  | Tok_Pow

  (* comparison operators *)
  | Tok_Assign
  | Tok_Equal
  | Tok_NotEqual
  | Tok_Greater
  | Tok_Less
  | Tok_GreaterEqual
  | Tok_LessEqual

  (* logical operators *)
  | Tok_And
  | Tok_Or
  | Tok_Not

  (* end of input *)
  | EOF


  (* Converts a token into a readable string for debugging and testing *)
let string_of_token = function
  (* literals *)
  | Tok_Int n -> "Tok_Int(" ^ string_of_int n ^ ")"
  | Tok_Bool b -> "Tok_Bool(" ^ string_of_bool b ^ ")"
  | Tok_ID s -> "Tok_ID(" ^ s ^ ")"

  (* keywords *)
  | Tok_If -> "Tok_If"
  | Tok_To -> "Tok_To"
  | Tok_Int_Type -> "Tok_Int_Type"
  | Tok_For -> "Tok_For"
  | Tok_Bool_Type -> "Tok_Bool_Type"
  | Tok_Main -> "Tok_Main"
  | Tok_Else -> "Tok_Else"
  | Tok_From -> "Tok_From"
  | Tok_While -> "Tok_While"
  | Tok_Print -> "Tok_Print"

  (* punctuation *)
  | Tok_LParen -> "Tok_LParen"
  | Tok_RParen -> "Tok_RParen"
  | Tok_LBrace -> "Tok_LBrace"
  | Tok_RBrace -> "Tok_RBrace"
  | Tok_Semi -> "Tok_Semi"

  (* arithmetic operators *)
  | Tok_Add -> "Tok_Add"
  | Tok_Sub -> "Tok_Sub"
  | Tok_Mult -> "Tok_Mult"
  | Tok_Div -> "Tok_Div"
  | Tok_Pow -> "Tok_Pow"

  (* comparison operators *)
  | Tok_Assign -> "Tok_Assign"
  | Tok_Equal -> "Tok_Equal"
  | Tok_NotEqual -> "Tok_NotEqual"
  | Tok_Greater -> "Tok_Greater"
  | Tok_Less -> "Tok_Less"
  | Tok_GreaterEqual -> "Tok_GreaterEqual"
  | Tok_LessEqual -> "Tok_LessEqual"

  (* logical operators *)
  | Tok_And -> "Tok_And"
  | Tok_Or -> "Tok_Or"
  | Tok_Not -> "Tok_Not"

  (* end of input *)
  | EOF -> "EOF"