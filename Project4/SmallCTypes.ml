(*
  This file defines the abstract syntax tree (AST) for the SmallC language.

  It includes definitions for:
  - expressions (arithmetic, boolean, relational, logical)
  - statements (assignments, declarations, control flow, blocks)
  - types (int and bool)
*)

open TokenTypes

(*
  Types supported by variable declarations.
*)
type typ =
  | Int_Type
  | Bool_Type

(* Runtime values (THIS IS WHAT YOU WERE MISSING) *)
type value =
  | Int_Val of int
  | Bool_Val of bool

(* Environment mapping variable names to values *)
type env = (string * value) list

(*
  Expressions in the language.
*)
type expr =
  | Int of int
  | Bool of bool
  | ID of string

  (* arithmetic *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Pow of expr * expr

  (* relational *)
  | Equal of expr * expr
  | NotEqual of expr * expr
  | Less of expr * expr
  | Greater of expr * expr
  | LessEqual of expr * expr
  | GreaterEqual of expr * expr

  (* logical *)
  | And of expr * expr
  | Or of expr * expr
  | Not of expr

(*
  Statements in the language.
*)
type stmt =
  | NoOp

  (* sequencing *)
  | Seq of stmt * stmt

  (* variable declaration *)
  | Declare of typ * string

  (* assignment *)
  | Assign of string * expr

  (* printing *)
  | Print of expr

  (* control flow *)
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of string * expr * expr * stmt