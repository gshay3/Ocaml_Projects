(* Converts a string into a list of characters *)
let explode (s : string) : char list =
  List.init (String.length s) (String.get s)

(* ========= Types ========= *)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(* ========= Tokenizer ========= *)

type token =
  | Tok_Char of char
  | Tok_Union      (* | *)
  | Tok_Star       (* * *)
  | Tok_LParen     (* ( *)
  | Tok_RParen     (* ) *)

let tokenize (s : string) : token list =
  let rec aux i acc =
    if i >= String.length s then List.rev acc
    else
      match s.[i] with
      | '|' -> aux (i+1) (Tok_Union :: acc)
      | '*' -> aux (i+1) (Tok_Star :: acc)
      | '(' -> aux (i+1) (Tok_LParen :: acc)
      | ')' -> aux (i+1) (Tok_RParen :: acc)
      | c   -> aux (i+1) (Tok_Char c :: acc)
  in
  aux 0 []

(* ========= Recursive Descent Parser ========= *)

(* Grammar (standard):
   expr   := term ('|' expr)?
   term   := factor term | factor
   factor := base '*' | base
   base   := char | '(' expr ')'
*)

exception ParseError

let rec parse_expr tokens =
  let (t1, rest) = parse_term tokens in
  match rest with
  | Tok_Union :: rest' ->
      let (t2, rest'') = parse_expr rest' in
      (Union (t1, t2), rest'')
  | _ -> (t1, rest)

and parse_term tokens =
  let rec aux acc tokens =
    match tokens with
    | [] -> (acc, [])
    | Tok_RParen :: _ -> (acc, tokens)
    | Tok_Union :: _ -> (acc, tokens)
    | _ ->
        let (f, rest) = parse_factor tokens in
        let new_acc =
          match acc with
          | Empty_String -> f
          | _ -> Concat (acc, f)
        in
        aux new_acc rest
  in
  match tokens with
  | [] -> (Empty_String, [])
  | _ ->
      let (f, rest) = parse_factor tokens in
      aux f rest

and parse_factor tokens =
  let (b, rest) = parse_base tokens in
  match rest with
  | Tok_Star :: rest' -> (Star b, rest')
  | _ -> (b, rest)

and parse_base tokens =
  match tokens with
  | [] -> raise ParseError
  | Tok_Char c :: rest -> (Char c, rest)
  | Tok_LParen :: rest ->
      let (e, rest') = parse_expr rest in
      (match rest' with
       | Tok_RParen :: rest'' -> (e, rest'')
       | _ -> raise ParseError)
  | _ -> raise ParseError

let parse_regexp tokens =
  match tokens with
  | [] -> Empty_String
  | _ ->
      let (ast, rest) = parse_expr tokens in
      match rest with
      | [] -> ast
      | _ -> raise ParseError

(* ========= Provided Helpers ========= *)

let string_to_regexp str =
  parse_regexp (tokenize str)
