open List
open Nfa
open Utils

(*********)
(* Types *)
(*********)

(* 
  from utils.ml, for your reference:

  type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t
*)

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

let regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t = 
  let shift_nfa (nfa: ('q,'s) nfa_t) (offset: int) : (int, char) nfa_t =
    {
      sigma = nfa.sigma;
      qs = List.map (fun q -> q + offset) nfa.qs;
      q0 = nfa.q0 + offset;
      fs = List.map (fun q -> q + offset) nfa.fs;
      delta = List.map (fun tr ->
        { input = tr.input;
          states = (fst tr.states + offset, snd tr.states + offset) }
      ) nfa.delta
    } in
  let rec helper regex = 
    match regex with
    |Empty_String ->
      {
        sigma = [];
        qs = [0];
        q0 = 0;
        fs = [0];
        delta = []
      }
    |Char x ->
      {
        sigma = [x];
        qs = [0;1];
        q0 = 0;
        fs = [1];
        delta = [{input = Some x; states = (0,1)}]
      }
    |Union (r1,r2)->
      let nfa1 = helper r1 in
      let nfa2 = helper r2 in
      let offset = List.fold_left max (-1) nfa1.qs + 1 in
      let nfa2_shift = shift_nfa nfa2 offset in
      let z0 = List.fold_left max (-1) (nfa1.qs @ nfa2_shift.qs) + 1 in
      let z1 = z0 + 1 in
      {
        sigma = List.fold_left (fun acc x -> if List.mem x acc then acc else acc @ [x]) [] (nfa1.sigma @ nfa2.sigma);
        qs = [z0] @ (nfa1.qs @ nfa2_shift.qs) @ [z1];
        q0 = z0;
        fs = [z1];
        delta = [{input = None; states = (z0,nfa1.q0)};
                  {input = None; states = (z0,nfa2_shift.q0)}]
                  @ nfa1.delta @ nfa2_shift.delta
                  @ (List.map (fun q -> {input = None; states = (q,z1)}) nfa1.fs)
                  @ (List.map (fun q -> {input = None; states = (q,z1)}) nfa2_shift.fs)
      }
    |Concat (r1,r2) ->
      let nfa1 = helper r1 in
      let nfa2 = helper r2 in
      let offset = List.fold_left max (-1) nfa1.qs + 1 in
      let nfa2_shift = shift_nfa nfa2 offset in
      {
        sigma = List.fold_left (fun acc x -> if List.mem x acc then acc else acc @ [x]) [] (nfa1.sigma @ nfa2.sigma);
        qs = nfa1.qs @ nfa2_shift.qs;
        q0 = nfa1.q0;
        fs = nfa2_shift.fs;
        delta = nfa1.delta @ (List.map (fun q -> {input = None; states = (q,nfa2_shift.q0)}) nfa1.fs)
                  @ nfa2_shift.delta
      }
    |Star r ->
      let nfa = helper r in
      let z0 = List.fold_left max (-1) nfa.qs + 1 in
      let z1 = z0 + 1 in
      {
        sigma = nfa.sigma;
        qs = [z0] @ nfa.qs @ [z1];
        q0 = z0;
        fs = [z1];
        delta = [{input = None; states = (z0,nfa.q0)};
                  {input = None; states = (z0,z1)}]
                  @ nfa.delta
                  @ (List.map (fun q -> {input = None; states = (q,z1)}) nfa.fs)
                  @ [{input = None; states = (z1,z0)}]
      }
  in
  helper regexp
;;

(* The following functions are useful for testing, we have implemented them for you *)
let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str