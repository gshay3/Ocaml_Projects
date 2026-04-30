(*
  NFA and DFA Implementation

  This file defines a nondeterministic finite automaton (NFA) and implements:
  - Basic NFA operations (move, epsilon-closure, acceptance)
  - Conversion from NFA to DFA using subset construction

  The NFA may include epsilon (None) transitions, and the resulting DFA
  represents sets of NFA states.
  @author Griffin Shay
*)

open List
open Utils

(*********)
(* Types *)
(*********)


type ('q, 's) transition = {
  input: 's option; 
  states: 'q * 'q;
}

(** NFA type *)
type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
} 


(****************)
(* Part 1: NFAs *)
(****************)

(*Given a set of states qa and an input symbol s,
  returns all states reachable in one step via transitions labeled s.*)
let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  match s with
  | Some x ->
    if List.mem x nfa.sigma then
     let lst = List.filter  (fun t -> s = t.input && (List.mem (fst t.states) qs)) nfa.delta in
     List.fold_left (fun a h -> (snd h.states)::a) [] lst
    else []
  | None ->
    let lst = List.filter  (fun t -> s = t.input && (List.mem (fst t.states) qs)) nfa.delta in
    List.fold_left (fun a h -> (snd h.states)::a) [] lst 
;;

(*Computes the epsilon closure of a set of states.*)
let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec helper l =
    let lst = List.filter  (fun t -> t.input = None && (List.mem (fst t.states) l)) nfa.delta in
    let new_lst = List.sort_uniq compare (l@(List.fold_left (fun a h -> (snd h.states)::a) [] lst)) in
    if l = new_lst then new_lst else helper new_lst in
  helper qs
;;

(*Determines whether the NFA accepts a given string.*)
let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let start = e_closure nfa [nfa.q0] in
  let final = List.fold_left (fun current x -> let next = move nfa current (Some x) in
    e_closure nfa next) start (explode s) in
  List.exists (fun y -> List.mem y nfa.fs) final
;;

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

(*From a set of NFA states, compute all reachable sets (DFA states) for each symbol in the alphabet.*)
let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let qs = e_closure nfa qs in
  List.map (fun x -> let next = move nfa qs (Some x) in e_closure nfa next) nfa.sigma
;;

(*Generates DFA transitions from a given DFA state (set of NFA states).*)
let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  let qs = e_closure nfa qs in
  List.map (fun x -> let ts = (let next = move nfa qs (Some x) in e_closure nfa next) in {input = Some x; states = (qs, ts)}) nfa.sigma
;;

(*Determines if a DFA state is accepting.*)
let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if List.exists (fun x -> List.mem x nfa.fs) qs then [qs] else []
;;

(*Iteratevly builds the DFA by exploring unprocessed states.*)
let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with
  |[] -> dfa
  |h::t -> let trans = new_trans nfa h in
    let dest = List.fold_left (fun acc tr -> let d = snd tr.states in
      if List.mem d acc then acc else d::acc) [] trans in
    let new_st = List.filter (fun state -> not (List.mem state dfa.qs)) dest in
    let new_qs = List.sort_uniq compare (dfa.qs @ new_st) in
    let new_delta = dfa.delta @ trans in
    let fs' = List.flatten (List.map (fun state -> new_finals nfa state) new_st) in
    let new_fs = List.sort_uniq compare (dfa.fs @ fs') in
    let new_dfa = {dfa with qs = new_qs; delta = new_delta; fs = new_fs} in
    nfa_to_dfa_step nfa new_dfa (t @ new_st)
;;

(*Converts an NFA into an equivalent DFA using subset construction.*)
let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let start = e_closure nfa [nfa.q0] in
  let dfa = {
    sigma = nfa.sigma;
    qs = [start];
    q0 = start;
    fs = new_finals nfa start;
    delta = []
  } in
  nfa_to_dfa_step nfa dfa [start]
;;
