(* 
  Description:
   This file defines a set of types and functions to manage a 
   simple database of people, including the ability to query, 
   insert, remove, update, sort, and apply conditions to the  
   database. The database operations work with a custom 'person' 
   type, and the 'condition' type allows for flexible querying 
   based on various conditions such as age, name, and hobbies.  
   The conditions can also be combined using logical operators.
   @author Griffin Shay
*)

(*Type definition for a 'person' which holds the details of a person (name, age, hobbies).*)
type person = { name : string; age : int; hobbies : string list }

(*Comparator function which compares two people.*)
type comparator = person -> person -> int

(* 
  The 'condition' type represents a flexible query condition that can be:
   - True: always evaluates to true.
   - False: always evaluates to false.
   - Age: checks a condition on the person's age.
   - Name: checks a condition on the person's name.
   - Hobbies: checks a condition on the person's hobbies.
   - And: logical AND of two conditions.
   - Or: logical OR of two conditions.
   - Not: logical negation of a condition.
   - If: conditional that evaluates one of two conditions based on another condition.
*)
type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

(* 
  The eval_condition function evaluates a given condition for a person.
   It recursively evaluates conditions, supporting basic ones like
   'True', 'False', 'Age', 'Name', and 'Hobbies', as well as compound
   conditions like 'And', 'Or', 'Not', and 'If'.
*)
let rec eval_condition cond p =
  match cond with
  | True -> true
  | False -> false
  | Age f -> f p.age
  | Name f -> f p.name
  | Hobbies f -> f p.hobbies
  | And (c1, c2) -> eval_condition c1 p && eval_condition c2 p
  | Or (c1, c2) -> eval_condition c1 p || eval_condition c2 p
  | Not c -> not (eval_condition c p)
  | If (c1, c2, c3) ->
      if eval_condition c1 p then eval_condition c2 p
      else eval_condition c3 p
  
(* TODO: Implement functions below *)

(*Type definition for 'db' as a list of people.*)
type db = person list

(*Initializes an empty database.*)
let newDatabase = ([]:db)

(*Insert a new person into the database at the head of the list.*)
let insert person db = person::db;;

(*Remove a person by name from the database.*)
let remove name db = List.filter (fun x -> x.name <> name) db;;

(*Sort the database using a comparator function.*)
let sort comparator db = List.sort comparator db;;

(*Given a condition, filter the database and return a list of people who satisfy the condition.*)
let query condition db = List.filter (fun p -> eval_condition condition p) db;;

(*Similar to 'query', but also sorts the resulting filtered list using the provided comparator.*)
let queryBy condition db comparator = List.sort comparator (List.filter condition db);;

(*Given a condition and a change function, update all people in the database who satisfy the condition.*)
let update condition db change = List.map (fun p -> if condition p then change p else p) db;;

(*Given a condition, remove all people from the database who satisfy the condition.*)
let deleteAll condition db = List.filter (fun p -> not(eval_condition condition p)) db;;
