(*  
    Description:
        1. Basic Operations: Functions like `abs`, `rev_tup`, and `is_even` 
            that provide basic utilities for common operations.
        2. Mathematical Functions: Functions such as `fibonacci`, `pow`, `log`, 
            and `gcf` that perform common mathematical operations.
        3. List Manipulation: A collection of functions for working with lists, 
            including `reverse`, `zip`, `merge`, `is_present`, `every_nth`, and others.
        4. Higher-Order Functions: Includes functions like `max_func_chain`, `is_there`, 
            `count_occ`, `uniq`, and `every_xth`, providing ways to manipulate and 
            filter lists with advanced techniques.
    @author Griffin Shay
*)

open List 

(**************************)
(* Implementation: Part 1 *)
(**************************)

(*abs: Returns the absolute value of the integer.*)
let abs x =
    if x < 0 then -x else x
;;

(*rev_tup: Reverses the order of elements in a 3-element tuple.*)
let rev_tup tup =
    match tup with (x, y, z) -> (z, y, x);;
;;

(*is_even: Checks if the integer is even and returns true or false.*)
let is_even x =
    if x mod 2 = 0 then true else false
;;

(*area: Computes the area of a rectangle given two corner points.*)
let area (p:int*int) (q:int*int) =
    let (x1, y1) = p in
    let (x2, y2) = q in
    let x = abs (x2 - x1) in
    let y = abs (y2 - y1) in
    x * y
;;

(**************************)
(* Implementation: Part 2 *)
(**************************)

(*fibonacci: Returns the n-th Fibonacci number using tail recursion.*)
let fibonacci n =
    let rec helper n x y = 
        if n = 0 then x
        else helper (n - 1) y (x + y)
    in
    helper n 0 1
;;

(*pow: Computes the power of a number x raised to the p power using tail recursion.*)
let pow x p =
    let rec helper x p y =
        if (p = 0) then y else
            let p = p - 1 in
            let y = x * y in
            helper x p y in
    helper x p 1
;;

(*log: Computes the base-x logarithm of y using integer division.*)
let log x y =
    let rec helper x y z = 
        if y = 1 || y < x then z else
            let y = y / x in
            let z = z + 1 in
            helper x y z in
    helper x y 0
;;

(*gcf: Computes the greatest common factor of two numbers.*)
let gcf x y = 
    let z = max x y in
    if x = 0 && y = 0 then 0 else
    let rec helper x y z =
        if x mod z = 0 && y mod z = 0 then z else
            let z = z-1 in
            helper x y z in
    helper x y z
;;

(**************************)
(* Implementation: Part 3 *)
(**************************)

(*reverse: Reverses the elements of a list using recursion.*)
let rec reverse lst =
    match lst with
    [] -> []
    |h::t -> (reverse t) @ [h]
;;

(*zip: Combines two lists of tuples into a new list with four elements per tuple.*)
let rec zip (lst1: ('a * 'b) list) (lst2: ('c * 'd) list) : ('a * 'b * 'c * 'd) list =
    match (lst1,lst2) with
	|[],[] -> []
	|[],_ -> []
	|_,[] -> []
	|(h1,h2)::t1, (h3,h4)::t2 -> (h1,h2,h3,h4) :: zip t1 t2
;;

(*insert: Inserts an element into a sorted list such that the list remains sorted.*)
let rec insert x lst = 
    match lst with
    |[] -> [x]  
    |h::t -> if x < h then x::h::t else h::insert x t 
;;

(*merge: Merges two sorted lists into a single sorted list.*)
let merge lst1 lst2 = 
    let x = lst1 @ lst2 in
    let rec helper lst =
      match lst with
      |[] -> []
      |h::t -> insert h (helper t)
    in
    helper x
;;

(*is_present: Checks if an element exists in a list.*)
let rec is_present lst x =
    match lst with
    [] -> false
    |h::t -> h=x || is_present t x
;;

(*every_nth: Returns a list of elements that are in positions that are multiples of n.*)
let every_nth n lst = 
    let rec helper i lst =
        match lst with
        |[] -> []
        |h::t -> if i mod n = 0 then h::helper (i+1) t else helper (i+1) t 
    in
    helper 1 lst
;;

(*jumping_tuples: Combines elements from two lists of tuples in a specified interwoven manner.*)
let jumping_tuples (lst1: ('a*'a) list) (lst2: ('a*'a) list) : 'a list =
    let rec helper l1 l2 i first second =
      match (l1, l2) with
      |[],[] -> reverse(first @ second)
      |(a,_)::t1,(_,x)::t2 ->
        if i mod 2 = 0 then
          helper t1 t2 (i+1) (a::first) (x::second)
        else
          helper t1 t2 (i+1) (x::first) (a::second)
      |_ -> failwith "lists must be same length"
    in
    helper lst1 lst2 0 [] []
;;

(*max_func_chain: Applies a chain of functions on an initial value and returns the maximum result.*)
let max_func_chain init funcs =
    let rec helper x max init funcs =
      match funcs with
      |[] -> max
      |h::t -> if (h x) > (h max) then helper x (h x) (h init) t else
        if (h init) > (h max) then helper x (h init) (h init) t else 
        if (h max) > max then helper x (h max) (h init) t else helper x max (h init) t
    in
    helper init init init funcs
;;

(**************************)
(* Implementation: Part 4 *)
(**************************)

(*is_there: Returns true if an element is present in a list.*)
let is_there lst x =
    fold_left (fun a h -> a || (h=x)) false lst
;;

(*count_occ: Counts the occurences of a specific element in a list.*)
let count_occ lst target =
    fold_left (fun count h -> if target = h then count +1 else count) 0 lst
;;

(*uniq: Removes duplicates from a list.*)
let uniq lst =
    fold_left (fun a h -> if (is_there a h) = true then a else h::a) [] lst
;;

(*every_xth: Returns a list of elements at positions that are multiples of x.*)
let every_xth x lst =
    let (_, lst_xth) =
    fold_left (fun (i, lst) h -> if i mod x = 0 then (i + 1, lst@[h]) else (i+1, lst)) (1, []) lst
    in lst_xth
;;
