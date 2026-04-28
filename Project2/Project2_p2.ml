(* 
  Description: Defines a polymorphic binary tree with operations like map, mirror, depth, and trimming.
  Includes tree manipulation functions using fold and helpers for tree initialization and traversal.
  @author Griffin Shay
*)

type 'a tree =
  | Node of 'a tree * 'a * 'a tree (* Node with left subtree, value, and right subtree *)
  | Leaf                            (* Leaf represents an empty tree *)

(*Recursively processes a tree with a folding function and an accumulator.*)
let rec tree_fold f init tree =
  match tree with
  | Leaf -> init
  | Node (l, v, r) ->
      let left_result = tree_fold f init l in
      let right_result = tree_fold f init r in
      f left_result v right_result
  
(*Transforms a tree by applying function 'f' to each node's value.*)
let map tree f = 
  tree_fold (fun l v r -> Node(l, f v, r)) Leaf tree
;;

(*Returns a mirrored tree (swap left and right subtrees).*)
let mirror tree = 
  tree_fold (fun l v r -> Node(r, v, l)) Leaf tree
;;

(*Traverses the tree in-order and collects values in a list.*)
let in_order tree = 
  tree_fold (fun l v r -> l @ [v] @ r) [] tree
;;

(*Traverses the tree pre-order and collects values in a list.*)
let pre_order tree = 
  tree_fold (fun l v r -> [v] @ l @ r) [] tree
;;

(*Applies composed functions in the tree structure.*)
let compose tree = 
  tree_fold (fun l v r x -> r(l(v x))) (fun x -> x) tree
;;

(*Computes the maximum depth of the tree.*)
let depth tree = 
  tree_fold (fun l _ r -> 1 + max l r) 0 tree
;;

(* Assumes complete tree *)
(*Trims the tree to depth 'n', replacing deeper nodes with Leaf.*)
let trim tree n = 
  let leaf_fn = fun _ -> Leaf in
  let node_fn l_fn v r_fn =
    fun d -> if d = n then Node(Leaf, v, Leaf)
      else Node(l_fn (d + 1), v, r_fn (d + 1))
  in
  let f = tree_fold node_fn leaf_fn tree in
  f 0
;;

(*Builds a tree by applying function 'f' to a value of 'v'.*)
let rec tree_init f v = 
  match (f v) with
  | None -> Leaf
  | Some (v_l, x, v_r) -> Node(tree_init f v_l, x, tree_init f v_r)
;; 

(*Splits the list at the first occurence of 'v'.*)
let rec split lst v = 
  let rec helper lst acc =
    match lst with
    | [] -> (List.rev acc, [])
    | h::t -> if h = v then (List.rev acc, t) else helper t (h::acc)
  in
  helper lst []
;;

(*Splits the list at position 'n'.*)
let rec split_at n lst =
  if n <= 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | x :: xs -> 
        let (first, rest) = split_at (n - 1) xs in
        (x :: first, rest)
;;

(*Reconstructs a tree from pre-order and in-order traversals.*)
let rec from_pre_in pre in_ord = 
  match (pre, in_ord) with
  | [],[] -> Leaf
  | [],_ -> Leaf 
  | h::t, _ -> 
    let (l,r) = split in_ord h in
    let l_len = List.length l in
    let l_pre, r_pre = split_at l_len t in
    Node (from_pre_in l_pre l, h, from_pre_in r_pre r)
;;
