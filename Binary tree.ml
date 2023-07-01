


(* 1 *)
(* java verification *)


(* 2 *)
(* In this assignment, you are meant to realize a tree data structure supporting indexed
lookups and updates. For that, consider the datapype 'a tree defined by *)

type 'a tree = Leaf of 'a | Node of int * int * 'a tree * 'a tree


(* the data in the tree are stored in the leaves, while each inner node Node (size, lsize,
left, right) maintains together with the two subtrees left and right, two numbers.
The first one, size, equals the number of leaves in the tree, while the second one lsize
records the number of leaves of the left subtree left. An example of such a well-formed
tree is *)

let t = Node (3, 2, Node (2, 1, Leaf "a", Leaf "b"), Leaf "c");;


(* Provide implementations of the following functions
val lookup : 'a tree -> int -> 'a
val update : 'a tree -> int -> 'a -> 'a tree
val insert : 'a tree -> int -> 'a -> 'a tree
val list_of : 'a tree -> 'a list *)

(* lookup t i returns the value contained in the ith leaf where counting starts from
0. E.g., lookup t 1 = "b". *)
let rec lookup t i = 
  match t with 
  | Leaf v -> v
  | Node (size, lsize, t', t'') -> 
    if (size <= i) then failwith "too big index" else
      if (i < lsize) then lookup t' i else lookup t'' (i-lsize)


(* update t i x returns the tree obtained from t by replacing the contents of the ith
leaf with x. Thus, e.g., update t 2 "d" = Node (3, 2, Node (2, 1, Leaf "a", Leaf "b"), Leaf "d"). *)

let rec update t i x = 
  match t with 
  | Leaf v -> if i=0 then Leaf x else failwith "too big index"
  | Node (size, lsize, t', t'') ->
    if (i < lsize) then Node(size, lsize, update t' i x, t'') 
    else Node(size, lsize, t', update t'' (i-lsize) x)


(* insert t i x returns the tree obtained from t by inserting a new subtree Leaf
x as the ith leaf, while all leaves with original positions i, i + 1, ... now will have
positions i+ 1, i+ 2, .... Thus, e.g., insert t 2 "d" = Node (4, 2, Node (2, 1,
Leaf "a", Leaf "b"), Node (2, 1, Leaf "d", Leaf "c")). *)

let rec insert t i x = 
  match t with 
  | Leaf v -> if i=0 then Node(2, 1, Leaf x, Leaf v) else failwith "too big index"
  | Node (size, lsize, tL, tR) ->
    if i<lsize then Node (size+1, lsize+1, insert tL i x, tR) else 
      Node (size+1, lsize, tL, insert tR (i-lsize) x)


(* list_of t returns the list consisting of the elements stored in the leaves of t in
left-to-right order. E.g., list_of t = ["a"; "b"; "c"]. Your function should run
in time linear in the number of leaves of t. *)

let rec list_of t = 
  match t with 
  | Leaf v -> [v]
  | Node (_, _, tL, tR) -> (list_of tL) @ (list_of tR) 
       
        