

(* Data type for binary trees*)
type tree=
| Empty 
| Node of int*tree*tree


(*binary tree t1 containing the values 1,6,8,9,12,42*)

let t1= Node (1, Empty, Node (6, Empty , Node (8, Node (9, Empty, Empty), Node (12, Empty, Node (42, Empty, Empty)))))

let rec to_list (t : tree) : int list=
match t with
| Empty -> []
| Node (value, left, right) -> 
  (to_list left) @ [value] @ (to_list right) 


let rec insert (value : int) (t : tree) : tree=
match t with
| Empty -> Node (value, Empty, Empty)
| Node (v, left, right) when value=v -> t
| Node (v, left, right) when value<v -> 
  Node (v, insert value left, right)
| Node (v, left, right) when value>v -> 
  Node (v, left, insert value right) 


let list_to_tree (lst : int list) : tree=
List.fold_left (fun acc x -> insert x acc) Empty lst


let remove (value : int) (t : tree) : tree=
let tree_in_list= List.filter (fun v -> v<>value) (to_list t) in
list_to_tree (tree_in_list)
