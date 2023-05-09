(* Hashmap *)

let is_empty (lst : ('k * 'v) list) : bool=
match lst with 
| [] -> true
| _ -> false

let rec get (key : 'k) (lst : ('k * 'v) list) : 'v option=
match lst with 
| [] -> None
| (k, v) :: xs -> if k=key then Some v else get key xs 



let put (key : 'k) (value : 'v) (lst : ('k * 'v) list) : ('k * 'v) list=
(key, value) :: List.filter (fun (k, v) -> k <> key) lst


let contains_key (key : 'k) (lst : ('k * 'v) list) : bool=
List.fold_left (fun acc (k, v) -> if k=key then true else acc) false lst

let remove (key : 'k) (lst : ('k * 'v) list) : ('k * 'v) list=
List.filter (fun (k, v) -> k <> key) lst

let keys (lst : ('k * 'v) list) : 'k list=
List.map (fun (k, v) -> k) lst


let values (lst : ('k * 'v) list) : 'v list=
List.map (fun (k, v) -> v) lst




let l1 = [(1, 2);(2, 5);(3, 7);(4, 9);(5, 0)]



(* Mapping for asscoative lists (fun x -> )*)

type ('k, 'v) mapping = 'k -> 'v option

let get k m = m k

let put k v m = fun k' -> if k=k' then Some v else m k'
let contains_key k m = m k <> None
let remove k m = fun k' -> if k=k' then None else m k'