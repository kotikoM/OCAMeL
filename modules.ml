


(* We model sets of values using modules with signature *)

module type Set = sig
  type t
  val to_string : t -> string
end



(* Furthermore, we define a signature for maps (mappings from keys to values): *)

module type Map = sig
  type key
  type value
  type t
  val empty : t
   val set : key -> value -> t -> t  (* set updates the mapping such that key is now mapped to value. *)
  val get : key -> t -> value  (* get retrieves the value for the given key and throws a Not_found exception if no such key exists in the map. *)
  val get_opt : key -> t -> value option  (* get_opt retrieves the value for the given key or None if the key does not exist. *)
  val to_string : t -> string  (* to_string produces a string representation for the mapping, e.g.:"{ 1 -> "x", 5 -> "y" }" *)
end



(* 1 *)
(* Implement a module StringSet of signature Set to model sets of strings. *)

module StringSet : Set  with type t=string = struct
  type t = string
  let to_string s =
    "\"" ^ s ^ "\""
end



(* 2 *)
(* Define a signature OrderedSet that extends the Set signature by a compare function with the usual type. *)

module type OrderedSet = sig 
  include Set 
  val compare : t -> t -> int 
end



(* 3 *)
(* Implement a functor BTreeMap that realizes the Map signature
and uses a binary tree to store key-value-pairs.
 The functor takes key and value sets as arguments. *)

module BTreeMap (K : OrderedSet) (V : Set) : Map with type key=K.t and type value= V.t = struct
  type key = K.t
  type value = V.t

  type t = Empty | Node of (key * value * t * t) 
   
  let empty = Empty

  let rec set k v tree = 
    match tree with
    | Empty -> Node (k, v, Empty, Empty)
    |  Node (key, value, tree', tree'') ->
      let diff = K.compare k key in
      if diff>0 then Node (key, value, tree', set k v tree'')
      else if diff=0 then Node (key, v, tree', tree'')
      else Node (key, value, set k v tree', tree'')
  

 
  let rec get_opt k tree = 
    match tree with 
    | Empty -> None
    | Node (key, value, tree', tree'') -> 
      let diff = K.compare k key in
      if diff>0 then get_opt k tree''
      else if diff=0 then Some value
      else get_opt k tree'

  let get k tree = 
    match (get_opt k tree) with
    | None -> raise Not_found
    | Some value -> value

  let to_string tree =
    let rec to_s tree=
     match tree with 
    | Empty -> ""
    | Node (k, v, tree', tree'') ->
      (to_s tree') ^ K.to_string k ^ "->" ^ V.to_string v ^ ", "  ^ (to_s tree'')
    in "{"^ (String.sub (to_s tree) 0 (String.length (to_s tree)-2)) ^ "}" 
end 
(*cut out last , *)


(* 4 *)
(* Implement a StringSet and an ordered IntSet module. *)

module IntSet : OrderedSet with type t = int = struct
  type t = int
  let to_string s = string_of_int s 
  let compare = Stdlib.compare
end



(* 5 *)
(* Use the BTreeMap functor to define modules for int-to-string and int-to-int maps. *)

module IntStringMap = BTreeMap (IntSet) (StringSet)

module IntIntMap = BTreeMap (IntSet) (IntSet)



(* 6 *)
(* Insert some values into an int-to-string map and print its string representation. *)

let lst = [1;5;3;7;8]

let m = List.fold_left (fun acc x -> IntStringMap.set x (string_of_int (x+2)) acc ) IntStringMap.empty lst


let _ = print_endline (IntStringMap.to_string m)