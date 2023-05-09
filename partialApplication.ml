
let a : int -> (int -> 'a) -> 'a = (fun a b c -> c (a + b)) 3 

let b : 'a -> int -> int -> int = (fun a b -> (+) b) 


let c : ('a -> string) -> (string -> 'a) -> string list = (fun a b c -> b (c a) :: [a]) "x" 


let d : int list -> (int -> (int -> int) -> int) -> int = (fun a b -> List.fold_left b 1 (List.map ( * ) a))


let e : 'a list -> ('a -> bool) list = (let x = List.map in x (<)) 

