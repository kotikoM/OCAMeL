

(* (%) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
let (%) = fun f1 f2 x -> f1 (f2 x())


(* (@@) : ('a -> 'b) -> 'a -> 'b *)
let (@@) = fun f x -> f x 


(* (|>) : 'a -> ('a -> 'b) -> 'b *)
let (|>) = fun x f -> f x