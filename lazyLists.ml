
(* Infinite data structures (e.g. lists) can be realized using the concept of lazy evaluation. 
Instead of constructing the entire data structure immediately,
 we only construct a small part and keep us a means to construct more on demand. *)

type 'a llist = Cons of 'a * (unit -> 'a llist)


(*Implement the function lnat : int -> int llist 
   that constructs the list of all natural numbers starting at the given argument.*)
let rec lnat n =
  Cons (n, fun () -> lnat (n+1))


(* Implement the function lfib : unit -> int llist 
   that constructs a list containing the Fibonacci sequence. *)
  let rec fib a b =
  Cons ((a+b), fun () -> fib b (a+b))
  
  let rec lfib () = Cons (0, fun () -> Cons (1, fun () -> fib 0 1))


(* Implement the function ltake : int -> 'a llist -> 'a list 
that returns the first n elements of the list. *)

let rec ltake n llst =
  match n, llst with
  | (0, _) -> []
  | (_, Cons (x, lazy_xs)) -> x :: ltake (n-1) (lazy_xs ())


  (* Implement the function lfilter : ('a -> bool) -> 'a llist -> 'a llist
  to filter those elements from the list that do not satisfy the given predicate. *)
  
  let rec lfilter f llst = 
    match llst with
    | Cons (x, lazy_xs) when f x -> Cons (x, fun () -> lfilter f (lazy_xs ()))
    | Cons (_, lazy_xs) -> lfilter f (lazy_xs ())
