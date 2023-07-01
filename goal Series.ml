
(* 1 *)

type t = A of bool * t | B

let rec f a = 42 + a []

let rec g b = function A (v,u) -> g (b && v) u | B -> b

let rec h c = match c with A (v,u) -> v::h u | B -> []

(* multiple choice *)
(* Type t defines a record -> type t is Not a record

Type t represents sequences of bools -> type t is a sequence of bools

The definition of f contains a syntax error -> f does not have syntax errors 
(it takes in a function that takes in list and returnes int)

Function g is tail-recursive -> function g is tail recursive

The type of g is bool -> bool -> type of g is bool->t->bool

The function h is free of side-effects -> yes

The expression h (A (false, A (true, B))) evaluates to [false;true] -> correct *)


(* Implement a tail-recursive version of h such that h operates in constant stack
space. Do not change its type! *)
let h c =
  let rec helper acc cs = 
    match cs with
    | B -> List.rev acc
    | A (v, u) -> helper (v::acc) u
  in helper [] c
  

(* Define a function e such that
(g true x) = (not (List.fold_left e false (h x)))
is true for all inputs x *)

let e acc x = acc && x 

(* 2 *)
(* The result of a soccer game is either a win, a draw or a loss, for which the team is
rewarded with 3, 1 or 0 points respectively. For the type *)

type result = 
Win (* 3 points *)
| Draw (* 1 point *)
| Loss (* 0 points *)

(* implement a function series : int -> int -> result list list, such that a
call series g p returns a list of all sequences of results of g games that lead to p
points.
Example: The call series 3 4 returns
[[Win;Draw;Loss];[Win;Loss;Draw];[Draw;Win;Loss];
[Draw;Loss;Win];[Loss;Win;Draw];[Loss;Draw;Win]] *)

let rec series g p =
 if g = 0 then  
    (if p = 0 then [[]] 
     else [])
    else 
   List.map (fun r -> Win::r) (series (g-1) (p-3))
 @ List.map (fun r -> Draw::r) (series (g-1) (p-1))
 @ List.map (fun r -> Loss::r) (series (g-1) p)
        
