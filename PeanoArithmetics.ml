(* The natural numbers can be defined recursively as follows:

0
0 is a natural number.
if 
�
n is a natural number, then so is the successor of 
�
n.
We can easily represent this in OCaml using corresponding constructors: *)


type nat = Zero | Succ of nat

let rec int_to_nat (a : int) : nat=
match a with
| 0 -> Zero
| _ -> Succ (int_to_nat (a-1))

let rec nat_to_int (a : nat) : int=
match a with
| Zero -> 0
| Succ x -> 1+nat_to_int x

let rec add (a : nat) (b : nat) : nat =
match a with 
| Zero -> b
| Succ x -> add x (Succ b)

let rec mul (a : nat) (b : nat) : nat=
match a with 
| Zero -> Zero 
| Succ x -> add b (mul x b)


let rec pow (a : nat) (b : nat) : nat=
match b with
| Zero -> Succ Zero
| Succ x -> mul a (pow a x)

let rec leq (a : nat) (b : nat) : bool=
match (a, b) with
| (Zero, _) -> true
| (_, Zero) -> false 
| (Succ x, Succ y) -> leq x y