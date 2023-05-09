(* let rec f a = match a with [] -> a 
| x::xs -> (x+1)::f xs
(* Not tail recursion *)

let rec g a b = if a = b then 0 
else if a < b then g (a+1) b 
else g (a-1) b
(* is tail recursive*)

let rec h a b c = if b then h a (not b) (c * 2) 
else if c > 1000 then a 
else h (a+2) (not b) (c * 2)
(* tail recursive*)

let rec i a = function [] -> a 
| x::xs -> i (i (x,x) [x]) xs  
stack overflow *)

let rec fac n = 
  if n < 2 then 1 
  else n * fac (n-1)

let tfac n =
  let rec temp n acc = 
    if n<2 then acc else
      temp (n-1) (acc * n) 
    in temp n 1 


let rec remove a = function [] -> []
| x::xs -> if x = a then remove a xs else x::remove a xs

let tremove a l =
  let rec temp a l acc = match l with [] -> acc
  | x::xs -> if x=a then temp a l acc else temp a l (a::acc)

in List.rev (temp a l [])


let rec partition f l = match l with [] -> [],[]
    | x::xs -> let a,b = partition f xs in
        if f x then x::a,b else a,x::b  

let tpartition f l =
  let rec temp l (a, b) =
    match l with [] -> (a, b)
    | x::xs -> temp xs (if f x then x::a, b else a, x::b) 
  in 
  let r = temp l ([], []) in
  List.rev (fst r), List.rev (snd r) 



  let fib n=
  let rec fib n acc1 acc2=
  if n<=1 then acc1 
  else fib (n-1) (acc1+acc2) (acc1)
in fib n 1 0