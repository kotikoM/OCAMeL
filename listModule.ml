 
(* Check the documentation of the OCaml List module and find out what the following functions do. 
Then implement them yourself. Make sure your implementations have the same type.
 In cases where the standard functions throw exceptions, you may just failwith "invalid". *)

 let hd (lst : 'a list) : 'a=
match lst with
| x::_ -> x
| _ -> failwith "error"

let tl (lst : 'a list) : 'a =
let rec temp lst =
match lst with 
| [] -> failwith "error"
| x::[] -> x
| x::xs -> temp xs
in temp lst


let length (lst : 'a list) : int =
let rec temp lst = 
  match lst with
  | [] -> failwith "error"
  | x :: [] -> 1
  | x :: xs -> 1 + temp xs
in temp lst


let append (lst : 'a list) (lst' : 'a list) : 'a list=
let rec temp lst lst'=
match lst with 
| [] -> lst'
| x :: xs -> x :: (temp xs lst')

in temp lst lst'



let rec nth lst n = 
  if n < 0 then raise (Invalid_argument "error") else
    match lst with 
    | [] -> failwith "error"
    | x :: xs -> if n=0 then x else nth xs (n-1)

let l1 = [1;2;3;4;5]

let l2 = [5;4;3;2;1]