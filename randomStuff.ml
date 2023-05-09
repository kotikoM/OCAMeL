let multiplyer x y = x*y
(* same as
let multiplyer (x:int) (y:int) : int=
x*y *)

let rec fib x=
  if x=0 then 0 else
    if x<3 then 1 else fib(x-1)+fib(x-2)


let rec power (n:int) (x:float):float=
if n=0 then 1. else
  if n=1 then x else x*.power(n-1) x


let gcd (x:int) (y:int): int=
let min = if x<y then x else y
in 
let rec dec m=
  if x mod m=0 && y mod m=0
    then m
else dec (m-1)
in 
dec min






let count_occurrence lst =

  (* Define a helper function 'count' that takes an accumulator list 'acc' and a list 'lst' and returns the list of pairs (e, n) where e is an element of 'lst' and n is the number of occurrences of e in 'lst'. *)
    let rec count acc lst =
   (* Use pattern matching to check if the list is empty or not. *)
     match lst with
   | [] -> List.rev acc (* If the list is empty, return the accumulator list in reverse order. *)
    | x::xs ->
    (* Count the number of occurrences of 'x' in the rest of the list using 'List.fold_left'. *)
    let count_x = List.fold_left (fun count y -> if x = y then count + 1 else count) 1 xs in
   (* Add the pair (x, count_x) to the accumulator list 'acc'. *)
   let acc' = (x, count_x)::acc in
   (* Remove all occurrences of 'x' from the remaining list using 'List.filter'. *)
   let xs' = List.filter (fun y -> y <> x) xs in
   (* Call the 'count' function recursively with the updated accumulator list 'acc'' and the remaining list 'xs''. *)
   count acc' xs'
   in
   (* Call the 'count' function with an empty accumulator list to get the list of pairs (e, n) for the input list. *)
   let sorted = List.sort (fun (_, count1) (_, count2) -> compare count2 count1) (count [] lst) in
   (* Sort the list in descending order of the occurrences using 'List.sort' and a comparison function that compares the second element of each tuple. *)
   (* Map each pair (e, n) to a pair (e, n) and return the result. *)
   List.map (fun (x, count) -> (x, count)) sorted