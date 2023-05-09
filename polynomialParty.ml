

(*polynomial has all powers from n to 1*)
(*coefficients are represented as list of floats*)


let p1 = [2.; -3.; 1.; -5.] (*2.x^3-3.x^2+x-5*)
let p2 = [2.; -3.; 1.] (*2.x^2-3.x+1*)

let rec power (element : float) (degree : int): float =
  match degree with
  | 0 -> 1.
  | _ -> element *. (power element (degree-1))

let index_map lst =
  let rec index_map_helper lst i acc =
    match lst with
    | [] -> List.rev acc
    | x :: xs -> index_map_helper xs (i+1) ((x,i) :: acc)
  in
  index_map_helper lst 0 []

let eval_poly (arg : float) (pol : float list) : float =
let indexed= index_map pol in
List.fold_left (fun acc (x, index) -> acc+.(x*.(power arg ((List.length pol)-index-1)))) 0. indexed 

 let derive_poly' (pol : float list) : float list=
let indexed = List.map (fun (x, y) -> (x *. float_of_int ((List.length pol)-y-1)), y) (index_map pol) 
in
 List.map (fun (x, y) -> x) indexed



let derive_poly (pol : float list) : float list =
  let lst=derive_poly' pol in
  let rec temp (pol : float list) : float list=
  match pol with
  | [] -> []
  | x::[] -> []
  | x::xs -> x:: temp xs
in temp lst
