
(* Function using List. *)

let squaresum (lst : int list) : int = 
  let maped lst = List.map (fun x -> x*x) lst in
  List.fold_left (fun acc x -> acc+x) 0 (maped lst)

let float_list (lst : int list) : float list =
  List.map (fun x -> float_of_int x) lst

let to_string (lst : int list) : string =
 let s="[" ^ List.fold_left (fun acc x -> acc ^ string_of_int x^";") "" lst in
 let s'=String.sub s 0 (String.length s - 1) ^ "]" in s'


let part_even (lst : int list) : int list =
  let rec temp lst evens odds=
  match lst with
  | [] -> List.rev_append evens odds
  | x :: xs -> 
    if x mod 2 = 0 then 
      temp xs (x :: evens) odds else
        temp xs evens (x :: odds)

      in temp lst [] [] 
  



  let l1 = [1;2;3;4;5;6;8]