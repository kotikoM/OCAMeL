

(* function takes in 3 lists and returnes list 
where first 3 elements are from those 3 lists respectivly
same applies to next 3 elements *)



let rec interleave3 l1 l2 l3 =
  let rec interleave2 l1 l2 =
    match l1 with 
    |[] -> l2
    | x::xs -> x::interleave2 l2 xs
  in
  match l1 with 
  |[] -> interleave2 l2 l3
  | x::xs -> x::interleave3 l2 l3 xs



  let l1 = [0;1;2]
  let l2 = [10;11;12]
  let l3 = [20;21;22]

 (*[0;10;20;1;11;21;2;12;22]*)




 (*tester*)
  let testing_interleave3 () =
    let l =
      [
        __LINE_OF__ ((interleave3 l1 l2 l3 ) = [0;10;20;1;11;21;2;12;22]);
      
     ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The member test succeeds.\n"; [])
    else (Printf.printf "The member test fails.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
 