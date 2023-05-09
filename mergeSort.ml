let init list = List.map (fun x -> [x]) list


let rec merge_kiu (t: 'a list * 'a list) : 'a list = 
match t with 
([],[]) -> [] 
| (a,[]) -> a 
| ([],b) -> b 
| h::t, hd::tl -> if h < hd then h :: merge_kiu (t, hd::tl) else hd :: merge_kiu (h::t, tl);;

let rec merge_list (lst: 'a list list ) : 'a list list = 
match lst with 
[] -> [] 
| [x] -> [x] 
| l1::l2::ls -> merge_kiu (l1,l2) :: merge_list ls;;

let rec iter (l: 'a list list) : 'a list = 
  match l with [] -> [] 
  | [x] -> x 
  | _ ->  iter (merge_list l);;

let sort list = iter (init list)



