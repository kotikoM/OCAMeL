(* Takes a list as input and returns a list of singleton lists *)
let init list = List.map (fun x -> [x]) list

(* Takes a tuple of two lists as input and returns a single merged list *)
let rec merge_kiu (t: 'a list * 'a list) : 'a list = 
  match t with 
  (* If both lists are empty, return an empty list *)
  | ([],[]) -> [] 
  (* If the second list is empty, return the first list *)
  | (a,[]) -> a 
  (* If the first list is empty, return the second list *)
  | ([],b) -> b 
  (* Compare the first elements of the two lists, select the smaller one, and continue recursively *)
  | h::t, hd::tl -> if h < hd then h :: merge_kiu (t, hd::tl) else hd :: merge_kiu (h::t, tl)

(* Takes a list of lists as input and recursively merges pairs of adjacent lists using `merge_kiu` *)
let rec merge_list (lst: 'a list list ) : 'a list list = 
  match lst with 
  (* If the list is empty, return an empty list *)
  | [] -> [] 
  (* If the list has only one element, return the list *)
  | [x] -> [x] 
  (* Merge pairs of adjacent lists until there is only one list left *)
  | l1::l2::ls -> merge_kiu (l1,l2) :: merge_list ls

(* Takes a list of lists as input and repeatedly merges the lists until there is only one list left *)
let rec iter (l: 'a list list) : 'a list = 
  match l with 
  (* If the list is empty, return an empty list *)
  | [] -> [] 
  (* If the list has only one element, return the list *)
  | [x] -> x 
  (* Merge the lists until there is only one list left *)
  | _ -> iter (merge_list l)

(* Sorts a list by repeatedly merging pairs of adjacent lists until there is only one list left *)
let sort list = iter (init list)
