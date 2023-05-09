let rec all_true (lst:bool list) : bool=
  match lst with 
  | [] -> true
  | x::[] -> if x=true then true else false
  | x::rest-> x && all_true rest
  
  
let even x=x mod 2=0

let rec even2ways (lst:int list) : bool=
match lst with
| [] -> true
| x::[] -> false 
| x1::x2::rest -> even x1 && even x2 && even2ways rest


let is_empty (lst:'a list) : bool=
match lst with 
| [] ->true
| _::_ ->false


let head (lst: 'a list) : 'a=
match lst with
| x::_->x
| _-> raise (Invalid_argument "head")


let rec add_list (lst : int list) : int=
match lst with
| [] -> 0
| x::[]->x
| x::rest-> x+add_list rest


let rec minimum (lst : int list) : int=
match lst with 
| []->  raise (Invalid_argument"minimum")
| x::[]-> x
| x::rest-> if x<minimum rest then x else minimum rest

let rec elem (element:'a) (lst:'a list) : bool=
match lst with
| []-> false
| x::rest -> x=element || elem element rest 



let rec length_of (lst: string list) : (string*int) list=
match lst with
| []->[]
| x::[] -> (x, (String.length x)) :: []
| x::rest -> (x, (String.length x)) :: length_of rest

let rec unzip (lst : ('a*'b) list) : ('a list * 'b list)=
match lst with
| [] -> ([],[])
| (x, y)::[] -> (x::[], y::[])
| (x, y) :: (x2, y2)::[] -> (x::x2::[], y::y2::[])
| (x, y) :: rest -> (x::(fst (unzip rest)), y::(snd (unzip rest) ))


let rec rev (lst : 'a list) : 'a list=
match lst with
| [] -> []
| x :: xs -> rev xs @ [x]