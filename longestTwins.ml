 
(*write lt_seq : 'a list -> 'a list 
that returns the longest sequence of elements that 
appears at least twice in the given list. 
If multiple sequences of the same length exist, 
the one that appears first shall be returned. The sequences must not overlap.


Examples:
lt_seq [1;2;2;3;4;2;2;2;3;1] returns [2;2;3]
lt_seq [true;false;false;true] returns [true]
lt_seq ['a';'a';'b';'b';'a';'b';'b';'a';'a'] returns ['a';'b';'b']
lt_seq [0.;1.;2.;0.;2.;1.;2.;1.;2.;3.] returns [1.;2.] *)


(*Helper function that returnes a sub-list of input list from position of desired size*)
let rec sub (lst : 'a list) (position : int) (size : int) : 'a list=
if ((position + size ) > List.length lst || position>List.length lst)  then [] else 
match lst with 
| [] -> []
| x :: xs -> 
  if position = 0 then 
    if size = 0 then []
    else x :: sub xs position (size-1)
  else sub xs (position-1) size 



  let lt_seq (lst : 'a list) : 'a list = 
    (*Boolean helper function that checks if input twin list has one or more duplicates*)
      let hasTwin (lst : 'a list) (twin : 'a list) : bool =
        let rec count_matches (lst : 'a list) (twin : 'a list) (count : int) : int =
          if twin=[] then 2 else
            match lst with
            | [] -> count
            | x :: xs ->
                 (if List.length lst < List.length twin then count
                  else
                    (if List.hd twin = x then
                       (* find starting point for checking *)
                       (if sub lst 0 (List.length twin) = twin then
                          (* take sub lists of lst and match it with twin and appropriately increase count *)
                          count_matches (sub lst (List.length twin) (List.length lst - List.length twin)) twin (count+1)
                        else
                          (count_matches xs twin count))
                     else
                       (count_matches xs twin count)))
      
        in
        
        (count_matches lst twin 0) > 1 (*true if twin has one or more duplicate*)

    in
  
    let rec twin' (lst : 'a list) (size : int) (position : int): 'a list =
      (*find 'a list that has twin(s) of input size in lst*)
      if size < 1 then [] else 
          if (position+size> List.length lst) then [] (*if code goes beyond lst return []*) 
          else let twin= sub lst position size in 
            (if (hasTwin lst twin) then twin 
              (*go through all positions*)
            else twin' lst size (position+1))
    in

    let rec twin'' (lst : 'a list) (size : int) (fTwin : 'a list): 'a list =
      (*contorl that size does not become negative*)
      let tempTwin= twin' lst size 0 in
      if (size < 1) then []
      else ( if (tempTwin<>[]) then tempTwin else (twin'' lst (size-1) []))        
      
      (*since we want to return longest twin we decrease size recursivly until biggerTwin becomes []*)

    in twin'' lst ((List.length lst)/2) [] (*start with maximum size*)
    (*the lt_seq returnes [] if lst has only unique characters*)

(*sample runs*)
let l1 = [1;2;2;3;4;2;2;2;3;1] (*[2;2;3]*)
let l2 = [true;false;false;true] (*[true]]*)
let l3 = ['a';'a';'b';'b';'a';'b';'b';'a';'a'] (*['a';'b';'b']*)
let l4 = [0.;1.;2.;0.;2.;1.;2.;1.;2.;3.] (*[1.;2.]*)

let l5 = [1;2;3;4;1;2;3;4]

