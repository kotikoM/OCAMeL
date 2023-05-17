(* 1 *)

(* Find functions f1, f2, and f3, such that

fold_left f1 [] [(a1, b1) ; ... ; (an, bn)] for arbitrary ai, bi computes the list [(b1, a1); ... ; (bn, an) ]
fold_left f2 [] [a_0 ; ... ; a_{n−3} ; a_{n−2}; a_{n−1}; a_n] for arbitrary elements a_i computes the list [a_n; a_{n−2} ; ... ; a_0 ; ... ; a_{n−3} ; a_{n−1}]
fold_left f3 (fun _ -> 0) [(k1 , v1) ; ... ; (kn, vn) ] computes a function g such that g(ki) = vi for all 1 ≤ i ≤ n. The k's are assumed to be pairwise distinct.

WRITE YOUR IMPLEMENTATIONS OF f1, f2, AND f3. *)



let f1 = (fun acc (a, b) -> acc@[(b, a)])

let f2 = (fun acc x ->if (List.length acc)=0 then [x] else
  if (List.length acc) mod 2=1 then acc@[x] 
  else [x]@acc)

let f3 acc (k, v) =
    fun x ->
      if x = k then v else acc x


let testing_fs () =
   let l =
     [
       __LINE_OF__ ((List.fold_left f1 [] [(1,2); (3,4); (5,6)]) =
                      [(2,1); (4,3); (6,5)]);
       __LINE_OF__ ((List.fold_left f2 [] ['a';'b';'c';'d';'e';'f';'g']) =
                      ['g';'e';'c';'a';'b';'d';'f']);
       __LINE_OF__ (let g = List.fold_left f3 (fun _ -> 0)
                              [('a',3); ('z', -9); ('d', 18)] in
                    (g 'a' = 3) && (g 'd' = 18) && (g 'z' = -9))
    ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The f1, f2, f3 test succeeds.\n"; [])
   else (Printf.printf "The f1, f2, f3 test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)



(* 2 *)

(* Rewrite the following functions in a tail-recursive form:

   let rec map f = function
      | [] -> []
      | x :: xs -> f x :: map f xs


   let rec replicate n x =
      if n < 1 then [] else x :: replicate (n-1) x *)

    let map_tr func lst=
        let rec tail acc = function
        | [] -> List.rev acc
        | x :: xs -> tail (func x::acc) xs
   in tail [] lst


   let replicate_tr n x=
        let rec tail acc n =
            if n<1 then acc 
            else tail (acc@[x]) (n-1) 
          in tail [] n 

   let test_tr_llist () =
    let l =
     [
      __LINE_OF__ (map_tr succ [1;2;3] = [2;3;4]);
      __LINE_OF__ (map_tr (fun x -> x^x) ["a";"b";"c"] = ["aa";"bb";"cc"]);
      __LINE_OF__ (replicate_tr 5 "a" = ["a";"a";"a";"a";"a"]);
      __LINE_OF__ (replicate_tr (-3) "a" = [])
       ] in
      let result = List.fold_left (&&) true (List.map snd l) in
      if result then (Printf.printf "The tests for map and replicate succeed.\n"; [])
      else (Printf.printf "The test for tests for map and replicate fail.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

(* 3 *)

(* Implement a mapping function that maps a function over a lazy list. 
Implement it both for custom and OCaml lazy list variants. 
Call them respectively map_over_custom_llist and map_over_ocaml_llist. *)

type 'a custom_llist = (unit -> 'a custom_cell) and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist)

type 'a ocaml_llist = 'a ocaml_cell Lazy.t and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist)



let rec map_over_custom_llist (f : 'a -> 'b) (l : 'a custom_llist)  : 'b custom_llist =
  fun () ->
    match l () with
    | NilC -> NilC
    | ConsC (hd, tl) -> ConsC (f hd, map_over_custom_llist f tl)


let rec map_over_ocaml_llist (f : 'a -> 'b) (l : 'a ocaml_llist)  : 'b ocaml_llist =
  lazy (match Lazy.force l with
            | NilO -> NilO
            | ConsO (hd, tl) -> ConsO (f hd, map_over_ocaml_llist f tl))


(* From list to llist*)
let rec fromListToCustom (lst : 'a list) : 'a custom_llist =
  match lst with
  | [] -> fun () -> NilC
   | x :: xs ->
     fun () -> ConsC (x, fromListToCustom xs)

 let rec fromListToO (lst : 'a list) : 'a ocaml_llist =
   lazy (
     match lst with
      | [] -> NilO
      | hd :: tl -> ConsO (hd, fromListToO tl)
      )


(* Helper functions for llists*)
let rec from_to_custom from to_ step =
  if from <= to_
  then fun () -> ConsC (from, from_to_custom (from + step) to_ step)
  else fun () -> NilC

let rec print_custom_llist n c_list =
if n != 0
then match c_list () with
   | NilC -> print_string "Nil\n"
   | ConsC (h, t) ->
      Printf.printf "%d, " h;
      print_custom_llist (n-1) t
else print_string "...\n"

let rec custom_llist_to_string n c_list =
if n != 0
then match c_list () with
| NilC -> "Nil"
| ConsC (h, t) ->
   string_of_int h ^ ", " ^
     custom_llist_to_string (n-1) t
else "..."

let rec from_to_ocaml from to_ step =
  if from <= to_
  then lazy (ConsO (from, from_to_ocaml (from + step) to_ step))
  else lazy NilO

let rec print_ocaml_llist n o_list =
if n != 0
then match Lazy.force o_list with
| NilO -> print_string "Nil\n"
| ConsO (h, t) ->
   Printf.printf "%d, " h;
   print_ocaml_llist (n-1) t
else print_string "...\n"

let rec ocaml_llist_to_string n o_list =
if n != 0
then match Lazy.force o_list with
| NilO -> "Nil"
| ConsO (h, t) ->
   string_of_int h ^ ", " ^
     ocaml_llist_to_string (n-1) t
else "..."





    let test_map_llist () =
  let l =
    [
      __LINE_OF__ (custom_llist_to_string 10
        (map_over_custom_llist (fun x -> (x+1)) (from_to_custom 0 5 1)) =
                     "1, 2, 3, 4, 5, 6, Nil");
      __LINE_OF__ (custom_llist_to_string 10
        (map_over_custom_llist (fun x -> (x+1)) (from_to_custom 6 5 1)) =
                     "Nil");
       __LINE_OF__ (ocaml_llist_to_string 10
        (map_over_ocaml_llist (fun x -> x+1) (from_to_ocaml 0 5 1)) =
                      "1, 2, 3, 4, 5, 6, Nil");
        __LINE_OF__ (ocaml_llist_to_string 10
        (map_over_ocaml_llist (fun x -> x+1) (from_to_ocaml 6 5 1)) =
                       "Nil")
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The test for mapping over lazy lists succeeds.\n"; [])
  else (Printf.printf "The test for mapping over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
(* 4 *)

(* Implement a merging functions in ocaml that combines two sorted lazy lists.
The idea of merging two lists: merge [1;4;6;7;8; ... ] [1;2;3;4;10; ... ] = [1;1;2;3;4;4;6;7;8;10; ... ]
Implement the function both for custom and OCaml lazy list variants. Call them respectively merge_custom_llists and merge_ocaml_llists.
let rec from_to_custom from to_ step = *)


let merge_custom_llists l1 l2 =
let rec merge_custom l1 l2 =
match (l1 (), l2 ()) with
| (NilC, l) -> l
| (l, NilC) -> l
| (ConsC (x1, l1'), ConsC (x2, l2')) -> 
  if x1<=x2 then
    ConsC (x1, fun () -> (merge_custom (l1') (l2)))
  else 
    ConsC (x2, fun () -> (merge_custom (l1) (l2')))
  
  in (fun () -> merge_custom l1 l2)


  let rec merge_ocaml_llists l1 l2 =
   lazy (match l1, l2 with
         | (lazy NilO, _) -> Lazy.force l2
         | (_, lazy NilO) -> Lazy.force l1
         | (lazy (ConsO (x1, xs1)), lazy (ConsO (x2, xs2))) ->
                if x1 <= x2 then 
                  ConsO (x1, merge_ocaml_llists xs1 l2)
                else 
                  ConsO (x2, merge_ocaml_llists l1 xs2))
    
  

let test_merge_llists () =
  let l =
    [
      __LINE_OF__ (custom_llist_to_string 13
        (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 0 5 1)) =
                     "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
      __LINE_OF__ (custom_llist_to_string 13
                     (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 6 5 1)) =
                     "0, 1, 2, 3, 4, 5, Nil");
      __LINE_OF__ (ocaml_llist_to_string 13
        (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 0 5 1)) =
                     "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
      __LINE_OF__ (ocaml_llist_to_string 13
                     (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 6 5 1)) =
                     "0, 1, 2, 3, 4, 5, Nil")
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The test for merging over lazy lists succeeds.\n"; [])
  else (Printf.printf "The test for merging over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)



(* 5 *)

(* Implement a function that drops duplicates from a sorted lazy list.
Implement it both for custom and OCaml lazy list variants. 
Call them respectively drop_dupl_custom_llist and drop_dupl_ocaml_llist. *)


let drop_dupl_custom_llist l =
let rec drop_dupl l = 
  match l() with
        | NilC -> NilC
        | ConsC (x, xs) ->
            match xs() with
            | NilC -> ConsC (x, fun () -> NilC)
            | ConsC (y, _) when x = y -> (drop_dupl xs)
            | ConsC (y, _) -> ConsC (x, fun () -> drop_dupl xs)

        in (fun () -> drop_dupl l)



  let drop_dupl_ocaml_llist l =
    let rec drop_dupl l =
      match Lazy.force l with
            | NilO -> NilO
            | ConsO (x, l') ->
              match Lazy.force l' with
              | NilO -> ConsO (x, lazy NilO)
              | ConsO (y, _) when x = y -> Lazy.force (lazy (drop_dupl l'))
              | ConsO (y, _) -> ConsO (x,lazy( drop_dupl l'))

    in lazy (drop_dupl l)
  
  
    let test_drop_dupl_llists () =
      let l =
        [
          __LINE_OF__ (custom_llist_to_string 13
                         (drop_dupl_custom_llist
                            (merge_custom_llists (from_to_custom 0 5 1)
                               (from_to_custom 0 5 2))) =
                         "0, 1, 2, 3, 4, 5, Nil");
          __LINE_OF__ (custom_llist_to_string 13
                         (drop_dupl_custom_llist
                            (merge_custom_llists (from_to_custom 0 5 1)
                               (from_to_custom 6 5 1))) =
                         "0, 1, 2, 3, 4, 5, Nil");
          __LINE_OF__ (ocaml_llist_to_string 13
                         (drop_dupl_ocaml_llist
                            (merge_ocaml_llists (from_to_ocaml 0 5 1)
                               (from_to_ocaml 0 5 1))) =
                         "0, 1, 2, 3, 4, 5, Nil");
          __LINE_OF__ (ocaml_llist_to_string 13
                         (drop_dupl_ocaml_llist
                            (merge_ocaml_llists (from_to_ocaml 0 5 1)
                               (from_to_ocaml 6 5 1))) =
                         "0, 1, 2, 3, 4, 5, Nil")
        ] in
      let result = List.fold_left (&&) true (List.map snd l) in
      if result then (Printf.printf "The test for dropping duplicates from  lazy lists succeeds.\n"; [])
      else (Printf.printf "The test for dropping duplicates from lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
            (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
    
    
(* 6 *)

(* Implement a function hamming that lazily computes the infinite sequence of Hamming numbers 
(i.e., all natural numbers whose only prime factors are 2, 3, and 5), e.g., hamming = [1;2;3;4;5;6;8;9;10;12;15;16;18;20; ... ]
Implement it both for custom and OCaml lazy list variants. Call them respectively hamming_custom_llist and hamming_ocaml_llist. *)


(* Slightly modified merge that takes into consideration duplicates and does not repeat them*)
let merge_custom l1 l2 =
  let rec merge_custom l1 l2 =
  match (l1 (), l2 ()) with
  | (NilC, l) -> l
  | (l, NilC) -> l
  | (ConsC (x1, l1'), ConsC (x2, l2')) -> 
    if x1=x2 then 
      ConsC (x1, fun ()-> merge_custom (l1') (l2'))
  else
    if x1<x2 then
      ConsC (x1, fun () -> (merge_custom (l1') (l2)))
    else 
      ConsC (x2, fun () -> (merge_custom (l1) (l2')))
    
    in (fun () -> merge_custom l1 l2)

let hamming_custom_llist=fun () ->
let rec hamming_numbers n = 
 ConsC (n, (merge_custom (merge_custom (fun () -> hamming_numbers (2*n) ) (fun () -> hamming_numbers (3*n))) (fun () -> hamming_numbers (5*n))))
in
hamming_numbers 1




let rec merge_ocaml l1 l2 =
  lazy (match l1, l2 with
        | (lazy NilO, _) -> Lazy.force l2
        | (_, lazy NilO) -> Lazy.force l1
        | (lazy (ConsO (x1, xs1)), lazy (ConsO (x2, xs2))) ->
              if x1=x2 then 
                ConsO (x1, merge_ocaml xs1 xs2)
            else 
               if x1 < x2 then 
                 ConsO (x1, merge_ocaml xs1 l2)
               else 
                 ConsO (x2, merge_ocaml l1 xs2))
                 

  let hamming_ocaml_llist =
  let rec hamming_numbers n = 
    lazy (ConsO (n, (merge_ocaml (merge_ocaml (hamming_numbers (2*n) ) (hamming_numbers (3*n))) (hamming_numbers (5*n)))))
  in hamming_numbers 1

  let test_hamming_llists () =
    let l =
      [
        __LINE_OF__ (custom_llist_to_string 14 hamming_custom_llist =
                       "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
        __LINE_OF__ (custom_llist_to_string 20 hamming_custom_llist = 
                       "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...");
        __LINE_OF__ (ocaml_llist_to_string 14 hamming_ocaml_llist =
                       "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
        __LINE_OF__ (ocaml_llist_to_string 20 hamming_ocaml_llist = 
                       "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...")
      ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The test for Hamming lists succeeds.\n"; [])
    else (Printf.printf "The test for hamming lists fails.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

