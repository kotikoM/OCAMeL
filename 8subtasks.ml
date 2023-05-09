(* 1 *)
(* Write a function member, which takes a comparision function c, a term t and a list l and returns true 
if l contains an element e such that e and t are equal with respect to c. *)
let rec member c t =function 
| [] -> false 
| x :: xs -> c x t = 0 || member c t xs


let equal_second_components (_, x) (_, y) = compare x y

let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2)

let testing_member () =
  let l =
    [
      __LINE_OF__ ((member compare 3 [1; 2; 3]) = true);
      __LINE_OF__ ((member compare 4 [1; 2; 3]) = false);
      __LINE_OF__ ((member compare 'a' ['a'; 'b'; 'c']) = true);
      __LINE_OF__ ((member equal_second_components ('a',5) [(1,2); (3,4); (5,6)]) = false);
      __LINE_OF__ ((member equal_second_components ('a',6) [(1,2); (3,4); (5,6)]) = true);
      __LINE_OF__ ((member equal_second_components (42, 6) [(1,2); (3,4); (5,6)]) = true);
      __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 2; 3]) = true);
      __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 3; 5]) = false);
   ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The member test succeeds.\n"; [])
  else (Printf.printf "The member test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


(* 2 *)
(* 
Write a function count_occurrences, which takes a list list1 and returns a list list2 of pairs (e,n), 
where e is an element of list1 and n is the number of occurrences of e in list1.
 Make sure that list2 is given in a sorted form, in the descending order of the occurrences. *)

 let count_occurrences lst =
  let rec count acc lst =
    match lst with
    | [] -> List.rev acc
    | x::xs ->
        let count_x = List.fold_left (fun count y -> if x = y then count + 1 else count) 1 xs in
        let acc' = (x, count_x)::acc in
        let xs' = List.filter (fun y -> y <> x) xs in
        count acc' xs'
  in
  let sorted = List.sort (fun (_, count1) (_, count2) -> compare count2 count1) (count [] lst) 
in sorted

        let testing_count_occurrences () =
          let l =
            [
              __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
              __LINE_OF__ ((count_occurrences ['a'; 'b'; 'a'; 'c'; 'c'; 'a'; 'd']) = [('a', 3); ('c', 2); ('b', 1); ('d', 1)]);
              __LINE_OF__ ((count_occurrences [0; 0; 0; -2; 3; -1; -1; 3; 3; 0]) = [(0, 4); (3, 3); (-1, 2); (-2, 1)]);
              __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
            ] in
          let result = List.fold_left (&&) true (List.map snd l) in
          if result then (Printf.printf "The count_occurrences test succeeds.\n"; [])
          else (Printf.printf "The count_occurrences test fails.\n Check the corresponding line numbers in the list below.\n";
                (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


                
                        
(* 3 *)

(* Write a function `drop_last : 'a list -> 'a list` which takes a list,
drops its last element and gives beck the remaining part. *)

let rec drop_last (lst: 'a list):'a list=
match lst with 
| []-> failwith "Empty list! has no last element"
| [a] -> []
| x :: xs -> x :: drop_last xs


let testing_drop_last () =
  let l =
    [
      __LINE_OF__ ((drop_last [1; 2; 3; 4]) = [1; 2; 3]);
      __LINE_OF__ ((drop_last [1]) = []);
      __LINE_OF__ ((try Some (drop_last []) with (Failure _) -> None) = None) (* If this line is reported during testing, you have an rrror in raising Failure *)
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The drop_last test succeeds.\n"; [])
  else (Printf.printf "The drop_last test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

      
(* 4 *)

(* Modify drop_last so that it returns an optional value, instead of raising an exception for empty lists. 
Let this variant be called drop_last_opt. Then its type should be 'a list -> 'a list option and sample runs look as *)

let rec drop_last_opt (lst: 'a list):'a list option=
match lst with 
| []-> None
| [a] -> Some []
| x :: xs -> match drop_last_opt xs with 
            | None -> None
            | Some ys -> Some (x::ys) 


let testing_drop_last_opt () =
   let l =
     [
       __LINE_OF__ ((drop_last_opt []) = None);
       __LINE_OF__ ((drop_last_opt [1]) = Some []);
       __LINE_OF__ ((drop_last_opt [1;2;3]) = Some [1;2])
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The drop_last_opt test succeeds.\n"; [])
   else (Printf.printf "The drop_last_opt test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


(* 5 *)

(* Write a function zip_with that, given a binary function and two lists, constructs a new list as in: *)

let rec zip_with f xs ys=
match (xs, ys) with 
| ([], _) | (_, []) -> []
| (x::xs', y::ys') -> f x y :: zip_with f xs' ys' 


let testing_zip_with () =
     let l =
       [
         __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6]) = [[1; 5]; [2; 6]]);
         __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6;7;8]) = [[1; 5]; [2; 6]; [3; 7]]);
         __LINE_OF__ ((zip_with (fun x y -> (x,y)) [1;2;3] ['a';'b']) = [(1, 'a'); (2, 'b')]);
         __LINE_OF__ ((zip_with (+) [1;2;3] [5;6]) =[6; 8]);
         __LINE_OF__ ((zip_with (^) ["aa";"bb";"cc"] ["1";"2"]) = ["aa1"; "bb2"]);
  
       ] in
     let result = List.fold_left (&&) true (List.map snd l) in
     if result then (Printf.printf "The zip_with test succeeds.\n"; [])
     else (Printf.printf "The zip_with test fails.\n Check the corresponding line numbers in the list below.\n";
           (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

(* 6 *)

(* Write a function unzip on lists (i.e., a list of pairs is ‘unzipped’ into a pair of lists) 
using one of the fold functions you already know. For instance, unzip [('a',1); ('b',2)] = (['a';'b'], [1;2]) *)

  let unzip lst=
  List.fold_right
      (fun (x, y) (xs, ys) -> (x::xs, y::ys))
      lst
      ([],[])

let testing_unzip () =
   let l =
     [
       __LINE_OF__ ((unzip [('a',1); ('b',2)]) = (['a';'b'], [1;2]));
       __LINE_OF__ ((unzip []) = ([], []));
       __LINE_OF__ ((unzip [('a',1)]) = (['a'], [1]));

     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The unzip test succeeds.\n"; [])
   else (Printf.printf "The unzip test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

         
(* 7 *)
(* Show evaluation steps of unzip [('a',1);('b',2)]

unzip is a function which follows inputted list from right to left applying the following function to each element (fun (x, y) (xs, ys) -> (x::xs, y::ys)),
which takes pairs as input and outputs pairs of list which are composed of first elements and second elements of the inputted pairs. ([], []) means that 
function is starting with empty list. 

unzip [('a',1);('b',2)]
firstly lst will be replaced with input. 

  List.fold_right
  (fun (x, y) (xs, ys) -> (x::xs, y::ys))
  [('a',1);('b',2)]
  ([],[])

  Next function is applied to rightmost element 

  (fun (x, y) (xs, ys) -> (x::xs, y::ys))
  ('a', 1)
  (List.fold_right (fun (x, y) (xs, ys) -> (x::xs, y::ys)) [('b',2)] ([], []))
  
  which results in: 
  (fun (x, y) (xs, ys) -> (x::xs, y::ys))
  ('a', 1)
  (['b'], [2])

  The function is next applied to next element in the list and then result is added to (['b'], [2])

  finally 'a' is added to first pait list and 1 is added to second pair and get (['a'; 'b'], [1; 2]) as a result *)


  
(* 8 *)

(* function table_and_scorers should return 2 lists. 
1. The list of tuples for each team of the form (t, g, w, d, l, gf, ga, p), 
where t stands for a team, g for games played, w for wins, d for draws, l for losses, 
gf for goals for (scored goals), ga for goals against (conseded goals), p for points, 
which records the summary of all the games played by the team. 

2. The list of goalscorers as triples (player, team, goals). 
Sort the list first by goals, in case of a tie by the player name alphabetically.

*)

(* (t, g, w, d, l, gf, ga, p), *)

(* (team+games played+wins+draws+losses+scored goals+conseded goals+ points) *)

  type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha

  type games = (team * string list * team * string list)


  (* (team:team * g:int * w:int * d:int * l:int * gf:int * ga:int * p:int) list *
  (player:string * team:team * goals:int) list  *)

  let table_and_scorers (lst : games list) : ((team*int*int*int*int*int*int*int) list * 
  (string*team*int) list)=
  let initial_stats = List.map (fun t -> (t, 0, 0, 0, 0, 0, 0, 0)) [Arg; Sau; Mex; Pol; Por; Kor; Uru; Gha] in

  let autoGoals (goals : string list) : (int) = List.fold_left (fun acc x -> if x="OG" then acc+1 else acc) 0 goals in
    let stats=List.fold_left (fun acc(t1, players1, t2, players2)->

    let goals1= (List.length players1) - (autoGoals players1) + (autoGoals players2) in
    let goals2=(List.length players2) - (autoGoals players2) + (autoGoals players1) in
    let w, d, l= if goals1>goals2 then 1, 0, 0 else if goals1=goals2 then 0, 1, 0
    else 0, 0, 1 in
    let gf, ga=goals1, goals2 in
    

    let update_stats (t, g', w', d', l', gf', ga', 0)=
    if t=t1 then (t, g'+1, w+w', d+d', l+l', gf+gf', ga+ga', 0)
    else if t=t2 then (t, g'+1, w'+l, d+d', l'+w, gf'+ga, ga'+gf, 0)
    else (t, g', w', d', l', gf', ga', 0) in

    List.map update_stats acc)

    initial_stats lst in

    let stats' = List.sort (fun (t1, g1, w1, d1, l1, gf1, ga1, p1) (t2, g2, w2, d2, l2, gf2, ga2, p2) ->
      if p1 > p2 then -1
      else if p1 < p2 then 1
      else if (gf1 - ga1) > (gf2 - ga2) then -1
      else if (gf1 - ga1) < (gf2 - ga2) then 1
      else if gf1 > gf2 then -1
      else if gf1 < gf2 then 1
      else 0
    ) stats in

    let stats''= List.filter (fun (t1, g1, _, _, _, _, _, _) -> g1<>0) stats' in


    let pointsCalc w d= 3*w+d in

    let stats'''= List.map (fun (t, g, w, d, l, gf, ga, p) -> ((t, g, w, d, l, gf, ga, pointsCalc w d)) ) stats'' in
    


    let scorer =List.fold_left (fun acc (t1, players1, t2, players2) ->
    let goalscorers1 = List.map (fun p -> (p, t1, 1)) players1 in
    let goalscorers2 = List.map (fun p -> (p, t2, 1)) players2 in
    goalscorers1 @ goalscorers2 @ acc) [] lst in
  
    let goals (lst : (string * team * int) list) (elem : string) : int =
    List.fold_left (fun acc (p, t, g) -> if p = elem then acc + 1 else acc) 0 lst in 
    
    let scorer' =
    let res = ref [] in
    List.iter (fun (p, t, g) ->
    if not (List.exists (fun (p', _, _) -> p = p') !res) 
    then res := (p, t, goals scorer p) :: !res) scorer;
    !res in 

    (*I guess oG i.g. autoGoals should be removed*)

    let scorer''= List.filter (fun (p, _, _) -> p <> "OG") scorer' in 
    
    let scorers''' =List.sort (
    fun (p1, _, g1) (p2, _, g2) ->
    if g1 > g2 then -1 else 
      if g1 < g2 then 1 else 
        String.compare p1 p2 ) scorer'' in

    (stats''', scorers''') 


    let mylist=[(Arg, ["Messi";"OG"], Sau, ["Al-Shehri"; "Al-Dawsari"]);
    (Sau, ["Al-Dawsari"], Mex, ["Martin"; "Chavez"; "OG"])
   ]
        
   let wc22_C = 
    [(Arg, ["Messi"], Sau, ["Al-Shehri"; "Al-Dawsari"]);
     (Mex, [], Pol, []);
     (Pol, ["Zielinski"; "Lewandowski"], Sau, []);
     (Arg, ["Messi"; "Fernandez"], Mex, []);
     (Pol, [], Arg, ["Mac Allister"; "Alvarez"]);
     (Sau, ["Al-Dawsari"], Mex, ["Martin"; "Chavez"])
    ]


    let wc22_H = 
      [(Uru, [], Kor, []);
       (Por, ["Ronaldo"; "Felix"; "Leao"], Gha, ["Ayew"; "Bukari"]);
       (Kor, ["Cho Gue-sung"; "Cho Gue-sung"], Gha, ["Salisu"; "Kudus"; "Kudus"]);
       (Por, ["Fernandes"; "Fernandes"], Uru, []);
       (Kor, ["Kim Young-gwon"; "Hwang Hee-chan"], Por, ["Horta"]);
       (Gha, [], Uru, ["De Arrascaeta"; "De Arrascaeta"])
      ]

  let testing_table_and_scorers () =
    let l =
      [
        __LINE_OF__ (table_and_scorers wc22_H =
                       ([(Por, 3, 2, 0, 1, 6, 4, 6);
                         (Kor, 3, 1, 1, 1, 4, 4, 4); 
                         (Uru, 3, 1, 1, 1, 2, 2, 4);
                         (Gha, 3, 1, 0, 2, 5, 7, 3)],
                        [("Cho Gue-sung", Kor, 2);
                         ("De Arrascaeta", Uru, 2);
                         ("Fernandes", Por, 2);
                         ("Kudus", Gha, 2);
                         ("Ayew", Gha, 1);
                         ("Bukari", Gha, 1);
                         ("Felix", Por, 1);
                         ("Horta", Por, 1);
                         ("Hwang Hee-chan", Kor, 1);
                         ("Kim Young-gwon", Kor, 1);
                         ("Leao", Por, 1);
                         ("Ronaldo", Por, 1);
                         ("Salisu", Gha, 1)]));
        __LINE_OF__ (table_and_scorers wc22_C =
                       ([(Arg, 3, 2, 0, 1, 5, 2, 6);
                         (Pol, 3, 1, 1, 1, 2, 2, 4); 
                         (Mex, 3, 1, 1, 1, 2, 3, 4);
                         (Sau, 3, 1, 0, 2, 3, 5, 3)],
                        [("Al-Dawsari", Sau, 2);
                         ("Messi", Arg, 2);
                         ("Al-Shehri", Sau, 1);
                         ("Alvarez", Arg, 1);
                         ("Chavez", Mex, 1);
                         ("Fernandez", Arg, 1);
                         ("Lewandowski", Pol, 1);
                         ("Mac Allister", Arg, 1);
                         ("Martin", Mex, 1);
                         ("Zielinski", Pol, 1)]))
      ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The table_and_scorers test succeeds.\n"; [])
    else (Printf.printf "The table_and_scorers test fails.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

