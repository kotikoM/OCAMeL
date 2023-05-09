

(*fold_left f1 0 l returns the length of list l.*)

let f1 = (fun acc x -> acc+1)


(*fold_left f2 [] [l1;l2;l3...ln] returnes longest of those lists*)
let f2 = (fun acc x -> if (List.length x) > (List.length acc) then x else acc)


(*fold_left f3 [] [a1, b1; a2, b2...an, bn] returnes [b1, a1; b2, a2; ...bn, an]*)
let f3 = (fun acc (a, b) -> acc@[(b, a)])


(*fold_left f3 [] [a0;...;a(n-2);a(n-1);an] returnes [an;a(n-2);...;a0;...;a(n-3);a(n-1)]*)
let f4 = (fun acc x ->if (List.length acc)=0 then [x] else
                      if (List.length acc) mod 2=1 then acc@[x] 
                      else [x]@acc)


(*fold_left f5 (fun _ -> 0) [(k1,v1);(k2,v2)...(kn, vn)] computes a function g such that g (ki)=vi *)
let f5 acc (k, v) =
  fun x ->
    if x = k then v else acc x


(*fold_left f6 [0] [f1;f2;f3...fn] [fn(fn-1(...f2(f1(0))));...f2(f1(0));f1(0);0] for functions fn*)



let f6 = (fun acc f-> f(acc) @ acc)


(*fold_left f7 a [cn;cn-1;...;c0] computes a^(2^(n+1))* pi(i=0) (n) (ci)^(2^i)*)
let rec power a p =
  match p with 
  | 0 -> 1
  | 1 -> a
  | _ -> power (a*a) (p-1)

let rec index a lst =
  match lst with
  | [] -> 0
  | x :: xs-> if x=a then 0 else 1+(index a xs)


let tempf7 = (fun (value, position) x -> ((power acc (power 2 (position+1))) * (power x (power 2 position))), (position+1))

let f7 = (*to do*)