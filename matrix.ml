

(* We define the signature *)
module type Ring = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val mul : t -> t -> t
  val compare : t -> t -> int
  val to_string : t -> string
end

(* of an algebraic ring structure, where add and mul are the two binary operations on the set of elements of type t. 
Values zero and one represent the identity elements regarding addition and multiplication. 
The function compare establishes a total order on the elements of the set. 
The auxiliary function to_string is used to generate a string representation of an element. *)

(* Furthermore, a matrix is given by the signature: *)
module type Matrix = sig
  type elem (* type of elements *)
  type t (* type of matrix *)
  val create : int -> int -> t (*create n m creates an empty (all zeroes) n×m matrix. *)
  val identity : int -> t (* identity n creates an n×n identity matrix. *)
  val from_rows : (elem list) list -> t (* from_rows l creates a matrix from the given list of rows (lists of elements). You may assume that l is non-empty and all lists have the same length. *)
  val set : int -> int -> elem -> t -> t (* set r c v m sets the element at row r and column c in matrix m to value v. *)
  val get : int -> int -> t -> elem (* get r c m returns the element at row r and column c from matrix m.   *)
  val transpose : t -> t (* transpose m returns the transpose of matrix m. *)
  val add : t -> t -> t (* add a b adds matrices a and b component-wise. *)
  val mul : t -> t -> t (* mul a b computes the matrix multiplication of a and b. *)
  val to_string : t -> string (* to_string m produces a string representation of m. Columns shall be separated by single whitespaces and rows by a newline character \ n. *)
end


(* 1 *)
(* Implement a module IntRing that implements the Ring signature for the int type. *)

module IntRing : Ring with type t=int = struct
  type t = int

  let zero = 0
  let one = 1
  let add a b = a+b
  let mul a b = a*b
  let compare a b = compare a b
  let to_string a = string_of_int a
  
end



(* 2 *)
(* Implement a module FloatRing that implements the Ring signature for the float type. *)

module FloatRing : Ring with type t=float = struct
  type t = float

  let zero = 0.
  let one = 1.
  let add a b = a+.b
  let mul a b = a*.b
  let compare a b = compare a b
  let to_string a = string_of_float a

end



(* 3 *)
(* Define a signature FiniteRing that extends the Ring signature with a value elems that represents a list of all elements of the ring's finite set. 
Make sure that everything in the Ring signature is part of FiniteRing (without copy-pasting them)! *)

module type FiniteRing = sig
  include Ring 

  val elems : t list
end



(* 4 *)
(* Implement a module BoolRing that implements the FiniteRing signature for the bool type. *)

module BoolRing : FiniteRing with type t=bool = struct
  type t = bool

  let zero = false
  let one = true
  let add a b = a || b
  let mul a b = a && b
  let compare a b = compare a b
  let to_string a = string_of_bool a
  let elems = [false; true]
end



(* 5 *)
(* Implement a functor SetRing that models a ring over the power set of the set in the FiniteRing passed as the functor's argument. 
SetRing has to implement the Ring signature. We use union and intersection as add and mul operations.
The representation produced by to_string is "{e1, ... , en}" . For compare we use a bit of an unconventional order. 
First, we order the elements in the set by their own order and then compare the sets lexicographically, 
e.g. {} < {1, 2, 3} < {2} < {2, 3} < {3} *)

module SetRing (F : FiniteRing) : Ring with type t=F.t list = struct
  type t = F.t list

  let zero = []
  let one = [F.one]
  let add a b = (List.sort_uniq compare (a @ b))
  let mul a b = (List.filter (fun x -> List.mem x b) a)
  let compare a b = 
    match a, b with
    | [], [] -> 0
    | [], _ ->  -1
    | _, [] -> 1
    | _ -> 
      let comp_elem x y =
        F.compare x y |> fun c -> if c=0 then 0 else if c<0 then -1 else  1 
      in
      let comp_elems = List.compare comp_elem in
      comp_elems a b  
  let to_string a = 
    "{" ^ (String.concat ", " (List.map F.to_string  a)) ^ "}"

end



(* 6 *)
(* Implement a functor DenseMatrix that satisfies the Matrix signature. 
The argument of the functor is a Ring that provides everything to implement the matrix operations.
The DenseMatrix is supposed to store all elements in the matrix in a list of rows, which are lists of elements. *)

module DenseMatrix (R : Ring) : Matrix with type elem=R.t =struct
  type elem = R.t
  type t = (elem list) list

  let create row column = List.init row (fun _ -> List.init column (fun _ -> R.zero))
  let identity n = List.init n (fun i -> List.init n (fun j -> if i=j then R.one else R.zero))
  let from_rows t = t
  let set r c v m = List.mapi (fun i x-> if i=r then (List.mapi (fun i y -> if i=c then v else y) x) else x) m 
  let get r c m = List.nth (List.nth m r) c
  let transpose m = 
    let num_rows = List.length m in
    let num_cols = List.length (List.hd m) in
    List.init num_cols (fun j -> List.init num_rows (fun i -> List.nth (List.nth m i) j))
  let add m1 m2 = List.map2 (List.map2 R.add) m1 m2
  let mul m1 m2 =
    let num_rows_m1 = List.length m1 in
    let num_cols_m2 = List.length (List.hd m2) in
    let m2t = transpose m2 in 
    List.init num_rows_m1 (fun i -> 
      List.init num_cols_m2 (fun j ->
        List.fold_left2 (fun acc a b -> R.add (R.mul a b) acc) R.zero (List.nth m1 i) (List.nth m2t j)
      )
    )

  let to_string m = 
    let row_strings = List.map (fun row ->
      "|"^String.concat " " (List.map R.to_string row)^"|"
    ) m in
    String.concat "\n" row_strings

end  



(* 7 *)
(* Implement a functor SparseMatrix that satisfies the Matrix signature. 
The argument of the functor is a Ring that provides everything to implement the matrix operations.
The SparseMatrix stores only those elements of the matrix that are non-zero. *)

module SparseMatrix (R : Ring) : Matrix with type elem=R.t =struct
  type elem = R.t
  type t = (int*int*elem) list (*(row, column, value) list*)

  let create row column = []
  let identity n = List.init n (fun i -> (i, i, R.one))
  let from_rows l = 
    List.filter (fun (a, _, _) -> a<>(-1)) 
    (List.concat (List.mapi (fun i row -> (List.mapi (fun j x -> if ((compare R.zero x)=0) then (-1, 0, x) else (i, j, x)) row))l ))
  let rec set r c v m = 
    match m with
    | [] -> raise Not_found
    | (i, j, x) :: xs -> 
      if (i=r && j=c) then [(i, j, v)]
      else [(i, j, x)] @ (set r c v xs) 
  let get r c m = 
    match List.find_opt (fun (i, j, _) -> (i=r) && (j=c)) m with
    | Some (_, _, v) -> v
    | None -> raise Not_found
  let transpose m = 
      List.sort (fun (x1, y1, _) (x2, y2, _) -> 
        if x1 <> x2 then compare x1 x2
        else compare y1 y2) (List.map (fun (i, j, x) -> (j, i, x)) m)
  let rec add a b =
    let rec add_aux acc a b =
      match a, b with
      | [], [] -> acc
      | [], _ -> acc @ b
      | _, [] -> acc @ a
      | ((i, j, v) :: a'), ((i', j', v') :: b') ->
        if ( i < i' || (i = i' && j < j') ) then
          add_aux (acc @ [(i, j, v')]) a' b
        else if ( i > i' || (i = i' && j > j') ) then
          add_aux (acc @ [(i', j', v')]) a b'
        else
          let sum = R.add v v' in
          if (compare R.zero sum)=0 then
            add_aux acc a' b'
          else
            add_aux (acc @ [(i, j, sum)]) a' b'
    in
    add_aux [] a b
     

    let mul a b =  
      let num_rows_a = List.fold_left (fun acc (i, _, _) -> max acc (i+1)) 0 a in
      let num_cols_b = List.fold_left (fun acc (_, j, _) -> max acc (j+1)) 0 b in
         
      let constructRow index m =
        let rec rower acc ind m'=
          match m' with 
          | [] -> acc
          | (i, j, v) :: m's ->
            if j=ind then (rower (acc@[(i, j,v)]) (ind+1) m's)
            else (rower (acc@[(i, ind, R.zero)]) (ind+1) m')
        in rower [] 0 (List.filter (fun (i, _, _) -> i=index) m)
            
      in List.init num_rows_a (fun i ->
           List.init num_cols_b (fun j ->
             List.fold_left2 (fun acc (_,_,x) (_, _, y) -> R.add (R.mul x y) acc) R.zero (constructRow i a) (constructRow j (transpose b))
           )
         )
         |> from_rows
         |> List.filter (fun (_, _, v) -> (v<>R.zero))
    
      

  let to_string m = 
    "["^String.concat "; " 
    (List.map (fun (i, j, v) -> "("^ (string_of_int i) ^ ", " ^ (string_of_int j) ^ ", " ^ (R.to_string v) ^ ")") m)^"]"

end  



(* Testing *)

module IntMatrix = DenseMatrix (IntRing)

let m1 = IntMatrix.from_rows [[1; 2]; [3; 4]]
let m2 = IntMatrix.from_rows [[5; 6]; [7; 8]]

let m3 = IntMatrix.mul m1 m2

(* prints |19 22|
          |43 50|  *)
let s1 = print_endline (IntMatrix.to_string m3)




let m4 = IntMatrix.from_rows [[3; 1; 4]]
let m5 = IntMatrix.from_rows [[4;3]; [2;5]; [6; 8]]

let m6 = IntMatrix.mul m4 m5

(* prints |38 46| *)
let s2 = print_endline (IntMatrix.to_string m6)



(* SparseMatrix tests *)

module IntSparseMatrix = SparseMatrix (IntRing)

let sp1 = IntSparseMatrix.from_rows [[1;2;3];[4;5;6];[7;8;9]]
let sp2 = IntSparseMatrix.from_rows [[1;2;3];[4;5;6];[7;8;9]]
let sp2' = IntSparseMatrix.from_rows [[-1;-2;-3];[-4;-5;-6];[-7;-8;-9]]

let sp3 = IntSparseMatrix.add sp1 sp2

(* prints [ (0, 0, 2); (0, 1, 4); (0, 2, 6); (1, 0, 8); (1, 1, 10); (1, 2, 12); (2, 0, 14); (2, 1, 16); (2, 2, 18) ] *)
let res1 = print_endline (IntSparseMatrix.to_string sp3)


let res2 = print_endline (IntSparseMatrix.to_string (IntSparseMatrix.identity 5))


let sp4 = IntSparseMatrix.from_rows [[1; 2]; [3; 4]]
let sp5 = IntSparseMatrix.from_rows [[5; 6]; [7; 8]]

let sp6 = IntSparseMatrix.mul sp4 sp5

let s'= print_endline (IntSparseMatrix.to_string sp6)