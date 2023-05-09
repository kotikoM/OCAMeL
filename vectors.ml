(* Implement some functionalities to compute with 3-dimensional vectors *)


(* Define a suitable data type for your point *)
type vector3=(int * int * int)



(* Define three points p1, p2 and p3 with different values *)
let p1 : vector3=(1, 2, 3)
let p2 : vector3=(-1, 10, -5)
let p3 : vector3=(8, -20, 4)



(* Implement a function vector3_to_string : vector3 -> string to convert a vector into a human-readable representation *)
let vector3_to_string (v : vector3) : string = 
  match v with
  | (v1, v2, v3) -> "("^Int.to_string v1 ^ ", " ^ Int.to_string v2 ^ ", " ^ Int.to_string v3 ^")"



(* Write a function vector3_add : vector3 -> vector3 -> vector3 to add two vectors.  *)
let vector3_add ((v1, v2, v3): vector3) ((v1', v2', v3') : vector3) : vector3 =
  (v1+v1', v2+v2', v3+v3')



(* Write a function vector3_max : vector3 -> vector3 -> vector3 that returns the larger vector (the vector with the greater magnitude) *)
let vector3_max ((v1, v2, v3): vector3) ((v1', v2', v3') : vector3) = 
  if sqrt(float(v1*v1+v2*v2+v3*v3)) > sqrt(float(v1'*v1'+v2'*v2'+v3'*v3'))
    then (v1, v2, v3)
else (v1', v2', v3')



(* Compute the result of adding p1 to the larger of p2 and p3 and print the result as a string *)
let result= vector3_to_string (vector3_add (p1) (vector3_max p2 p3))