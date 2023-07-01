

(* 1 *)
(* Rewrite the given function list_function into a tail recursive alternative list_function_tr.
For all values of f and ls, list_function_tr f ls must return exactly the same result *)
let list_function f ls =
  let rec list_function_helper i = function
  | [] -> []
  | x :: xs ->
  match f x i with
  | Some b -> b :: list_function_helper (i + 1) xs
  | None -> list_function_helper (i + 1) xs
  in
  list_function_helper 0 ls



(* tail recursive version *)
let list_function_tr f ls =
  let rec rev a = function
  | [] -> a
  | x::xs -> rev (x::a) xs in

  let rec list_function_helper_tr bs i = function
  | [] -> rev [] bs
  | x :: xs ->
  match f x i with
  | Some b -> list_function_helper_tr (b::bs) (i + 1) xs
  | None -> list_function_helper_tr bs (i + 1) xs in
  list_function_helper_tr [] 0 ls



(* 2 *)
(* In this assignment, you are meant to realize Java streams in Ocaml. For that, consider
the datapype 'a stream defined by *)

type 'a pair = Null | Pair of 'a * 'a stream
and 'a stream = unit -> 'a pair


(* A stream thus is a function which represents a finite or infinite sequence of elements.
Realize the following functions. *)

(* stream_of_list : 'a list -> 'a stream – which takes a list and turns it into
a stream. *)
let rec stream_of_list lst = fun () ->  (* DO NOT FORGET () *)
  match lst with
  | [] -> Null
  | x :: xs -> Pair (x, stream_of_list xs)


(* stream_of_fun : (int -> 'a) -> 'a stream – which takes a function f : int
-> 'a and returns a stream whose elements are f 0; f 1; ... . This stream should
be infinite. *)
let rec stream_of_fun f =
  let rec nextStream n = fun () ->
    Pair (f n, nextStream (n+1))
  in nextStream 0 


(* map : ('a -> 'b) -> 'a stream -> 'b stream – which takes a function f : 'a
-> 'b and a stream of elements of type 'a and turns it into a stream of elements
of type 'b by applying f to each element in the input stream *)
let rec map f stream = fun () -> 
  match stream () with
  | Null -> Null
  | Pair (v, stream') -> Pair (f v, map f stream')


(* concat : 'a stream -> 'a stream -> 'a stream – which returns the concatenation of the two argument streams. *)
let rec concat stream stream'= fun () -> 
  match stream() with 
  | Null -> stream'()
  | Pair (v, rest) -> Pair (v, concat rest stream')


(* reduce : ('a -> 'b -> 'a) -> 'a -> 'b stream -> 'b – which takes a combiner function f, 
and an initial accumulator value a and a stream and successively
combines all elements from the stream into a single accumulator value *)
let rec reduce f acc stream =
  match stream () with 
  | Null -> acc
  | Pair (v, rest) -> reduce f (f acc v) rest 


(* Take care that your functions map and concat work both for finite and infinite streams.
Now use the given functions to construct a function avg : 'a list -> ('a -> float)
-> float by first turning the list argument into a stream, then applying the function
argument to all elements in the stream, then computing the sum as well as the number
of elements in the stream in order finally to determine the average. In case that the list
was empty, the exception Failure "empty list" should be raised. *)

let rec average lst f =
  match lst with 
  | [] -> raise (Failure "empty list")
  | _ -> let mappedStream = map f (stream_of_list lst) in
        (((*sum*)
        reduce (fun acc x -> acc+.x) 0. mappedStream
        ) /. ( (*amount*)
        reduce (fun acc x -> acc+.1.) 0. mappedStream
        ))





(* 4 *)
(* In this assignment, we will define modules that function as interpreters for imperative
languages. The interpreters are all modules with the following signature: *)

module type I = sig
  type i
  type s
  val init_state : unit -> s
  val execute : s -> i -> s
  val return : s -> int
  end

(* Where i is the type of instructions that can be executed by the interpreter and s is the
type of the state these instructions operate on. init_state () creates a new empty state,
from which return can extract a single return value. Finally, execute s i executes the
instruction i on state s and returns the new state. *)


(* Define an interpreter AI for simple arithmetic operations on the type type record
= {x:int; y:int}. The state returned by init_state () shall have both x and
y set to zero, and the return value is the value of x. The instructions are from
the type type arith_instr = Set of int | Swap | Min | Sub where for every
constructor execute will the following action:
• Set n will set x to n.
• Swap will swap x and y.
• Min sets x to the smaller value of x and y.
• Sub sets x to x - y *)
type arith_instr = Set of int | Swap | Min | Sub
type state = {x:int; y:int}


module AI = struct
  type i = arith_instr
  type s = state
  
  let init_state () ={x=0; y=0}
  let return s = s.x
  let execute s i= 
    match i with 
    | Set v-> {x=v; y=s.y}
    | Swap -> {x=s.y; y=s.x}
    | Min -> if s.x>s.y then {x=s.y; y=s.y} else s
    | Sub -> {x=s.x-s.y; y=s.y}

end



(* Define a functor CI that takes another interpreter Base and extends it with control
flow features. To do that, it is the same as Base except in what instructions it
can execute. These are now given by type i = (Base.s, Base.i) control_flow
whose elements are listed below:
• Base i executes the instruction i of the base interpreter.
• Sequence l executes the instructions in the list l in sequence.
• Condition (pred, true_case, false_case) checks the state with the predicate pred 
and then executes either the instruction true_case or false_case
depending on the result.
• Loop (pred, body) checks the state with the predicate pred and 
then executes the instruction body until the result is false.
 *)
type ('s,'i) control_flow = 
Base of 'i
| Sequence of ('s,'i) control_flow list
| Condition of ('s -> bool) * ('s,'i) control_flow * ('s,'i) control_flow
| Loop of ('s -> bool) * ('s,'i) control_flow


module CI (Base : I) = struct
type i = (Base.s,Base.i) control_flow
type s = Base.s

let init_state () = Base.init_state ()
let return s = Base.return s
let rec execute s c = 
  match c with
| Base i -> Base.execute s i
| Sequence list -> List.fold_left execute s list
| Condition (p,i1,i2) -> if p s then execute s i1
else execute s i2
| Loop (p,i) -> if p s then execute (execute s i) (Loop (p,i))
else s
end


(* Now define the interpreter ACI that is an AI extended with control flow. Define a
program gcd_prog that represents the gcd program from MiniJava (assuming that
the input arguments are known to be positive), and use it to define a function gcd
: int -> int -> int. That function should create a state with the arguments as
x and y, then execute the gcd program to eventually return the result *)

module ACI = CI (AI)

let gcd_prog (a,b) = Sequence [
                            Base (Set b);
                            Base Swap;
                            Base (Set a);
                Loop (
                  (fun s -> s.x = s.y),
                Condition(
                  (fun s -> s.x < s.y),
                Sequence [
                    Base Swap;
                    Base Sub;
                    Base Swap
                          ],
                    Base Sub)
                      )
                        ]

let gcd a b = 
  let s = ACI.init_state () in
  let s = ACI.execute s (gcd_prog (a,b)) in
ACI.return s


(* Finally define an interpreter RemoteI that takes another interpreter Base whose
operations never raise exceptions, and executes them on another thread. The interpreter should execute the same instructions as Base, but details about the state are
not exposed to the outside.
• The function init_state creates the new thread where the operations are
executed on, and then creates a new state of Base on that thread.
• Each call to execute executes the given instruction on the other thread asynchronously, without waiting for the instruction to be executed.
• return terminates the thread after retrieving the return value.
You may use use the Thread and Event module from the threads library to implement the RemoteI. *)
(* 
module RemoteI(Base:I) = struct
  open Thread
  open Event
  type i = Ret of int | Instruction of Base.i | Finish
  type s = i channel
  let init_state () = let c = new_channel() in
                        let rec serve s =
                            match sync (receive c) with
                            | Finish -> sync (send c (Ret (Base.return s)))
                            | Instruction i -> serve (Base.execute s i)
                            | _ -> serve s in
                        let _ = create serve (Base.init_state ()) in c
  let execute c i = let doit () = sync (send c (Instruction i)) in
                    let _ = create doit () in c

  let return c = let _ = sync (send c Finish) in
                  match sync (receive c) with
                  | Ret x -> x
                  | _ -> raise (Failure "return expected!")
                
  end
   *)