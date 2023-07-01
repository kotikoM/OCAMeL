
(* WRITE IN UTOP *)
(* #thread;;
#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;
 *)


(* A future represents the result of an asynchronous computation. 
   Imagine a time-consuming operation, that is relocated to another thread, 
   then the main thread keeps some kind of "handle" to check whether the operation in the other thread has finished, 
   to query the result or as a means to do other operations with the result. This "handle" is what we call a future. *)

(* Implement a module Future with a type 'a t that represents a future object. Furthermore, perform these tasks: *)

module Future = struct open Thread open Event

type 'a msg = Result of 'a | Ex of exn  

type 'a t = 'a msg channel 

(* 1 *)
(* Implement create : ('a -> 'b) -> 'a -> 'b t, 
that applies the function given as the first argument to the data given as second argument
in a separate thread. A future for the result of this operation is returned. *)

let create f a =
   let ch = new_channel () in
   let task () = 
      let r = try (Result (f a)) with e -> Ex e 
   in sync (send ch r) 
   in
   let _ = create task () in 
   ch

(* 2 *)
(* Implement get : 'a t -> 'a that waits for the asynchronous operation to finish and returns the result. *)

let get c = match sync (receive c) with 
| Result r ->  r
| Ex e -> raise e



(* 3 *)
(* Implement then_ : ('a -> 'b) -> 'a t -> 'b t 
such that a call then_ f x returns a future that represents the result of applying f 
to the result of the computation referred to by x. 
The application of f must again be asynchronous, so then_ must not block! *)

let tjen_ f c = failwith "t"



(* 4 *)
(* Extend your implementation with exception support, such that if an asynchronous operation throws, 
this exception is passed to the thread where the future resides and can be caught there. *)





(* 5 *)
(* Implement when_any : 'a t list -> 'a t 
that constructs a future that provides its result once any of the given futures has finished its computation. 
Make sure that \mio{when_any} does not block! *)

let when_any cs = failwith "t" 



(* 6 *)
(* Implement when_all : 'a t list -> 'a list t 
that constructs a future that corresponds to a list of all the results of the given futures. *)

let when_all cs = failwith "t"



(* 7 *)
(* Find additional useful functions for the Future module and implement them. *)








end