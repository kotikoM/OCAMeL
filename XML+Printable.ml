

(* 1 *)
(* Consider the following datatype definitions for XML document *)

type tag = A | B | C

type 't xml = 
  Text of string 
| Comment of 't xml
| XML of 't * 't xml list

type 't query = Simple of 't | NotSimple of 't * string

(* Thus, e.g., *)

let doc = XML (A, [Comment (XML (C,[]));XML (B, [Text "Hello!"; XML (A, [])])])

let q = NotSimple (A, "Hello!")

(* are values of types tag xml and tag query, respectively.
Implement the following functions *)

(* val contains: string -> 't xml -> bool
val search: 't query -> 't xml -> 't xml list
val open_tag: tag -> string
val closing_tag: tag -> string
val string_of: tag xml -> string *)

(* so that, e.g.,
> contains "Hello!" doc;;
> -: bool = true
> search (Simple B) doc;;
> -: tag xml list = [XML (B, [Text "Hello!"; XML (A, [])])]
> open_tag A;;
> -: string = "<A>"
> closing_tag A;;
> -: string = "</A>"
> string_of (XML (C, [Text "ab"; Comment (Text "cd"); Text "ef"]));;
> -: string = "<C>abef</C>"
 *)

 (* More precisely,
(a) The function contains returns true for a string s and an XML document d
if d contains a text element Text s outside comments.
(b) The function search for a simple query Simple t and an XML document d
returns the list of all elements XML (t’,l) occurring in d (possibly including d)
outside comments where t = t′ holds. For a query NotSimple (t, s), it returns
the list of all elements XML (t′, l) occurring in d (possibly including d) outside
comments which contain occurrences of an element Text s outside comments
so that additionally t = t′ holds. *)


let rec contains str xml = 
  match xml with 
  | Text v -> v = str
  | Comment xml' -> false
  | XML (v, xml'') -> List.fold_left (fun acc x -> acc || (contains str x)) false xml''

  let rec containss str xml = 
    match xml with 
    | Text v -> v = str
    | Comment xml' -> false
    | XML (v, xml'') -> List.exists (contains str) xml''


let rec search q d = 
  match q with 
  | NotSimple (t, s) -> List.filter (fun x -> contains s x) (search (Simple t) d)  
  | Simple t -> (
          match d with 
          | Text v -> []
          | Comment _ -> []
          | XML (t', d') -> if t=t' then XML(t', d')::List.concat (List.map (fun x -> search q x) d') 
                                    else List.concat (List.map (fun x -> search q x) d'))
            
             


let rec open_tag tg = "<" ^ (
  match tg with 
  | A -> "A"
  | B -> "B"
  | C -> "C"

) ^ ">"

let rec closing_tag tg = "<" ^ (
  match tg with 
  | A -> "A"
  | B -> "B"
  | C -> "C"

) ^ "/>"

let rec string_of xml = 
  match xml with 
  | Text st -> st
  | Comment _ -> ""
  | XML (t, xml') -> open_tag t ^ String.concat "" (List.map (fun x -> string_of x) xml') ^closing_tag t






(* 2 *)
(* In this exercise you will implement modules for printing values to the standard output.
These modules have the following signature:  *)

module type Printable = sig
  type t
  val print : t -> unit
  val length : t -> int
  end
  
(* Here, t is the type of the values that can be printed by the module. print will print
the value to the standard output. length returns the number of chars that printing the
provided value would produce. The concrete type of t must be visible to the outside for
all Printable modules. Calling length must not have any side-effects. If and how often
the standard output channel is flushed does not matter *)


(* Implement a module PrintableString that takes a string and prints it as-is –
without adding newlines or otherwise modifying it. *)

module PrintableString : Printable with type t=string = struct
  type t = string

  let print t = print_string t
  let length t = String.length t

end


(* Do the analogous module PrintableInt for the type int. *)

module PrintableInt : Printable with type t=int = struct
  type t = int

  let print t = print_int t
  let length t = String.length (string_of_int t)

end


(* Implement the functor PrintableList that takes a Printable and then prints a
list of values by printing each element in the order of the list. Each value should be
followed by a newline. For this exercise, a newline is the single char \n, regardless
of the operating system. *)

module PrintableList (P : Printable) : Printable with type t=P.t list= struct
  type t = P.t list

  let print lst = List.iter (fun x -> P.print x; print_char '\n') lst
  
  let length lst = List.fold_left (fun acc x -> acc + (P.length x)) 0 lst

end


(* Implement the functor PrintablePair that takes modules A and B, 
both implementing Printable, and provides a Printable for the pair A.t * B.t by printing
the values in order, seperated by a space. *)

module PrintablePair (A:Printable) (B:Printable) : Printable with type t = A.t* B.t  = struct
  type t = A.t * B.t

  let print (t', t'') = (A.print t'); (print_string " "); (B.print t'')

  let length (t', t'') = (A.length t') + (B.length t'') + 1

end


(* Implement the functor PrintableWarnings that takes a Printable called Warning,
and combines it together with some module Location so that the resulting module
is a Printable for the type (Location.t * Warning.t) list. *)

module PrintableWarnings  (Location : Printable)  (Warning : Printable) : Printable with type t = (Location.t* Warning.t) list = struct
  
  type t = (Location.t * Warning.t) list


  let print lst = List.iter (fun (loc, warn) -> Location.print loc;  print_string ":"; Warning.print warn; print_newline () ) lst
  
  let length lst = List.fold_left (fun acc (loc, warn) -> acc + (Warning.length warn) + (Location.length loc) + 1) 0 lst

end


(* Define the module StringWarnings by using PrintableString and PrintableInt
as argument modules Warning and Location, respectively, for the functor PrintableWarnings.
Then use it to write a function warn : unit -> unit that prints two warnings with
text and location of your choice *)

module StringWarnings = PrintableWarnings(PrintableInt)(PrintableString) 

let warn () =
  let warnings = [(1, "A"); (2, "B")] in
  StringWarnings.print warnings