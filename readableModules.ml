
(* We define the signature: *)

module type Readable = sig
  type t (* reading state *)
  type arg (* what is being read *)
  val begin_read : arg -> t
  val end_read : t -> unit
  val at_end : t -> bool
  val read_line : t -> (t * string)
  end

  (* to describe a source from which we can read text line by line. 
  Notice, that the arg type is a means for the implementation to 
  specifiy the argument it needs to initialize reading with begin_read. 
  While at_end checks whether reading reached the end of the input,
   read_line is used to read the next line (text until next newline character '\n') 
   and move the reading position forward. *)


(* 1 *)
(* Implement a module ReadableString, that is of type Readable and is used to read from a string.
 The string is given to begin_read. *)

 module ReadableString : Readable with 
 type t=string*int and type arg=string = struct
  type t = string * int (* current string and position *)
  type arg = string

  let begin_read (input : string) : t = (input, 0) (* start from the first char *)
  let end_read (_ : t) : unit = ()
  let at_end (input , position) = position >= String.length input || input.[position] = '\n'
  let read_line (input, position) =
    (* cycle through the string until the end or \n and
        finally return read string *)
  let rec read_line_aux acc i =
    if i >= String.length input || input.[i] = '\n' then
        ((input, i + 1), String.concat "" (List.rev acc))
    else
        read_line_aux ((String.sub input i 1) :: acc) (i + 1)
    in
    read_line_aux [] position
end


      

 (* 2 *)
 (* Implement a module ReadableFile, that is of type Readable and allows to read from a file. 
 The name of the file is given to begin_read. *)

 module ReadableFile : Readable with 
 type t=in_channel and type arg=string = struct
  type t = in_channel (* open_in returns in_channel *)
  type arg = string 
  
  let begin_read fileName = open_in fileName
  let end_read fileChannel = close_in fileChannel 
  let at_end inChannel = 
    try 
    ignore (input_line inChannel);
    false 
  with
  End_of_file -> true 
  let read_line inChannel = 
    let line = input_line inChannel in
  (inChannel, line)

end



(* 3 *)
(* Implement a functor Reader that extends a given Readable such that all types 
and values of the Readable are also available on the Reader. 
Furthermore, it provides a function read_all : t -> (t * string)
that reads the entire text that is available. *)

module Reader (R : Readable) : 
sig 
include Readable with type t=R.t and type arg=R.arg
val read_all : t -> (t*string)
end = struct
 include R

  let read_all state =
    let rec read_all_lines acc state = 
      if at_end state then 
        (state, String.concat "\n" (List.rev acc)) 
    else
      let (new_state, line) = read_line state in 
      read_all_lines (line::acc) new_state
    in read_all_lines [] state 

end 




(* Testing *)

let s = "A tempString\nNoo"
let s1 = ReadableString.begin_read "A tempString\nNoo"
let e = ReadableString.at_end s1
let s1, l1 = ReadableString.read_line s1
let s1, l2 = ReadableString.read_line s1
let e = ReadableString.at_end s1 




let f1 = ReadableFile.begin_read "input.txt"
let f1, l1 = ReadableFile.read_line f1
let f1, l2 = ReadableFile.read_line f1
