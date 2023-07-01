open Thread
open Event
(* In this assignment you are going to simulate a server 
where users can post their own blog.
As a blog we consider a list of posts (strings).
We define these types: *)

type blog = string list
type user = string
type pass = string
type message = Post of user * pass * string
             | Read of user * blog channel
type t = message channel

(* Clients communicate with the server using messages through a single channel: *)
(* A user can publish a new post on her blog by sending a Post message to the server. 
   The message has to contain the user’s name, password and text to be published. 
   If username and password are correct, the server appends the text to the user’s blog. 
   Messages with incorrect credentials or non-existing users are simply ignored. *)

(* To read a user’s blog, a Read message with the corresponding user has to be sent to the server. 
   Furthermore, the message has to contain a channel on which the server sends the requested blog 
   or an empty list if no such user exists. *)

(* Implement these functions: *)

(* start_server : (user * pass) list -> t starts a server on its own thread. 
   As an argument the function receives the list of registered users and their corresponding passwords. *)

  
   let start_server users = 
    let c = new_channel () in
    let rec server_fun (blogs : (user * blog) list) =
      let get_post user = match List.assoc_opt user blogs with
      | Some b -> b 
      | None -> []
    in
      match sync (receive c) with 
      | Post (user, pass, txt) ->
      if List.assoc_opt user users = Some pass then
        server_fun ((user, (get_post user) @ [txt])::(List.remove_assoc user blogs)) 
      else server_fun blogs
      | Read (user, answer_c) -> 
        sync (send answer_c (get_post user));
        server_fun blogs
    in 
    let _ = create server_fun [] in
    c



(* post : t -> user -> pass -> string -> unit publishes a new blog post (last argument) in the given users' post. *)

  let post s u p t = 
    sync (send s (Post (u, p, t)))



(* read : t -> user -> blog requests a user’s blog from the server. *)

  let read s u = 
    let ch = new_channel () in 
    sync (send s (Read (u, ch)));
    sync (receive ch)




let test =
  let s = start_server [("userA", "passA"); ("userB", "passB")] in
  post s "userB" "passB" "Welcome to my OCaml blog.";
  post s "userA" "passA" "My name is A and I'm starting my own blog!";
  post s "userB" "12345" "I am a hacker attacking B's blog now!";
  post s "userB" "passB" "You can have threads in OCaml!";
  read s "userB"
(* returns:
["Welcome to my OCaml blog."; "You can have threads in OCaml!"]
*) 