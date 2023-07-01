

open Thread
open Event




(* A reader-writer lock (rwlock for short) allows multiple readers to enter a critical section or a single writer. 
A module providing such lock therefore should satisfy the following module type: *)

module type RW = sig
  type ack 
  type rwlock 

  val acquire_read  : rwlock -> ack
  val acquire_write : rwlock -> ack
  val release       : ack -> unit
  val new_rwlock    : unit -> rwlock
end



(* Provide an implemention of a module RW with this module type. *)

(* 1 *)
(* Realize an rwlock as a pair of two channels, one for read requests, one for write requests 
together with corresponding functions acquire_read, acquire_write and release. *)

module RW : RW = struct

  type ack = unit channel

  (* Read * Write *)
  type rwlock = (ack channel * ack channel) 

  let acquire_read (r, _) = 
    let ack = new_channel () in
    sync (send r ack);
    ack

  let acquire_write (_, w)= 
  let ack = new_channel () in
  sync (send w ack);
  ack
  
  let release ack = 
    sync (send ack ())




  (* 2 *)
(* Now implement the function new_rwlock which creates a fresh rwlock. For the rwlock, 
   a server thread should be started which reacts to acquire demands. 
   The server thread maintains the number of admitted readers in its parameter and is informed whenever a reader releases its lock. 
   Only when the number of readers has dropped to 0, a writer may acquire the lock. 
   Only when such a writer has released its lock, the stage is open for potential readers. *)

  let new_rwlock = fun () -> (
    let r = new_channel () in
    let w = new_channel () in

    let count = new_channel () in

    let rec server x =
      if x = 0 then select [

        wrap (receive w) (fun c -> sync (receive c); server 0);
        wrap (receive r) (fun c -> let _ = create  read_server c in server 1)

      ] else select [

        wrap (receive count) (fun () -> server (x-1));
        wrap (receive r) (fun c -> let _ = create  read_server c in server 1)

      ]
      and 
      read_server c = sync (receive c);
                      sync (send count ())

    in
    let _ = create server 0 in
    (r, w)
  )

  let main = let l1 = new_rwlock () in
            let l2 = new_rwlock () in 
            let th (l1, l2) = let a1 = acquire_read l1 in
            let _ = delay 1.0 in
            let a2 =acquire_read l2 in
            release a2;
            release a1;
            print_int (id (self ()));
            print_string " finished\n"
  in let t1 = create th (l1, l2)
  in let t2 = create th (l2, l1)
  in join t1;
  join t2 
  
end


