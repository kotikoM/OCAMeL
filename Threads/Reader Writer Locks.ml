

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




(* 2 *)
(* Now implement the function new_rwlock which creates a fresh rwlock. For the rwlock, 
   a server thread should be started which reacts to acquire demands. 
   The server thread maintains the number of admitted readers in its parameter and is informed whenever a reader releases its lock. 
   Only when the number of readers has dropped to 0, a writer may acquire the lock. 
   Only when such a writer has released its lock, the stage is open for potential readers. *)