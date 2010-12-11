(** making a progress bar on an ANSI-complient terminal *)

(* initialize the progress bar with the maximal status value *)
val init: int -> unit
(* increment the current status value *)
val step: unit -> unit
(* setting the current status value *)
val set_state : int -> unit
(* finishing the progress bar and printing the time *)
val finish : unit -> unit
(* decrement the current status value *)
val back: unit -> unit
(* increment the maximal status value *)
val incr_max: unit -> unit
(* get the status of the progress bar: returns an integer between 0 and 100 *)
val get_status : unit -> int