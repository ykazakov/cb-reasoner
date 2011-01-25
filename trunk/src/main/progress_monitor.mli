(* project monitor functions *)
type t = {
	(* start with input message *)
	start : string -> unit;
	(* report the current progress modulo maximal *)
	report : int -> int -> unit;
	(* finish progress bar *)
	finish : unit -> unit;
}

val of_out_channel : ?width: int -> ?message_width: int ->
?update_interval: float -> ?progress_thrashold: float ->
out_channel -> t