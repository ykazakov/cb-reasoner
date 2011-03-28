(* wrapping the project tracker together with all relevant functions *)
type t = {
	(* starting the progress monitor with a message *)
	start : string -> int -> unit;
	(* increment the current progress value *)
	step: ?step: int -> unit -> unit;
	(* decrement the current status value *)
	back: unit -> unit;
	(* jumping to a particular status value *)
	jump : int -> unit;
	(* increment the maximal status value *)
	incr_max: unit -> unit;
	(* jumping to the maximum progress *)
	jump_max: unit -> unit;
	(* finishing *)
	finish : unit -> unit;
}

val start : t list -> string -> int -> unit
val step : ?step: int -> t list -> unit
val back : t list -> unit
val jump : t list -> int -> unit
val incr_max : t list -> unit
val jump_max : t list -> unit
val finish : t list -> unit

val of_progress_monitor : Progress_monitor.t -> t