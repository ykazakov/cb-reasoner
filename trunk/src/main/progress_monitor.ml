(* project monitor functions *)
type t = {
	(* start with input message *)
	start : string -> unit;
	(* report the current progress modulo maximal *)
	report : int -> int -> unit;
	(* finish progress bar *)
	finish : unit -> unit;
}

(* progress monitor state *)
type state = {
	(* id of the current progress *)
	mutable id: int;
	(* the cached start time of the bar *)
	mutable start_time: float;
	(* last time the bar has been updated *)
	mutable last_update_time: float;
	(* the maximum progress (between 0 and 1) achieved thus far *)
	mutable last_progress: float;
}

let of_out_channel ?(width = 20) ?(message_width = 25) ?(update_interval = 0.1) ?(progress_thrashold = 0.01) out_chan =
	(* printing of the bar *)
	let restore_cursor () =
		Printf.fprintf out_chan "%s" (String.make (width + 7) '\b')
	in
	let float_width = float_of_int width in
	let print_bar progress =
		let k = int_of_float (progress *. float_width) in
		Printf.fprintf out_chan
			"[%s%!%s] %3d%%"
			(String.make k '=')
			(String.make (width - k) ' ')
			(int_of_float (progress *. 100.0))
	in
	let pm = {
		id = 0;
		start_time = 0.0;
		last_update_time = 0.0;
		last_progress = 0.0;
	}
	in {	
		start = (fun message ->
						let time = Unix.gettimeofday () in
						pm.id <- succ pm.id;
						pm.start_time <- time;
						pm.last_update_time <- time;
						pm.last_progress <- 0.0;
						Printf.fprintf out_chan "%2n. %-*s" pm.id message_width message
			);
		report = (fun state max ->
						let time = Unix.gettimeofday () in
						let progress =
							if max = 0 then 0.0
							else float_of_int (state) /. float_of_int (max)
						in
						if (time > pm.last_update_time +. update_interval) &&
						(progress > pm.last_progress +. progress_thrashold) then begin
							pm.last_update_time <- time;
							pm.last_progress <- progress;
							print_bar progress;
							restore_cursor ();
						end
			);
		finish = (fun () ->
						print_bar 1.0;
						Printf.fprintf out_chan " done in %.3fs.\n"
							(Unix.gettimeofday () -. pm.start_time);
						flush out_chan
			);
	}