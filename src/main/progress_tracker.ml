module PM = Progress_monitor

(* wrapping the progress tracker together with all relevant functions *)
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

(* useful functions for lists of progress trackers *)
let start pt_lst message max = 
	List.iter (fun pt -> pt.start message max) pt_lst
let step ?(step = 1) pt_lst = 
	List.iter (fun pt -> pt.step ~step: step ()) pt_lst
let back pt_lst =
	List.iter (fun pt -> pt.back ()) pt_lst
let jump pt_lst state =
	List.iter (fun pt -> pt.jump state) pt_lst
let incr_max pt_lst =
	List.iter (fun pt -> pt.incr_max ()) pt_lst
let jump_max pt_lst =
	List.iter (fun pt -> pt.jump_max ()) pt_lst
let finish pt_lst =
	List.iter (fun pt -> pt.finish ()) pt_lst

type state = {
	(* a maximal progress state to be reached *)
	mutable max_state : int;
	(* the current state *)
	mutable current_state : int;
}

let of_progress_monitor pm =
	let pt = {
		max_state = 0;
		current_state = 0;
	}
	in
	let report () =
		pm.PM.report pt.current_state pt.max_state
	in {
		start = (fun message max ->
						pt.max_state <- max;
						pt.current_state <- 0;
						pm.PM.start message
			);
		step = (fun ?(step = 1) () ->
						pt.current_state <- pt.current_state + step;
						report ()
			);
		back = (fun () ->
						pt.current_state <- pred pt.current_state;
						report ()
			);
		jump = (fun state ->
						pt.current_state <- state;
						report ()
			);
		incr_max = (fun () ->
						pt.max_state <- succ pt.max_state;
						report ()
			);
		jump_max = (fun () ->
						pt.current_state <- pt.max_state;
						report ()
			);
		finish = (fun () -> pm.PM.finish ());
	}