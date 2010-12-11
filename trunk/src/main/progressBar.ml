(** making a progress bar on an ANSI-complient terminal *)

(* width of the progress bar in characters *)
let width = 20

(* update threshold in seconds *)
let update_interval = 0.1

(* the output channel *)
let out_chan = stderr

type t = {
	(* a maximal progress state to be reached *)
	mutable max_state : int;
	(* the current state *)
	mutable current_state : int;
	(* the maximal progress so far *)
	mutable max_progress : int;
	(* the timestap of the init value *)
	mutable init_time : float;
	(* the timestamp of the last update *)
	mutable last_update_time : float;
}

(* the variable to store the global progress bar *)

let pb = {
	max_state = 0;
	current_state = 0;
	max_progress = 0;
	init_time = 0.0;
	last_update_time = 0.0;
}

let safe_int = max_int / 100

let get_status () =
	if pb.max_state > 0 then
		if pb.current_state < safe_int then
			(100 * pb.current_state) / pb.max_state
		else (100 * (pb.current_state / 100) ) / (pb.max_state / 100)
	else 0
;;

(* printing *)

let restore_cursor () =
  Printf.fprintf out_chan "%s" (String.make (width + 7) '\b')
;;

let print_bar n =	
	let k = (n * width ) / 100 in	
	Printf.fprintf out_chan
		"[%s%!%s] %3d%%"
		(String.make k '=')
		(String.make (width - k) ' ')
		n
	;		
;;

let init max_state =
	pb.max_state <- max_state;
	pb.current_state <- 0;
	pb.max_progress <- 0;
	pb.init_time <- Unix.gettimeofday ();
	pb.last_update_time <- 0.0;
	print_bar 0;
	restore_cursor ();
	flush out_chan;
;;

let update_pb () =
	let time = Unix.gettimeofday () in
	if time > pb.last_update_time +. update_interval
	then begin
		let prgr = get_status () in
		if prgr > pb.max_progress then
			pb.max_progress <- prgr;
		pb.last_update_time <- time;		
		print_bar prgr;
		restore_cursor ();
	end
;;

let step () =
	pb.current_state <- succ pb.current_state;
	update_pb ();
;;

let set_state state =
	(* the state cannot be set to max several times *)
	if pb.current_state < pb.max_state then begin
		pb.current_state <- state;
		update_pb ();
	end;
;;

let finish () =		
	print_bar 100;
	Printf.fprintf out_chan " done in %.3fs.\n"
            (Unix.gettimeofday () -. pb.init_time);
        flush out_chan;   
;;

let back () =
	pb.current_state <- pred pb.current_state;
;;

let incr_max () =
	pb.max_state <- succ pb.max_state
;;

