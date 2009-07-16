let mem_control_init () =
  let old_controls = Gc.get () in
  let new_controls = { old_controls with
    Gc.minor_heap_size = 5 * 1024 * 1024 * 8 / Sys.word_size; (* 5MB *)
    Gc.major_heap_increment = 10 * 1024 * 1024 * 8 / Sys.word_size; (* 10MB *)
    Gc.max_overhead = 10000;
    Gc.space_overhead = 200;
  } in
  Gc.set new_controls

let print_memory_usage () =
  let stat = Gc.stat () and control = Gc.get () in
  let max_words_total = stat.Gc.heap_words + control.Gc.minor_heap_size in
  let bytes = ( max_words_total * ( Sys.word_size / 8) ) in
  let kbytes = (bytes / 1024) in
  let mbytes = (kbytes / 1024) in
  Printf.fprintf stderr "Allocated memory:\t%d Mbytes %d kBytes\n"
    mbytes (kbytes - mbytes * 1024)

let print_cpu_time () =
  Printf.fprintf stderr "CPU time:\t\t%.3fs\n" (Sys.time ())

module W = Weakhashset.Make (struct
    type t = int list
    let rec hash = function
      | [] -> 0
      | hd :: tl -> hd + hash tl
    let equal l1 l2 = List.for_all2 (=) l1 l2
  end)

(*|module W = Weak.Make (struct                 *)
(*|    type t = int list                        *)
(*|    let rec hash = function                  *)
(*|      | [] -> 0                              *)
(*|      | hd :: tl -> hd + hash tl             *)
(*|    let equal l1 l2 = List.for_all2 (=) l1 l2*)
(*|  end)                                       *)

(* the size of array *)
let n = 1000000
(* the number of different elements *)
let m = 100 * n
(* the number of iterations *)
let k = 100000000

let _ =
  (*|  mem_control_init ();*)
  
  let data = Array.create n [] in
  let whs = W.create n in
  for i = 0 to k - 1 do
    let j = Random.int m in
    let l = W.merge whs [j] in
    let ll = W.merge whs [j] in
    assert (l == ll);
    data.(Random.int n) <- l;
    if (i mod 1000000 = 0) then
      begin
        (*|        Gc.full_major ();*)
        Printf.printf "%n\t" (i / 1000000);
        Printf.fprintf stderr "%.3fs\t" (Sys.time ());
        print_memory_usage ();
        flush_all ()
      end;
    if (i mod n = 0) then
      for j = 0 to n - 1 do
        let l = data.(j) in
        match l with
        | [p] ->
            let ll = W.merge whs [p] in
            assert (l == ll);
        | _ -> ()
      done;
  done;
  
  print_memory_usage ();
  print_cpu_time ()