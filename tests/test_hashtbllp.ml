(** Testing module [hashsetlp.ml] *)
open OUnit

(* canonincal implementation against which the test is performed *)

module H = Hashtbl.Make (struct
    type t = int
    let hash x = x
    let equal = (==)
  end)

module Hlp = Hashtbllp.Make (struct
    type t = int
    let hash x = x
    let equal = (==)
  end)

(* ---------------- settings ------------------ *)

let print_memory_usage () =
  let stat = Gc.stat () and control = Gc.get () in
  let max_words_total = stat.Gc.heap_words + control.Gc.minor_heap_size in
  let bytes = ( max_words_total * ( Sys.word_size / 8) ) in
  let kbytes = (bytes / 1024) in
  let mbytes = (kbytes / 1024) in
  Printf.fprintf stderr "Allocated memory:\t%d Mbytes %d kBytes\n"
    mbytes (kbytes - mbytes * 1024)
;;

let print_cpu_time () =
  Printf.fprintf stderr "CPU time:\t\t%.3fs\n" (Sys.time ())
;;

let time_stamp = ref 0.0

let update_time () =
  let curr_time = Sys.time () in
  Printf.fprintf stderr "\ttime: %.3fs\t" (curr_time -. !time_stamp);
  flush stderr;
  time_stamp := curr_time
;;

let _ =
  let old_controls = Gc.get () in
  let new_controls = { old_controls with
    Gc.minor_heap_size = 4 * 1024 * 1024 * 8 / Sys.word_size; (* 4MB *)
    Gc.major_heap_increment = 8 * 1024 * 1024 * 8 / Sys.word_size; (* 8MB *)
    Gc.max_overhead = 100000;
    Gc.space_overhead = 400;
  } in
  Gc.set new_controls

(* ---------------- preparations------------------ *)

let n = 1000000        
let h1 = H.create 1
let h2 = H.create 1
let hs1 = Hlp.create 1
let hs2 = Hlp.create 1

let generate () =
  Printf.fprintf stderr "Preparing tests...";
  flush stderr;
  
  for i = 1 to n do
    let k = Random.int n in
    let x = Random.int n in
    H.add h1 k x;
    Hlp.add hs1 k x;
    let k = Random.int n in
    let x = Random.int n in
    H.add h2 k x;
    Hlp.add hs2 k x;
  done;
  update_time ();
  Printf.fprintf stderr "done\n";
  flush stderr
;;

(* ----------------- Sanity Checking ---------------------- *)

(* testing length *)
let test_length () =
  assert_equal ~msg:"test 1" (H.length h1) (Hlp.length hs1);
  assert_equal ~msg:"test 2" (H.length h2) (Hlp.length hs2);
;;

(* testing mem *)
let test_mem () =
  for i = 1 to n do
    let k = Random.int n in
    assert_equal ~msg:"test 1" (H.mem h1 k) (Hlp.mem hs1 k);
    assert_equal ~msg:"test 2" (H.mem h2 k) (Hlp.mem hs2 k);
  done;
  H.iter (fun k _ -> assert_bool "test 3" (Hlp.mem hs1 k)) h1;
  H.iter (fun k _ -> assert_bool "test 4" (Hlp.mem hs2 k)) h2;
;;

(* testing find and remove *)
let test_find_remove () =
  let h = H.copy h1 in
  let hs = Hlp.copy hs1 in
  for i = 1 to n do
    let k = Random.int n in
    begin try
      let x = Hlp.find hs k in
      assert_equal ~msg:"test 1" (H.find h k) x;
      H.remove h k;
      Hlp.remove hs k;
    with Not_found -> assert_equal ~msg:"test 2" (H.mem h1 k) (Hlp.mem hs1 k);
    end;
  done;
  assert_equal ~msg:"test 3" (H.length h) (Hlp.length hs);
  (* emptying *)
  H.iter (fun k x ->
          let y = Hlp.find hs k in
          assert_equal ~msg:"test 4" x y;
          Hlp.remove hs k;
    ) h;
  assert_equal ~msg:"test 5" (Hlp.length hs) 0;
;;

(* testing find_all *)
let test_find_all () =
  H.iter (fun k _ ->
          let l = H.find_all h1 k in
          let ls = Hlp.find_all hs1 k in
          assert_bool "test 1" (List.for_all2 (fun x y -> x = y) l ls);
    ) h1;
  H.iter (fun k _ ->
          let l = H.find_all h2 k in
          let ls = Hlp.find_all hs2 k in
          assert_bool "test 2" (List.for_all2 (fun x y -> x = y) l ls);
    ) h2;
;;

(* testing add, remove and replace *)
let test_add_remove_replace () =
  let h = H.create 1 in
  let hs = Hlp.create 1 in
  for j = 1 to 1000 do
    for i = 1 to n / 1000 do
      let k = Random.int n in
      let x = Random.int n in
      begin match Random.int 3 with
        | 1 ->
            H.add h k x;
            Hlp.add hs k x;
        | 2 ->
            H.remove h k;
            Hlp.remove hs k;
        | _ ->
            H.replace h k x;
            Hlp.replace hs k x
      end;
    done;
    assert_equal ~msg:"test 1" (H.length h) (Hlp.length hs);
    (* emptying *)
    H.iter (fun k x ->
            let y = Hlp.find hs k in
            assert_equal ~msg:"test 4" x y;
            Hlp.remove hs k;
      ) h;
    assert_equal ~msg:"test 5" (Hlp.length hs) 0;
    H.clear h;
  done;
;;

(* testing fold *)
let test_fold () =
  assert_equal ~msg:"test 1"
    (H.fold (fun k x s -> k * x + s) h1 0)
    (Hlp.fold (fun k x s -> k * x + s) hs1 0);
  assert_equal ~msg:"test 2"
    (H.fold (fun k x s -> k * x + s) h2 0)
    (Hlp.fold (fun k x s -> k * x + s) hs2 0);
;;

let sanity = "Sanity checking" >::: [
  "test_length" >:: test_length;
  "test_mem" >:: test_mem;
  "test_find_remove" >:: test_find_remove;
  "test_find_all" >:: test_find_all;
  "test_add_remove_replace" >:: test_add_remove_replace;
  "test_fold" >:: test_fold;
  ]

let _ =
  generate ();
  let verbose = ref true in
  if not (was_successful (run_test_tt ~verbose:!verbose sanity)) then
    exit 1;
  print_memory_usage ();
  print_cpu_time ();