(* Testing the functions of the module ints against the module set *)
open OUnit
open Consed.T

(* canonincal implementation against which the test is performed *)
module M = Map.Make (struct
    type t = int consed
    let compare i1 i2 = compare i1.data i2.data
  end)

module H = Chashmap.Make (struct
  type t = int
  end)
module IM = H.Map

module C = Consed.Make (struct
    type t = int
    let hash (i: int) = i
    let equal (i1: int) (i2: int) = (i1 = i2)
  end )

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

(* ---------------- preparations------------------ *)

let n = 500000
let m1 = ref M.empty
let m2 = ref M.empty
let im1 = IM.create 13
let im2 = IM.create 13

let generate () =
  Printf.fprintf stderr "Preparing tests...";
  flush stderr;
  for i = 1 to n do
    let k = C.cons (Random.int n) in
    let x = (Random.int n) + 1 in (* positive values *)
    m1 := M.add k x !m1;
    IM.add im1 k x;
  done;
  for i = 1 to 30 do
    let k = C.cons (Random.int n) in
    let x = (Random.int n) + 1 in (* positive values *)
    m2 := M.add k x !m2;
    IM.add im2 k x;
  done;
  update_time ();
  Printf.fprintf stderr "done\n";
  flush stderr
;;

(* ----------------- CORRECTNESS SUIT ---------------------- *)

(* testing mem and iter *)
let test_mem () =
  for i = 1 to n do
    let k = C.cons (Random.int n) in
    assert_equal ~msg:"test 1" (M.mem k !m1) (IM.mem im1 k);
    assert_equal ~msg:"test 2" (M.mem k !m2) (IM.mem im2 k);
  done;
  update_time ();
;;

let test_find_iter () =
  IM.iter (fun k x ->
          assert_equal ~msg:"test 1" (M.find k !m1) x
    ) im1;
  M.iter (fun k x ->
          assert_equal ~msg:"test 2" (IM.find im1 k) x
    ) !m1;
  IM.iter (fun k x ->
          assert_equal ~msg:"test 3" (M.find k !m2) x
    ) im2;
  M.iter (fun k x ->
          assert_equal ~msg:"test 4" (IM.find im2 k) x
    ) !m2;
  update_time ();
;;

(*|(* testing process *)                                              *)
(*|let test_process () =                                              *)
(*|  let im = ref IM.empty in                                         *)
(*|  for i = 1 to n do                                                *)
(*|    let k = C.cons (Random.int n) in                               *)
(*|    im := IM.process k (function                                   *)
(*|        | None -> assert_bool "test 1" (not (IM.mem k !im)); Some 0*)
(*|        | Some _ -> assert_bool "test 2" (IM.mem k !im); Some 1    *)
(*|      ) !im;                                                       *)
(*|    assert_bool "test 3" (IM.mem k !im);                           *)
(*|  done;                                                            *)
(*|  update_time ();                                                  *)
(*|;;                                                                 *)
(*|                                                                   *)
(*|let test_process () =                                              *)
(*|  let im = ref !im1 in                                             *)
(*|  IM.iter (fun key x ->                                            *)
(*|          im := IM.process key (function                           *)
(*|              | None -> Some x                                     *)
(*|              | Some y -> Some (x + y)                             *)
(*|            ) !im;                                                 *)
(*|    ) !im2;                                                        *)
(*|  IM.iter (fun key x ->                                            *)
(*|          im := IM.process key (function                           *)
(*|              | None -> assert_bool "test 1" false; None           *)
(*|              | Some y -> if x == y then None else Some (y - x)    *)
(*|            ) !im;                                                 *)
(*|    ) !im1;                                                        *)
(*|  assert_bool "test 2" (IM.equal (=) !im !im2);                    *)
(*|  update_time ();                                                  *)
(*|;;                                                                 *)

(* testing remove *)
let test_remove () =
  let m = ref !m1 in
  let im = IM.copy im1 in
  for i = 1 to n do
    let k = C.cons (Random.int n) in
    m := M.remove k !m;
    IM.remove im k;
  done;
  M.iter ( fun k x ->
          assert_equal ~msg:"test 1" (IM.find im k) x
    ) !m;
  IM.iter ( fun k x ->
          assert_equal ~msg:"test 2" (M.find k !m) x
    ) im;
  update_time ();
;;

(*|(* testing replace *)                                                                      *)
(*|let test_replace () =                                                                      *)
(*|  let im = ref !im1 in                                                                     *)
(*|  let m = ref !m1 in                                                                       *)
(*|  for i = 1 to n do                                                                        *)
(*|    let k = C.cons (Random.int n) in (                                                     *)
(*|      try                                                                                  *)
(*|        im := IM.replace k 0 !im;                                                          *)
(*|        assert_bool "test 1" (M.mem k !m);                                                 *)
(*|      with Not_found -> assert_bool "test 2" (not (M.mem k !m))                            *)
(*|    );                                                                                     *)
(*|    if M.mem k !m then (                                                                   *)
(*|      assert_equal ~msg:"test 4" (IM.find k !im) 0;                                        *)
(*|      m := M.add k 0 !m;                                                                   *)
(*|    )                                                                                      *)
(*|  done;                                                                                    *)
(*|  M.iter ( fun k x ->                                                                      *)
(*|          assert_equal ~msg:"test 5" (IM.find k !im) x                                     *)
(*|    ) !m;                                                                                  *)
(*|  IM.iter ( fun k x ->                                                                     *)
(*|          assert_equal ~msg:"test 6" (M.find k !m) x                                       *)
(*|    ) !im;                                                                                 *)
(*|  update_time ();                                                                          *)
(*|;;                                                                                         *)
(*|                                                                                           *)
(*|(* testing equal *)                                                                        *)
(*|let test_equal () =                                                                        *)
(*|  assert_bool "test 1" (IM.equal (=) !im1 !im1);                                           *)
(*|  let im = ref IM.empty in                                                                 *)
(*|  M.iter (fun k x -> im := IM.add k x !im) !m1;                                            *)
(*|  assert_bool "test 2" (IM.equal (=) !im !im1);                                            *)
(*|  let im = ref IM.empty in                                                                 *)
(*|  IM.iter (fun key x -> im := IM.add key x !im) !im1;                                      *)
(*|  assert_bool "test 3" (IM.equal (=) !im !im1);                                            *)
(*|  update_time ();                                                                          *)
(*|;;                                                                                         *)
(*|                                                                                           *)
(*|(* testing union *)                                                                        *)
(*|let test_union () =                                                                        *)
(*|  assert_bool "test 1" (IM.equal (=) (IM.union (fun x _ -> x) !im1 IM.empty) !im1);        *)
(*|  assert_bool "test 2" (IM.equal (=) (IM.union (fun x _ -> x) IM.empty !im1) !im1);        *)
(*|  assert_bool "test 3" (IM.equal (=) (IM.union (fun x y -> (x + y) / 2) !im1 !im1) !im1);  *)
(*|  let im = IM.union (fun x y -> x + y) !im1 !im2 in                                        *)
(*|  assert_bool "test 4" (IM.equal (=) im (IM.union (fun x y -> x + y) !im2 !im1));          *)
(*|  IM.iter (fun k x ->                                                                      *)
(*|          let y =                                                                          *)
(*|            (try M.find k !m1 with Not_found -> 0)                                         *)
(*|            +                                                                              *)
(*|            (try M.find k !m2 with Not_found -> 0)                                         *)
(*|          in assert_equal ~msg:"test 5" x y                                                *)
(*|    ) im;                                                                                  *)
(*|  let m = ref !m1 in                                                                       *)
(*|  M.iter (fun k x ->                                                                       *)
(*|          m := M.add k (x + try M.find k !m with Not_found -> 0) !m                        *)
(*|    ) !m2;                                                                                 *)
(*|  M.iter ( fun k x ->                                                                      *)
(*|          assert_equal ~msg:"test 6" (IM.find k im) x                                      *)
(*|    ) !m;                                                                                  *)
(*|  update_time ();                                                                          *)
(*|;;                                                                                         *)
(*|                                                                                           *)
(*|(* testing inrsection *)                                                                   *)
(*|let test_inter () =                                                                        *)
(*|  assert_bool "test 1" (IM.equal (=) (IM.inter (fun x _ -> x) !im1 IM.empty) IM.empty);    *)
(*|  assert_bool "test 2" (IM.equal (=) (IM.inter (fun x _ -> x) IM.empty !im1) IM.empty);    *)
(*|  assert_bool "test 3" (IM.equal (=) (IM.inter (fun x y -> ((x + y) / 2)) !im1 !im1) !im1);*)
(*|  let im = IM.inter (fun x y -> (x + y)) !im1 !im2 in                                      *)
(*|  assert_bool "test 4" (IM.equal (=) im (IM.inter (fun x y -> (x + y)) !im2 !im1));        *)
(*|  IM.iter (fun k x ->                                                                      *)
(*|          let y = M.find k !m1 + M.find k !m2                                              *)
(*|          in assert_equal ~msg:"test 5" x y                                                *)
(*|    ) im;                                                                                  *)
(*|  M.iter (fun k x ->                                                                       *)
(*|          if M.mem k !m2 then                                                              *)
(*|            assert_equal ~msg:"test 6" (IM.find k im) (x + M.find k !m2)                   *)
(*|    ) !m1;                                                                                 *)
(*|  update_time ();                                                                          *)
(*|;;                                                                                         *)
(*|                                                                                           *)
(*|(* testing difference *)                                                                   *)
(*|let test_diff () =                                                                         *)
(*|  assert_bool "test 1" (IM.equal (=) (IM.diff !im1 IM.empty) !im1);                        *)
(*|  assert_bool "test 2" (IM.equal (=) (IM.diff IM.empty !im1) IM.empty);                    *)
(*|  assert_bool "test 3" (IM.equal (=) (IM.diff !im1 !im1) IM.empty);                        *)
(*|  let im = IM.diff !im1 !im2 in                                                            *)
(*|  assert_bool "test 4" (IM.equal (=)                                                       *)
(*|        (IM.inter (fun x _ -> assert_bool "test 4.1" false; x) im !im2)                    *)
(*|        IM.empty);                                                                         *)
(*|  assert_bool "test 5" (IM.equal (=)                                                       *)
(*|        (IM.union (fun x _ -> assert_bool "test 5.1" false; x) im                          *)
(*|            (IM.inter (fun x _ -> x) !im1 !im2))                                           *)
(*|        !im1);                                                                             *)
(*|  IM.iter (fun k x ->                                                                      *)
(*|          assert_equal ~msg:"test 6" (M.find k !m1) x;                                     *)
(*|          assert_bool "test 7" (not (M.mem k !m2));                                        *)
(*|    ) im;                                                                                  *)
(*|  M.iter (fun k x ->                                                                       *)
(*|          if not (M.mem k !m2) then                                                        *)
(*|            assert_equal ~msg:"test 7" (IM.find k im) x                                    *)
(*|    ) !m1;                                                                                 *)
(*|  update_time ();                                                                          *)
(*|;;                                                                                         *)

(* testing iter2 *)
let test_iter2 () =
  let im = IM.create 13 in
  IM.iter2 ( fun key x y ->
          IM.add im key (x + y);
          assert_equal ~msg:"test 1" (IM.find im1 key) x;
          assert_equal ~msg:"test 2" (IM.find im2 key) y;
    ) im1 im2;
  (*|  assert_bool "test 3" (IM.equal (=) (IM.inter (fun x y -> (x + y)) !im1 !im2) !im);*)
  update_time ();
;;

(*|(* testing iter_s *)                                                          *)
(*|let test_iter_s () =                                                          *)
(*|  let is = ref IM.Set.empty in                                                *)
(*|  IM.iter (fun key x -> is := IM.Set.add key !is) !im2;                       *)
(*|  let im = ref IM.empty in                                                    *)
(*|  IM.iter_s ( fun key x ->                                                    *)
(*|          im := IM.add key x !im;                                             *)
(*|          assert_equal ~msg:"test 1" (IM.find key !im1) x;                    *)
(*|          assert_bool "test 2" (IM.Set.mem key !is);                          *)
(*|    ) !im1 !is;                                                               *)
(*|  assert_bool "test 3" (IM.equal (=) (IM.inter (fun x y -> x) !im1 !im2) !im);*)
(*|  update_time ();                                                             *)
(*|;;                                                                            *)
(*|                                                                              *)
(*|(* testing fold *)                                                            *)
(*|let test_fold () =                                                            *)
(*|  assert_equal ~msg:"test 1"                                                  *)
(*|    (IM.fold (fun k x n -> k.data + x + n) !im1 0)                            *)
(*|    (M.fold (fun k x n -> k.data + x + n) !m1 0);                             *)
(*|  update_time ();                                                             *)
(*|;;                                                                            *)
(*|                                                                              *)
(*|(* testing map *)                                                             *)
(*|let test_map () =                                                             *)
(*|  let im = IM.map (fun x -> 2 * x) !im1 in                                    *)
(*|  let m = M.map (fun x -> 2 * x) !m1 in                                       *)
(*|  IM.iter (fun k x ->                                                         *)
(*|          assert_equal ~msg:"test 1" (M.find k m) x                           *)
(*|    ) im;                                                                     *)
(*|  M.iter (fun k x ->                                                          *)
(*|          assert_equal ~msg:"test 2" (IM.find k im) x                         *)
(*|    ) m;                                                                      *)
(*|  update_time ();                                                             *)
(*|;;                                                                            *)
(*|                                                                              *)
(*|(* testing map *)                                                             *)
(*|let test_mapi () =                                                            *)
(*|  let im = IM.mapi (fun k x -> k.data + x) !im1 in                            *)
(*|  let m = M.mapi (fun k x -> k.data + x) !m1 in                               *)
(*|  IM.iter (fun k x ->                                                         *)
(*|          assert_equal ~msg:"test 1" (M.find k m) x                           *)
(*|    ) im;                                                                     *)
(*|  M.iter (fun k x ->                                                          *)
(*|          assert_equal ~msg:"test 2" (IM.find k im) x                         *)
(*|    ) m;                                                                      *)
(*|  update_time ();                                                             *)
(*|;;                                                                            *)
(*|                                                                              *)
(*|(* testing choose *)                                                          *)
(*|let test_choose () =                                                          *)
(*|  let im = ref !im1 in                                                        *)
(*|  for i = 1 to n do                                                           *)
(*|    try                                                                       *)
(*|      let k, x = IM.choose !im in                                             *)
(*|      assert_equal ~msg:"test 1" (IM.find k !im) x;                           *)
(*|      im := IM.remove k !im;                                                  *)
(*|    with Not_found -> assert_bool "test 2" (IM.is_empty !im)                  *)
(*|  done;                                                                       *)
(*|  update_time ();                                                             *)
(*|;;                                                                            *)

(* testing iter2 spped *)
let test_iter2s () =
  for i = 1 to 100000 do
    IM.iter2 ( fun key x y ->
            ()
      ) im1 im2;
  done;
  update_time ();
;;

let corr_suite = "Correctness" >::: [
  "test_mem" >:: test_mem;
  "test_find_iter" >:: test_find_iter;
  (*|  "test_process" >:: test_process;*)
  "test_remove" >:: test_remove;
  (*|  "test_replace" >:: test_replace;*)
  (*|  "test_equal" >:: test_equal;    *)
  (*|  "test_union" >:: test_union;    *)
  (*|  "test_inter" >:: test_inter;    *)
  (*|  "test_diff" >:: test_diff;      *)
  "test_iter2" >:: test_iter2;  
  (*|  "test_iter_s" >:: test_iter_s;*)
  (*|  "test_fold" >:: test_fold;    *)
  (*|  "test_map" >:: test_map;      *)
  (*|  "test_mapi" >:: test_mapi;    *)
  (*|  "test_choose" >:: test_choose;*)
  ]

(* ----------------- PERFORMANCE SUIT ---------------------- *)

(* testing iter2 speed *)
let test_iter2s () =
  for i = 1 to 100000 do
    IM.iter2 ( fun key x y ->
            ()
      ) im1 im2;
  done;
  update_time ();
;;

(* testing memory consumption *)
let test_memory () =
  let l = ref [] in
  for i = 1 to 100000 do
    let im = IM.create 13 in
    for j = 1 to 30 do
      let k = C.cons (Random.int n) in
(*|      let k = C.cons (100000 * i + j) in*)
      let x = (Random.int n) + 1 in (* positive values *)
      IM.add im k x;
    done;
    l := im :: !l
  done
;;

let perf_suite = "Performance" >::: [
  "test_iter2s" >:: test_iter2s;
(*|  "test_memory" >:: test_memory;*)
  ]

(* -------------------------------------------------------- *)

let _ =
  generate ();;
  let verbose = ref true in
  if not (was_successful (run_test_tt ~verbose:!verbose perf_suite)) then
    exit 1;
  print_memory_usage ();
  print_cpu_time ();