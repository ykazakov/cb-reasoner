(* Testing the functions of the module ints against the module set *)
open OUnit
open Consed.T

(* canonincal implementation against which the test is performed *)
(*|module S = Set.Make (struct type t = int let compare = compare end)*)
module S = Intset

module IS = Chashset.Make (struct
    type t = int
  end)

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

let n = 100000
let s1 = ref S.empty
let s2 = ref S.empty
let is1 = IS.create 13
let is2 = IS.create 13
let c1 = ref 0
let c2 = ref 0

let generate () =
  Printf.fprintf stderr "Preparing tests...";
  flush stderr;
  for i = 1 to n do
    let k = Random.int n in
    s1 := S.add k !s1;
    IS.add is1 (C.cons k);
    let k = Random.int n in
    s2 := S.add k !s2;
    IS.add is2 (C.cons k);
  done;
  c1 := IS.cardinal is1;
  c2 := IS.cardinal is2;
  update_time ();
  Printf.fprintf stderr "done\n";
  flush stderr
;;

(* ----------------- PERFORMANCE SUIT ---------------------- *)

(* testing cardinalities *)
let test_card () =
  assert_equal ~msg:"test 1" (S.cardinal !s1) !c1;
  assert_equal ~msg:"test 2" (S.cardinal !s2) !c2;
  update_time ();
;;

(* testing mem and iter *)
let test_mem () =
  for i = 1 to n do
    let k = Random.int n in
    assert_equal ~msg:"test 1" (S.mem k !s1) (IS.mem is1 (C.cons k));
    assert_equal ~msg:"test 2" (S.mem k !s2) (IS.mem is2 (C.cons k));
  done;
  update_time ();
;;
let test_mem_iter () =
  IS.iter (fun k_c ->
          assert_bool "test 1" (S.mem k_c.data !s1)
    ) is1;
  S.iter (fun k ->
          assert_bool "test 2" (IS.mem is1 (C.cons k))
    ) !s1;
  IS.iter (fun k_c ->
          assert_bool "test 3" (S.mem k_c.data !s2)
    ) is2;
  S.iter (fun k ->
          assert_bool "test 4" (IS.mem is2 (C.cons k))
    ) !s2;
  update_time ();
;;

(* testing remove *)
let test_remove () =
  let s = ref !s1 in
  let is = IS.copy is1 in
  for i = 1 to n do
    let k = Random.int n in
    s := S.remove k !s;
    IS.remove is (C.cons k);
  done;
  assert_equal (S.cardinal !s) (IS.cardinal is);
  update_time ();
;;

(* testing equal *)
let test_equal () =
  assert_bool "test 1" (IS.equal is1 is1);
  update_time ();
  let is = IS.create 13 in
  S.iter (fun k -> IS.add is (C.cons k)) !s1;
  update_time ();
  assert_bool "test 2" (IS.equal is is1);
  update_time ();
  let is = IS.create 13 in
  IS.iter (fun k -> IS.add is k) is1;
  update_time ();
  assert_bool "test 3" (IS.equal is is1);
  update_time ();
;;

(*|(* testing subset *)                                                         *)
(*|let test_subset () =                                                         *)
(*|  assert_bool "test 1" (IS.is_subset IS.empty IS.empty);                     *)
(*|  assert_bool "test 2" (IS.is_subset IS.empty !is1);                         *)
(*|  assert_bool "test 2" (IS.is_subset !is1 !is1);                             *)
(*|  assert_equal ~msg:"test 3" (IS.is_subset !is1 IS.empty) (IS.is_empty !is1);*)
(*|  let is = ref !is1 in                                                       *)
(*|  for i = 1 to 100 do                                                        *)
(*|    let k = Random.int n in                                                  *)
(*|    if IS.mem (C.cons k) !is1 then (                                         *)
(*|      is := IS.remove (C.cons k) !is;                                        *)
(*|      assert_bool "test 4" (IS.is_subset !is !is1);                          *)
(*|    ) else                                                                   *)
(*|      assert_bool "test 5" (not (IS.is_subset (IS.add (C.cons k) !is) !is1));*)
(*|  done;                                                                      *)
(*|  update_time ();                                                            *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing union *)                                                          *)
(*|let test_union () =                                                          *)
(*|  assert_bool "test 1" (IS.equal (IS.union !is1 IS.empty) !is1);             *)
(*|  assert_bool "test 2" (IS.equal (IS.union IS.empty !is1) !is1);             *)
(*|  assert_bool "test 3" (IS.equal (IS.union !is1 !is1) !is1);                 *)
(*|  let is = IS.union !is1 !is2 in                                             *)
(*|  assert_bool "test 4" (IS.equal (IS.union !is2 !is1) is);                   *)
(*|  assert_equal ~msg:"test 5" (S.cardinal (S.union !s1 !s2)) (IS.cardinal is);*)
(*|  assert_equal ~msg:"test 6" (IS.is_subset !is1 is) true;                    *)
(*|  assert_equal ~msg:"test 7" (IS.is_subset !is2 is) true;                    *)
(*|  assert_equal ~msg:"test 8" (IS.is_subset is !is1) (IS.is_subset !is2 !is1);*)
(*|  update_time ();                                                            *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing intersection *)                                                   *)
(*|let test_inter () =                                                          *)
(*|  assert_bool "test 1" (IS.equal (IS.inter !is1 IS.empty) IS.empty);         *)
(*|  assert_bool "test 2" (IS.equal (IS.inter IS.empty !is1) IS.empty);         *)
(*|  assert_bool "test 3" (IS.equal (IS.inter !is1 !is1) !is1);                 *)
(*|  let is = IS.inter !is1 !is2 in                                             *)
(*|  assert_bool "test 4" (IS.equal (IS.inter !is2 !is1) is);                   *)
(*|  assert_equal ~msg:"test 5" (S.cardinal (S.inter !s1 !s2)) (IS.cardinal is);*)
(*|  assert_equal ~msg:"test 6" (IS.is_subset is !is1) true;                    *)
(*|  assert_equal ~msg:"test 7" (IS.is_subset is !is2) true;                    *)
(*|  assert_equal ~msg:"test 8" (IS.is_subset !is1 is) (IS.is_subset !is1 !is2);*)
(*|  update_time ();                                                            *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing difference *)                                                     *)
(*|let test_diff () =                                                           *)
(*|  assert_bool "test 1" (IS.equal (IS.diff !is1 IS.empty) !is1);              *)
(*|  assert_bool "test 2" (IS.equal (IS.diff IS.empty !is1) IS.empty);          *)
(*|  assert_bool "test 3" (IS.equal (IS.diff !is1 !is1) IS.empty);              *)
(*|  let is = IS.diff !is1 !is2 in                                              *)
(*|  assert_bool "test 4" (IS.equal (IS.inter is !is2) IS.empty);               *)
(*|  assert_bool "test 5" (IS.equal (IS.union is (IS.inter !is1 !is2)) !is1);   *)
(*|  assert_equal ~msg:"test 6" (S.cardinal (S.diff !s1 !s2)) (IS.cardinal is); *)
(*|  assert_equal ~msg:"test 7" (IS.is_subset is !is1) true;                    *)
(*|  assert_equal ~msg:"test 8" (IS.is_empty (IS.inter is !is2)) true;          *)
(*|  update_time ();                                                            *)
(*|;;                                                                           *)

(* testing iter2 *)
let test_iter2 () =
  let is = IS.create 13 in
  IS.iter2 ( fun k ->
          IS.add is k;
          assert_bool "test 1" (IS.mem is1 k);
          assert_bool "test 2" (IS.mem is2 k);
    ) is1 is2;
  update_time ();
;;

(*|(* testing iter_diff *)                                       *)
(*|let test_iter_diff () =                                       *)
(*|  let i = ref 0 in                                            *)
(*|  IS.iter_diff ( fun k ->                                     *)
(*|          incr i;                                             *)
(*|          assert_bool "test 1" (IS.mem k !is1);               *)
(*|          assert_bool "test 2" (not (IS.mem k !is2));         *)
(*|    ) !is1 !is2;                                              *)
(*|  assert_equal ~msg:"test 3" (S.cardinal (S.diff !s1 !s2)) !i;*)
(*|  update_time ();                                             *)
(*|;;                                                            *)

(* testing fold *)
let test_fold () =
  assert_equal ~msg:"test 1"
    (IS.fold (fun k_c n -> k_c.data + n) is1 0)
    (S.fold (fun k n -> k + n) !s1 0);
  update_time ();
;;

(*|(* testing for_all *)                                                        *)
(*|let test_forall () =                                                         *)
(*|  for i = 1 to 100 do                                                        *)
(*|    let k = Random.int n in                                                  *)
(*|    assert_equal ~msg:"test 1"                                               *)
(*|      (IS.for_all (fun k1_c -> k1_c.data <> k) !is1)                         *)
(*|      (S.for_all (fun k1 -> k1 <> k) !s1)                                    *)
(*|  done;                                                                      *)
(*|  update_time ();                                                            *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing exists *)                                                         *)
(*|let test_exists () =                                                         *)
(*|  for i = 1 to 100 do                                                        *)
(*|    let k = Random.int n in                                                  *)
(*|    assert_equal ~msg:"test 1"                                               *)
(*|      (IS.exists (fun k1_c -> k1_c.data = k) !is1)                           *)
(*|      (S.exists (fun k1 -> k1 = k) !s1)                                      *)
(*|  done;                                                                      *)
(*|  update_time ();                                                            *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing filter *)                                                         *)
(*|let test_filter () =                                                         *)
(*|  for i = 1 to 10 do                                                         *)
(*|    let k = Random.int n in                                                  *)
(*|    assert_equal ~msg:"test 1"                                               *)
(*|      (IS.cardinal (IS.filter (fun k1_c -> k1_c.data mod k > k / 2) !is1))   *)
(*|      (S.cardinal (S.filter (fun k1 -> k1 mod k > k / 2) !s1))               *)
(*|  done;                                                                      *)
(*|  update_time ();                                                            *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing partition *)                                                      *)
(*|let test_partition () =                                                      *)
(*|  for i = 1 to 10 do                                                         *)
(*|    let k = Random.int n in                                                  *)
(*|    let ist, isf = IS.partition (fun k1_c -> k1_c.data mod k > k / 2) !is1 in*)
(*|    let st, sf = S.partition (fun k1 -> k1 mod k > k / 2) !s1 in             *)
(*|    assert_equal ~msg:"test 1" (IS.cardinal ist) (S.cardinal st);            *)
(*|    assert_equal ~msg:"test 2" (IS.cardinal isf) (S.cardinal sf);            *)
(*|  done;                                                                      *)
(*|  update_time ();                                                            *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing elements *)                                                       *)
(*|let test_elements () =                                                       *)
(*|  let l = IS.elements !is1 in                                                *)
(*|  assert_equal ~msg:"est 1" (List.length l) !c1;                              *)
(*|  let s = ref !s1 in                                                         *)
(*|  List.iter (fun k_c -> s := S.remove k_c.data !s) l;                        *)
(*|  assert_bool "test 2" (S.is_empty !s);                                      *)
(*|  update_time ();                                                            *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing choose *)                                                         *)
(*|let test_choose () =                                                         *)
(*|  let is = ref !is1 in                                                       *)
(*|  for i = 1 to n do                                                          *)
(*|    try                                                                      *)
(*|      let k = IS.choose !is in                                               *)
(*|      assert_bool "test 1" (IS.mem k !is);                                   *)
(*|      is := IS.remove k !is;                                                 *)
(*|    with Not_found -> assert_bool "test 2" (IS.is_empty !is)                 *)
(*|  done;                                                                      *)
(*|  update_time ();                                                            *)
(*|;;                                                                           *)

let corr_suite = "Correctness" >::: [
  "test_card" >:: test_card;
  "test_mem" >:: test_mem;
  "test_mem_iter" >:: test_mem_iter;
  (*|  "test_subset" >:: test_subset;*)
  "test_remove" >:: test_remove;
  "test_equal" >:: test_equal;
  (*|  "test_union" >:: test_union;*)
  (*|  "test_inter" >:: test_inter;*)
  (*|  "test_diff" >:: test_diff;  *)
  "test_iter2" >:: test_iter2;
  (*|  "test_iter_diff" >:: test_iter_diff;*)
  "test_fold" >:: test_fold;
  (*|  "test_forall" >:: test_forall;      *)
  (*|  "test_exists" >:: test_exists;      *)
  (*|  "test_filter" >:: test_filter;      *)
  (*|  "test_partition" >:: test_partition;*)
  (*|  "test_elements" >:: test_elements;  *)
  (*|  "test_choose" >:: test_choose;      *)
  ]

(* ----------------- PERFORMANCE SUIT ---------------------- *)

(* testing iter2 speed *)
let test_iter2s () =
  let is = IS.create 13 in
  for i = 1 to 100 do
    let k = C.cons (Random.int n) in
    IS.add is k;
  done;
  for i = 1 to 1000000 do
    IS.iter2 ( fun elt ->
            ()
      ) is is1;
  done;
  update_time ();
;;

(* testing memory consumption *)
let test_memory () =
  let a = Array.create 53584 [] in
  for i = 1 to 56 do
    for j = 0 to 53584 - 1 do
      match a.(j) with
      | [] -> a.(j) <- [IS.create 1]
      | [is] -> let k = C.cons (Random.int 32000) in
          IS.add is k;
      | _ -> ()
    done;
  done
;;

let perf_suite = "Performance" >::: [
(*|  "test_iter2s" >:: test_iter2s;*)
  "test_memory" >:: test_memory;
  ]

(* -------------------------------------------------------- *)

let _ =
(*|  generate ();*)
  let verbose = ref true in
  if not (was_successful (run_test_tt ~verbose:!verbose perf_suite)) then
    exit 1;
  print_memory_usage ();
  print_cpu_time ();
;;