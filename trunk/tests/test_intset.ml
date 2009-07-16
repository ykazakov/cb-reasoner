(* Testing the functions of the module ints against the module set *)
open OUnit

(* canonincal implementation against which the test is performed *)
module S = Set.Make (struct type t = int let compare = compare end)

module IS = Intset_hc

(* ---------------- preparations------------------ *)

let _ =
  Printf.fprintf stderr "Preparing tests...";
  flush stderr

let n = 1000000
let s1 = ref S.empty
let s2 = ref S.empty
let is1 = ref IS.empty
let is2 = ref IS.empty

let _ =
  for i = 1 to n do
    let k = Random.int n in
    s1 := S.add k !s1;
    is1 := IS.add k !is1;
    let k = Random.int n in
    s2 := S.add k !s2;
    is2 := IS.add k !is2;
  done

let c1 = IS.cardinal !is1
let c2 = IS.cardinal !is2

let _ =
  Printf.fprintf stderr "done\n";
  flush stderr

;;

(* ----------------- TESTS ---------------------- *)

(* testing cardinalities *)
let test_card () =
(*|  Printf.fprintf stderr "Cardinality %n, check: %n\n" c1 (S.cardinal !s1); *)
  assert_equal ~msg:"test 1" c1 (S.cardinal !s1);
(*|  S.iter (fun k -> Printf.fprintf stderr "%n" k) !s2;*)
(*|  Printf.fprintf stderr "Cardinality %n, check: %n\n" c2 (S.cardinal !s2);*)
  assert_equal ~msg:"test 2" c2 (S.cardinal !s2);  
;;

(* testing mem and iter *)
let test_mem () =
  for i = 1 to n do
    let k = Random.int n in
    assert_equal ~msg:"test 1" (S.mem k !s1) (IS.mem k !is1);
    assert_equal ~msg:"test 2" (S.mem k !s2) (IS.mem k !is2);
  done;
;;
let test_mem_iter () =
  IS.iter (fun k ->
          assert_bool "test 1" (S.mem k !s1)
    ) !is1;
  S.iter (fun k ->
          assert_bool "test 2" (IS.mem k !is1)
    ) !s1;
  IS.iter (fun k ->
          assert_bool "test 3" (S.mem k !s2)
    ) !is2;
  S.iter (fun k ->
          assert_bool "test 4" (IS.mem k !is2)
    ) !s2;
;;

(*|(* testing add_f *)                                                          *)
(*|let test_add_f () =                                                          *)
(*|  let is = ref IS.empty in                                                   *)
(*|  for i = 1 to n do                                                          *)
(*|    let k = Random.int n in                                                  *)
(*|    is := IS.add_f k                                                         *)
(*|      (fun _ -> assert_bool "test 1" (not (IS.mem k !is)))                   *)
(*|      (fun _ -> assert_bool "test 2" (IS.mem k !is)) !is;                    *)
(*|    assert_bool "test 3" (IS.mem k !is);                                     *)
(*|  done;                                                                      *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing remove *)                                                         *)
(*|let test_remove () =                                                         *)
(*|  let s = ref !s1 in                                                         *)
(*|  let is = ref !is1 in                                                       *)
(*|  for i = 1 to n do                                                          *)
(*|    let k = Random.int n in                                                  *)
(*|    s := S.remove k !s;                                                      *)
(*|    is := IS.remove k !is;                                                   *)
(*|  done;                                                                      *)
(*|  assert_equal (S.cardinal !s) (IS.cardinal !is);                            *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing equal *)                                                          *)
(*|let test_equal () =                                                          *)
(*|  assert_bool "test 1" (IS.equal !is1 !is1);                                 *)
(*|  let is = ref IS.empty in                                                   *)
(*|  S.iter (fun k -> is := IS.add k !is) !s1;                                  *)
(*|  assert_bool "test 2" (IS.equal !is !is1);                                  *)
(*|  let is = ref IS.empty in                                                   *)
(*|  IS.iter (fun k -> is := IS.add k !is) !is1;                                *)
(*|  assert_bool "test 3" (IS.equal !is !is1);                                  *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing subset *)                                                         *)
(*|let test_subset () =                                                         *)
(*|  assert_bool "test 1" (IS.subset IS.empty IS.empty);                        *)
(*|  assert_bool "test 2" (IS.subset IS.empty !is1);                            *)
(*|  assert_bool "test 2" (IS.subset !is1 !is1);                                *)
(*|  assert_equal ~msg:"test 3" (IS.subset !is1 IS.empty) (IS.is_empty !is1);   *)
(*|  let is = ref !is1 in                                                       *)
(*|  for i = 1 to 100 do                                                        *)
(*|    let k = Random.int n in                                                  *)
(*|    if IS.mem k !is1 then (                                                  *)
(*|      is := IS.remove k !is;                                                 *)
(*|      assert_bool "test 4" (IS.subset !is !is1);                             *)
(*|    ) else                                                                   *)
(*|      assert_bool "test 5" (not (IS.subset (IS.add k !is) !is1));            *)
(*|  done;                                                                      *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing union *)                                                          *)
(*|let test_union () =                                                          *)
(*|  assert_bool "test 1" (IS.equal (IS.union !is1 IS.empty) !is1);             *)
(*|  assert_bool "test 2" (IS.equal (IS.union IS.empty !is1) !is1);             *)
(*|  assert_bool "test 3" (IS.equal (IS.union !is1 !is1) !is1);                 *)
(*|  let is = IS.union !is1 !is2 in                                             *)
(*|  assert_equal ~msg:"test 4" (S.cardinal (S.union !s1 !s2)) (IS.cardinal is);*)
(*|  assert_equal ~msg:"test 5" (IS.subset !is1 is) true;                       *)
(*|  assert_equal ~msg:"test 6" (IS.subset !is2 is) true;                       *)
(*|  assert_equal ~msg:"test 7" (IS.subset is !is1) (IS.subset !is2 !is1);      *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing intersection *)                                                   *)
(*|let test_inter () =                                                          *)
(*|  assert_bool "test 1" (IS.equal (IS.inter !is1 IS.empty) IS.empty);         *)
(*|  assert_bool "test 2" (IS.equal (IS.inter IS.empty !is1) IS.empty);         *)
(*|  assert_bool "test 3" (IS.equal (IS.inter !is1 !is1) !is1);                 *)
(*|  let is = IS.inter !is1 !is2 in                                             *)
(*|  assert_equal ~msg:"test 4" (S.cardinal (S.inter !s1 !s2)) (IS.cardinal is);*)
(*|  assert_equal ~msg:"test 5" (IS.subset is !is1) true;                       *)
(*|  assert_equal ~msg:"test 6" (IS.subset is !is2) true;                       *)
(*|  assert_equal ~msg:"test 7" (IS.subset !is1 is) (IS.subset !is1 !is2);      *)
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
(*|  assert_equal ~msg:"test 7" (IS.subset is !is1) true;                       *)
(*|  assert_equal ~msg:"test 8" (IS.is_empty (IS.inter is !is2)) true;          *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing iter2 *)                                                          *)
(*|let test_iter2 () =                                                          *)
(*|  let is = ref IS.empty in                                                   *)
(*|  IS.iter2 ( fun k ->                                                        *)
(*|          is := IS.add k !is;                                                *)
(*|          assert_bool "test 1" (IS.mem k !is1);                              *)
(*|          assert_bool "test 2" (IS.mem k !is2);                              *)
(*|    ) !is1 !is2;                                                             *)
(*|  assert_bool "test 3" (IS.equal (IS.inter !is1 !is2) !is)                   *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing iter_diff *)                                                      *)
(*|let test_iter_diff () =                                                      *)
(*|  let i = ref 0 in                                                           *)
(*|  IS.iter_diff ( fun k ->                                                    *)
(*|          incr i;                                                            *)
(*|          assert_bool "test 1" (IS.mem k !is1);                              *)
(*|          assert_bool "test 2" (not (IS.mem k !is2));                        *)
(*|    ) !is1 !is2;                                                             *)
(*|  assert_equal ~msg:"test 3" (S.cardinal (S.diff !s1 !s2)) !i                *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing fold *)                                                           *)
(*|let test_fold () =                                                           *)
(*|  assert_equal ~msg:"test 1"                                                 *)
(*|    (IS.fold (fun k n -> k + n) !is1 0)                                      *)
(*|    (S.fold (fun k n -> k + n) !s1 0)                                        *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing for_all *)                                                        *)
(*|let test_forall () =                                                         *)
(*|  for i = 1 to 100 do                                                        *)
(*|    let k = Random.int n in                                                  *)
(*|    assert_equal ~msg:"test 1"                                               *)
(*|      (IS.for_all (fun k1 -> k1 <> k) !is1)                                  *)
(*|      (S.for_all (fun k1 -> k1 <> k) !s1)                                    *)
(*|  done                                                                       *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing exists *)                                                         *)
(*|let test_exists () =                                                         *)
(*|  for i = 1 to 100 do                                                        *)
(*|    let k = Random.int n in                                                  *)
(*|    assert_equal ~msg:"test 1"                                               *)
(*|      (IS.exists (fun k1 -> k1 = k) !is1)                                    *)
(*|      (S.exists (fun k1 -> k1 = k) !s1)                                      *)
(*|  done                                                                       *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing filter *)                                                         *)
(*|let test_filter () =                                                         *)
(*|  for i = 1 to 10 do                                                         *)
(*|    let k = Random.int n in                                                  *)
(*|    assert_equal ~msg:"test 1"                                               *)
(*|      (IS.cardinal (IS.filter (fun k1 -> k1 mod k > k / 2) !is1))            *)
(*|      (S.cardinal (S.filter (fun k1 -> k1 mod k > k / 2) !s1))               *)
(*|  done                                                                       *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing partition *)                                                      *)
(*|let test_partition () =                                                      *)
(*|  for i = 1 to 10 do                                                         *)
(*|    let k = Random.int n in                                                  *)
(*|    let ist, isf = IS.partition (fun k1 -> k1 mod k > k / 2) !is1 in         *)
(*|    let st, sf = S.partition (fun k1 -> k1 mod k > k / 2) !s1 in             *)
(*|    assert_equal ~msg:"test 1" (IS.cardinal ist) (S.cardinal st);            *)
(*|    assert_equal ~msg:"test 2" (IS.cardinal isf) (S.cardinal sf);            *)
(*|  done                                                                       *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing elements *)                                                       *)
(*|let test_elements () =                                                       *)
(*|  let l = IS.elements !is1 in                                                *)
(*|  assert_equal ~msg:"est 1" (List.length l) c1;                              *)
(*|  let s = ref !s1 in                                                         *)
(*|  List.iter (fun k -> s := S.remove k !s) l;                                 *)
(*|  assert_bool "test 2" (S.is_empty !s)                                       *)
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
(*|  done                                                                       *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing compare *)                                                        *)
(*|let test_compare () =                                                        *)
(*|  let jmax = 100 in                                                          *)
(*|  for j = 1 to jmax do                                                       *)
(*|    let isa = ref IS.empty in                                                *)
(*|    let isb = ref IS.empty in                                                *)
(*|    let isc = ref IS.empty in                                                *)
(*|    for i = 1 to n / jmax do                                                 *)
(*|      isa := IS.add (Random.int jmax) !isa;                                  *)
(*|      isb := IS.add (Random.int jmax) !isb;                                  *)
(*|      isc := IS.add (Random.int jmax) !isc;                                  *)
(*|    done;                                                                    *)
(*|    let cab = IS.compare !isa !isb in                                        *)
(*|    let cbc = IS.compare !isb !isc in                                        *)
(*|    let cca = IS.compare !isc !isa in                                        *)
(*|    assert_bool "test 1" (not (cab >= 0 && cbc >= 0 && cca > 0));            *)
(*|    assert_bool "test 2" (not (cab >= 0 && cbc > 0 && cca >= 0));            *)
(*|    assert_bool "test 3" (not (cab > 0 && cbc >= 0 && cca >= 0));            *)
(*|    if cab = 0 then assert_bool "test 4" (IS.equal !isa !isb);               *)
(*|    if cbc = 0 then assert_bool "test 5" (IS.equal !isb !isc);               *)
(*|    if cca = 0 then assert_bool "test 6" (IS.equal !isc !isa);               *)
(*|  done                                                                       *)
(*|;;                                                                           *)
(*|                                                                             *)
(*|(* testing right *)                                                          *)
(*|let test_right () =                                                          *)
(*|  let jmax = 100 in                                                          *)
(*|  for j = 1 to jmax do                                                       *)
(*|    let isa = ref IS.empty in                                                *)
(*|    let isb = ref IS.empty in                                                *)
(*|    let isc = ref IS.empty in                                                *)
(*|    let isd = ref IS.empty in                                                *)
(*|    for i = 1 to n / jmax do                                                 *)
(*|      isa := IS.add (Random.int jmax) !isa;                                  *)
(*|      isb := IS.add (Random.int jmax) !isb;                                  *)
(*|      isc := IS.add (Random.int jmax) !isc;                                  *)
(*|      isd := IS.add (Random.int jmax) !isd;                                  *)
(*|    done;                                                                    *)
(*|    (* first we sort isa, isb, isc, and isd according to IS.compare *)       *)
(*|    if IS.compare !isa !isb > 0 then                                         *)
(*|      (let istmp = !isb in isb := !isa; isa := istmp);                       *)
(*|    if IS.compare !isb !isc > 0 then (                                       *)
(*|      let istmp = !isc in                                                    *)
(*|      if IS.compare !isa !isc > 0 then                                       *)
(*|        (isc := !isa; isa := istmp)                                          *)
(*|      else (isc := !isb; isb := istmp);                                      *)
(*|    );                                                                       *)
(*|    if IS.compare !isc !isd > 0 then (                                       *)
(*|      let istmp = !isd in                                                    *)
(*|      if IS.compare !isb !isd > 0 then                                       *)
(*|        if IS.compare !isa !isd > 0 then                                     *)
(*|          (isd := !isa; isa := istmp)                                        *)
(*|        else (isd := !isb; isb := istmp)                                     *)
(*|      else (isd := !isc; isc := istmp);                                      *)
(*|    );                                                                       *)
(*|    if IS.right !isb !isa !isd then                                          *)
(*|      assert_bool "test 1" (IS.right !isc !isa !isb)                         *)
(*|    else if IS.right !isc !isa !isd then                                     *)
(*|      assert_bool "test 2" (not (IS.right !isb !isa !isc))                   *)
(*|    else assert_bool "test 3" (not (IS.right !isc !isb !isd))                *)
(*|  done                                                                       *)
(*|;;                                                                           *)

let suite = "Intset tests" >::: [
  "test_card" >:: test_card;
  "test_mem" >:: test_mem;
  "test_mem_iter" >:: test_mem_iter;
(*|  "test_add_f" >:: test_add_f;        *)
(*|  "test_subset" >:: test_subset;      *)
(*|  "test_remove" >:: test_remove;      *)
(*|  "test_equal" >:: test_equal;        *)
(*|  "test_union" >:: test_union;        *)
(*|  "test_inter" >:: test_inter;        *)
(*|  "test_diff" >:: test_diff;          *)
(*|  "test_iter2" >:: test_iter2;        *)
(*|  "test_iter_diff" >:: test_iter_diff;*)
(*|  "test_fold" >:: test_fold;          *)
(*|  "test_frall" >:: test_forall;       *)
(*|  "test_exists" >:: test_exists;      *)
(*|  "test_filter" >:: test_filter;      *)
(*|  "test_partition" >:: test_partition;*)
(*|  "test_elements" >:: test_elements;  *)
(*|  "test_choose" >:: test_choose;      *)
(*|  "test_compare" >:: test_compare;    *)
(*|  "test_right" >:: test_right;        *)
  ]

let _ =
  let verbose = ref true in
  if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
    exit 1