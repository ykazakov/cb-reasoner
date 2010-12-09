(* Testing the functions of the module ints against the module set *)
open OUnit

(* canonincal implementation against which the test is performed *)
module M = Map.Make (struct type t = int let compare = compare end)

module IM = Imap.Make (struct
    type t = int
    let compare x y = x - y
    let right x y z = x lxor y > x lxor z
  end)

(* ---------------- preparations------------------ *)

let _ =
  Printf.fprintf stderr "Preparing tests...";
  flush stderr

let n = 1000000
let m1 = ref M.empty
let m2 = ref M.empty
let im1 = ref IM.empty
let im2 = ref IM.empty

let _ =
  for i = 1 to n do
    let k = Random.int n in
    let x = Random.int n in
    m1 := M.add k x !m1;
    im1 := IM.add k x !im1;
    let k = Random.int n in
    let x = Random.int n in
    m2 := M.add k x !m2;
    im2 := IM.add k x !im2;
  done

let _ =
  Printf.fprintf stderr "done\n";
  flush stderr

;;

(* ----------------- TESTS ---------------------- *)

(* testing mem and iter *)
let test_mem () =
  for i = 1 to n do
    let k = Random.int n in
    assert_equal ~msg:"test 1" (M.mem k !m1) (IM.mem k !im1);
    assert_equal ~msg:"test 2" (M.mem k !m2) (IM.mem k !im2);
  done;
;;

let test_find_iter () =
  IM.iter (fun k x ->
          assert_equal ~msg:"test 1" (M.find k !m1) x
    ) !im1;
  M.iter (fun k x ->
          assert_equal ~msg:"test 2" (IM.find k !im1) x
    ) !m1;
  IM.iter (fun k x ->
          assert_equal ~msg:"test 3" (M.find k !m2) x
    ) !im2;
  M.iter (fun k x ->
          assert_equal ~msg:"test 4" (IM.find k !im2) x
    ) !m2;
;;

(* testing add_f *)
let test_add_f () =
  let im = ref IM.empty in
  for i = 1 to n do
    let k = Random.int n in
    im := IM.add_f k
      (fun () -> assert_bool "test 1" (not (IM.mem k !im)); 0)
      (fun _ -> assert_bool "test 2" (IM.mem k !im); 1) !im;
    assert_bool "test 3" (IM.mem k !im);
  done;
;;

(* testing remove *)
let test_remove () =
  let m = ref !m1 in
  let im = ref !im1 in
  for i = 1 to n do
    let k = Random.int n in
    m := M.remove k !m;
    im := IM.remove k !im;
  done;
  M.iter ( fun k x ->
          assert_equal ~msg:"test 1" (IM.find k !im) x
    ) !m;
  IM.iter ( fun k x ->
          assert_equal ~msg:"test 2" (M.find k !m) x
    ) !im;
;;

(* testing replace *)
let test_replace () =
  let im = ref !im1 in
  let m = ref !m1 in
  for i = 1 to n do
    let k = Random.int n in (
      try
        im := IM.replace k 0 !im;
        assert_bool "test 1" (M.mem k !m);
      with Not_found -> assert_bool "test 2" (not (M.mem k !m))
    );
    if M.mem k !m then (
      assert_equal ~msg:"test 4" (IM.find k !im) 0;
      m := M.add k 0 !m;
    )
  done;
  M.iter ( fun k x ->
          assert_equal ~msg:"test 5" (IM.find k !im) x
    ) !m;
  IM.iter ( fun k x ->
          assert_equal ~msg:"test 6" (M.find k !m) x
    ) !im;
;;

(* testing replace_f *)
let test_replace_f () =
  let im = ref !im1 in
  let m = ref !m1 in
  for i = 1 to n do
    let k = Random.int n in
    let rm = Random.bool () in (
      try
        im := IM.replace_f k
          (fun x -> if rm then raise IM.Remove else 2 * x) !im;
        assert_bool "test 1" (M.mem k !m);
      with Not_found -> assert_bool "test 2" (not (M.mem k !m))
    );
    if rm then (
      assert_bool "test 3" (not (IM.mem k !im));
      m := M.remove k !m;
    ) else if M.mem k !m then (
      let x = 2 * (M.find k !m) in
      assert_equal ~msg:"test 4" (IM.find k !im) x;
      m := M.add k x !m;
    )
  done;
  M.iter ( fun k x ->
          assert_equal ~msg:"test 5" (IM.find k !im) x
    ) !m;
  IM.iter ( fun k x ->
          assert_equal ~msg:"test 6" (M.find k !m) x
    ) !im;
;;

(* testing equal *)
let test_equal () =
  assert_bool "test 1" (IM.equal (=) !im1 !im1);
  let im = ref IM.empty in
  M.iter (fun k x -> im := IM.add k x !im) !m1;
  assert_bool "test 2" (IM.equal (=) !im !im1);
  let im = ref IM.empty in
  IM.iter (fun key x -> im := IM.add key x !im) !im1;
  assert_bool "test 3" (IM.equal (=) !im !im1);
;;

(* testing union *)
let test_union () =
  assert_bool "test 1" (IM.equal (=) (IM.union (fun x _ -> x) !im1 IM.empty) !im1);
  assert_bool "test 2" (IM.equal (=) (IM.union (fun x _ -> x) IM.empty !im1) !im1);
  assert_bool "test 3" (IM.equal (=) (IM.union (fun x y -> (x + y) / 2) !im1 !im1) !im1);
  let im = IM.union (fun x y -> x + y) !im1 !im2 in
  assert_bool "test 4" (IM.equal (=) im (IM.union (fun x y -> x + y) !im2 !im1));
  IM.iter (fun k x ->
          let y =
            (try M.find k !m1 with Not_found -> 0)
            +
            (try M.find k !m2 with Not_found -> 0)
          in assert_equal ~msg:"test 5" x y
    ) im;
  let m = ref !m1 in
  M.iter (fun k x ->
          m := M.add k (x + try M.find k !m with Not_found -> 0) !m
    ) !m2;
  M.iter ( fun k x ->
          assert_equal ~msg:"test 6" (IM.find k im) x
    ) !m;
;;

(* testing filter *)
let test_filter () =
  assert_bool "test 1" (IM.equal (=) (IM.filter (fun x _ -> x) !im1 IM.empty) IM.empty);
  assert_bool "test 2" (IM.equal (=) (IM.filter (fun x _ -> x) IM.empty !im1) IM.empty);
  assert_bool "test 3" (IM.equal (=) (IM.filter (fun x y -> (x + y) / 2) !im1 !im1) !im1);
  let im = IM.filter (fun x y -> x + y) !im1 !im2 in
  assert_bool "test 4" (IM.equal (=) im (IM.filter (fun x y -> x + y) !im2 !im1));
  IM.iter (fun k x ->
          let y = M.find k !m1 + M.find k !m2
          in assert_equal ~msg:"test 5" x y
    ) im;
  M.iter (fun k x ->
          if M.mem k !m2 then
            assert_equal ~msg:"test 6" (IM.find k im) (x + M.find k !m2)
    ) !m1;
;;

(* testing difference *)
let test_diff () =
  assert_bool "test 1" (IM.equal (=) (IM.diff !im1 IM.empty) !im1);
  assert_bool "test 2" (IM.equal (=) (IM.diff IM.empty !im1) IM.empty);
  assert_bool "test 3" (IM.equal (=) (IM.diff !im1 !im1) IM.empty);
  let im = IM.diff !im1 !im2 in
  assert_bool "test 4" (IM.equal (=) (IM.filter (fun x _ -> assert_bool "test 4.1" false; x) im !im2) IM.empty);
  assert_bool "test 5" (IM.equal (=)
        (IM.union (fun x _ -> assert_bool "test 5.1" false; x) im (IM.filter (fun x _ -> x) !im1 !im2))
        !im1);
  IM.iter (fun k x ->
          assert_equal ~msg:"test 6" (M.find k !m1) x;
          assert_bool "test 7" (not (M.mem k !m2));
    ) im;
  M.iter (fun k x ->
          if not (M.mem k !m2) then
            assert_equal ~msg:"test 7" (IM.find k im) x
    ) !m1;
;;

(* testing iter2 *)
let test_iter2 () =
  let im = ref IM.empty in
  IM.iter2 ( fun key x y ->
          im := IM.add key (x + y) !im;
          assert_equal ~msg:"test 1" (IM.find key !im1) x;
          assert_equal ~msg:"test 2" (IM.find key !im2) y;
    ) !im1 !im2;
  assert_bool "test 3" (IM.equal (=) (IM.filter (fun x y -> x + y) !im1 !im2) !im)
;;

(* testing iter_s *)
let test_iter_s () =
  let is = ref IM.Set.empty in
  IM.iter (fun key x -> is := IM.Set.add key !is) !im2;
  let im = ref IM.empty in
  IM.iter_s ( fun key x ->
          im := IM.add key x !im;
          assert_equal ~msg:"test 1" (IM.find key !im1) x;
          assert_bool "test 2" (IM.Set.mem key !is);
    ) !im1 !is;
  assert_bool "test 3" (IM.equal (=) (IM.filter (fun x y -> x) !im1 !im2) !im)
;;

(* testing fold *)
let test_fold () =
  assert_equal ~msg:"test 1"
    (IM.fold (fun k x n -> k + x + n) !im1 0)
    (M.fold (fun k x n -> k + x + n) !m1 0)
;;

(* testing map *)
let test_map () =
  let im = IM.map (fun x -> 2 * x) !im1 in
  let m = M.map (fun x -> 2 * x) !m1 in
  IM.iter (fun k x ->
          assert_equal ~msg:"test 1" (M.find k m) x
    ) im;
  M.iter (fun k x ->
          assert_equal ~msg:"test 2" (IM.find k im) x
    ) m;
;;

(* testing map *)
let test_mapi () =
  let im = IM.mapi (fun k x -> k + x) !im1 in
  let m = M.mapi (fun k x -> k + x) !m1 in
  IM.iter (fun k x ->
          assert_equal ~msg:"test 1" (M.find k m) x
    ) im;
  M.iter (fun k x ->
          assert_equal ~msg:"test 2" (IM.find k im) x
    ) m;
;;

(* testing choose *)
let test_choose () =
  let im = ref !im1 in
  for i = 1 to n do
    try
      let k, x = IM.choose !im in
      assert_equal ~msg:"test 1" (IM.find k !im) x;
      im := IM.remove k !im;
    with Not_found -> assert_bool "test 2" (IM.is_empty !im)
  done;
;;

let suite = "Intset tests" >::: [
  "test_mem" >:: test_mem;
  "test_find_iter" >:: test_find_iter;
  "test_add_f" >:: test_add_f;
  "test_remove" >:: test_remove;
  "test_replace" >:: test_replace;
  "test_replace_f" >:: test_replace_f;
  "test_equal" >:: test_equal;
  "test_union" >:: test_union;
  "test_fiter" >:: test_filter;
  "test_diff" >:: test_diff;
  "test_iter2" >:: test_iter2;
  "test_iter_s" >:: test_iter_s;
  "test_fold" >:: test_fold;
  "test_map" >:: test_map;
  "test_mapi" >:: test_mapi;
  "test_choose" >:: test_choose;
  ]

let _ =
  let verbose = ref true in
  if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
    exit 1