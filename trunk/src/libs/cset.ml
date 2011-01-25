(* sets of consed elements based on a variation of Patricia trees *)

open Consed

type 'a elt = 'a consed
let intright x y z = x lxor y > x lxor z

type 'a s =
  | SEmpty
  | SNode of 'a s * 'a * 'a s

type 'a t =
  | Empty
  | Node of 'a * 'a s

type 'a _elt = 'a elt
type 'a _s = 'a s
type 'a _t = 'a t

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let singleton elt = Node (elt, SEmpty)

let is_singleton = function Node (_, SEmpty) -> true | _ -> false

let rec mem_s xt mt = function
  (* invariant: xt > mt *)
  | SEmpty -> false
  | SNode (l, mr, r) ->
      if xt > mr.tag then mem_s xt mr.tag r
      else xt == mr.tag || mem_s xt mt l
let mem x = function
  | Empty -> false
  | Node (m, s) ->
      if x.tag > m.tag then mem_s x.tag m.tag s else x == m

let rec iter_incr_s f = function
  | SEmpty -> ()
  | SNode (l, mr, r) -> iter_incr_s f l; f mr; iter_incr_s f r
let iter_incr f = function
  | Empty -> ()
  | Node (m, s) -> f m; iter_incr_s f s

let rec iter_decr_s f = function
  | SEmpty -> ()
  | SNode (l, mr, r) -> iter_decr_s f r; f mr; iter_decr_s f l
let iter_decr f = function
  | Empty -> ()
  | Node (m, s) -> iter_decr_s f s; f m

let iter_s = iter_incr_s
let iter = iter_incr

let rec iter2_aux f m1t s1 m2 s2 = match s1, s2 with
  (* invariant m2.tag >= m1t *)
  | SEmpty, _ -> if m1t == m2.tag then f m2 else ()
  | _, SEmpty -> if m1t == m2.tag || mem_s m2.tag m1t s1 then f m2 else ()
  | SNode (l1, mr1, r1), SNode (l2, mr2, r2) ->
      if mr2.tag > mr1.tag then
        if m2.tag lxor m1t > m2.tag lxor mr2.tag then
          if m2.tag > mr1.tag then iter2_aux f mr1.tag r1 m2 s2
          else iter2_aux f m2.tag s2 mr1 r1
        else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
          (iter2_aux f m1t l1 m2 l2; iter2_aux f mr1.tag r1 mr2 r2)
        else iter2_aux f m1t s1 m2 l2
      else (* if mr2.tag <= mr1t *)
      if m2.tag lxor m1t > m2.tag lxor mr1.tag then iter2_aux f m2.tag s2 mr1 r1
      else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
        (iter2_aux f m1t l1 m2 l2; iter2_aux f mr2.tag r2 mr1 r1)
      else iter2_aux f m1t l1 m2 s2
let iter2 f set1 set2 = match set1, set2 with
  | Empty, _ -> ()
  | _, Empty -> ()
  | Node (m1, s1), Node (m2, s2) ->
      if m2.tag > m1.tag then iter2_aux f m1.tag s1 m2 s2
      else iter2_aux f m2.tag s2 m1 s1

exception Unchanged

let rec add_s_min x mt = function
  | SEmpty -> SNode (SEmpty, x, SEmpty)
  | SNode (l, mr, r) as s ->
      if x.tag lxor mt > x.tag lxor mr.tag then SNode (SEmpty, x, s)
      else SNode (add_s_min x mt l, mr, r)
let rec add_s x mt = function
  (* invariant: x.tag > mt *)
  | SEmpty -> SNode (SEmpty, x, SEmpty)
  | SNode (l, mr, r) as s ->
      if x.tag > mr.tag then
        if mr.tag lxor mt > mr.tag lxor x.tag then SNode (l, mr, add_s x mr.tag r)
        else SNode (s, x, SEmpty)
      else if x == mr then raise Unchanged
      else (* if x.tag < mr.tag *)
      if x.tag lxor mt > x.tag lxor mr.tag then SNode (l, x, add_s_min mr x.tag r)
      else SNode (add_s x mt l, mr, r)
let add x = function
  | Empty -> Node (x, SEmpty)
  | Node (m, s) as set ->
      if x.tag > m.tag then
        try Node (m, add_s x m.tag s)
        with Unchanged -> set
      else if x == m then set
      else (* if x.tag < m.tag *)
      Node (x, add_s_min m x.tag s)

let rec pop_min = function
  | SEmpty -> raise Not_found
  | SNode (l, mr, r) ->
      if l == SEmpty then mr, r
      else let ml, ll = pop_min l
        in ml, SNode (ll, mr, r)

let rec set_of_s = function
  | SEmpty -> Empty
  | SNode (l, mr, r) ->
      if l == SEmpty then Node (mr, r)
      else let ml, ll = pop_min l in
        Node (ml, SNode (ll, mr, r))

let rec remove_s xt mt = function
  (* invariant: xt > mt *)
  | SEmpty -> raise Unchanged
  | SNode (l, mr, r) ->
      if xt > mr.tag then SNode (l, mr, remove_s xt mr.tag r)
      else if xt < mr.tag then SNode (remove_s xt mt l, mr, r)
      else (* if xt == mr.tag *)
      if r == SEmpty then l
      else let mrr, rr = pop_min r in SNode (l, mrr, rr)
let remove x = function
  | Empty -> Empty
  | Node (m, s) as set ->
      if x.tag > m.tag then
        try Node (m, remove_s x.tag m.tag s)
        with Unchanged -> set
      else if x == m then set_of_s s
      else set

let rec iter_remove_s f xt mt = function
  (* invariant: e > m *)
  | SEmpty -> ()
  | SNode (l, mr, r) ->
      if xt > mr.tag then (iter_s f l; f mr; iter_remove_s f xt mr.tag r)
      else if xt < mr.tag then (iter_remove_s f xt mt l; f mr; iter_s f r)
      else (* if xt = mr.tag *)
      (iter_s f l; iter_s f r)
let iter_remove f x = function
  | Empty -> ()
  | Node (m, s) ->
      if x.tag > m.tag then (f m; iter_remove_s f x.tag m.tag s)
      else if x == m then iter_s f s
      else (f m; iter_s f s)

let divide = function
  | Empty -> Empty, Empty
  | Node (m, SEmpty) as set -> set, Empty
  | Node (m, SNode (l, mr, r)) -> Node (m, l), Node (mr, r)

let join set1 set2 = match set1, set2 with
  | Empty, _ -> set2
  | _, Empty -> set1
  | Node (m1, s1), Node (m2, s2) -> Node (m1, SNode (s1, m2, s2))

let rec inter_aux m1t s1 m2 s2 = match s1, s2 with
  (* invariant m2.tag >= m1t *)
  | SEmpty, _ -> if m1t == m2.tag then Node (m2, SEmpty) else Empty
  | _, SEmpty -> if m1t == m2.tag || mem_s m2.tag m1t s1 then
        Node (m2, SEmpty) else Empty
  | SNode (l1, mr1, r1), SNode (l2, mr2, r2) ->
      if mr2.tag > mr1.tag then
        if m2.tag lxor m1t > m2.tag lxor mr2.tag then
          if m2.tag > mr1.tag then inter_aux mr1.tag r1 m2 s2
          else inter_aux m2.tag s2 mr1 r1
        else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
          join (inter_aux m1t l1 m2 l2) (inter_aux mr1.tag r1 mr2 r2)
        else inter_aux m1t s1 m2 l2
      else (* if mr2.tag <= mr1.tag *)
      if m2.tag lxor m1t > m2.tag lxor mr1.tag then inter_aux m2.tag s2 mr1 r1
      else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
        join (inter_aux m1t l1 m2 l2) (inter_aux mr2.tag r2 mr1 r1)
      else inter_aux m1t l1 m2 s2
let inter set1 set2 = match set1, set2 with
  | Empty, _ -> Empty
  | _, Empty -> Empty
  | Node (m1, s1), Node (m2, s2) ->
      if m2.tag > m1.tag then inter_aux m1.tag s1 m2 s2
      else inter_aux m2.tag s2 m1 s1

let rec union_s m1t s1 m2 s2 = match s1, s2 with
  (* invariant: m2.tag >= m1t; computes the union of s1, m2, s2 minus m1 *)
  | SEmpty, _ -> if m1t == m2.tag then s2 else add_s_min m2 m1t s2
  | _, SEmpty -> if m1t == m2.tag then s1
      else (try add_s m2 m1t s1 with Unchanged -> s1)
  | SNode (l1, mr1, r1), SNode (l2, mr2, r2) ->
      if mr2.tag > mr1.tag then
        if m2.tag lxor m1t > m2.tag lxor mr2.tag then
          if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
            if m2.tag > mr1.tag then SNode (l1, mr1, union_s mr1.tag r1 m2 s2)
            else SNode (l1, m2, union_s m2.tag s2 mr1 r1)
          else SNode (s1, m2, s2)
        else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
          SNode (union_s m1t l1 m2 l2, mr1, union_s mr1.tag r1 mr2 r2)
        else SNode (union_s m1t s1 m2 l2, mr2, r2)
      else (* if mr2.tag < mr1.tag *)
      if m2.tag lxor m1t > m2.tag lxor mr1.tag then SNode (l1, m2, union_s m2.tag s2 mr1 r1)
      else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
        SNode (union_s m1t l1 m2 l2, mr2, union_s mr2.tag r2 mr1 r1)
      else SNode (union_s m1t l1 m2 s2, mr1, r1)
let union set1 set2 = match set1, set2 with
  | Empty, _ -> set2
  | _, Empty -> set1
  | Node (m1, s1), Node (m2, s2) ->
      if m2.tag > m1.tag then Node (m1, union_s m1.tag s1 m2 s2)
      else Node (m2, union_s m2.tag s2 m1 s1)

(** subset is not a symmetric operation therefore we need two auxiliary funcitons *)
let rec is_subset_aux m1t s1 m2t s2 = match s1, s2 with
  (* invariant: m2t >= m1t; computes union of m1 and s1 minus the union of *)
  (* m2 and s2                                                             *)
  | SEmpty, _ -> m1t == m2t
  | _, SEmpty -> false (* the only true is alredy covered by the previous case *)
  | SNode (l1, mr1, r1), SNode (l2, mr2, r2) ->
      if mr2.tag > mr1.tag then
        if m2t lxor m1t > m2t lxor mr2.tag then false
        else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
          (is_subset_aux m1t l1 m2t l2) && (is_subset_aux mr1.tag r1 mr2.tag r2)
        else is_subset_aux m1t s1 m2t l2
      else (* if mr2.tag < mr1.tag *)
      if m2t lxor m1t > m2t lxor mr1.tag then false
      else mr2.tag lxor m1t > mr2.tag lxor mr1.tag &&
        is_subset_aux m1t l1 m2t l2 && is_supset_aux mr2.tag r2 mr1.tag r1
and is_supset_aux m1t s1 m2t s2 = match s1, s2 with
  (* invariant: m2t >= m1t; computes union of m2t and s2 minus the union   *)
  (* of m1t and s1                                                         *)
  | _, SEmpty -> m1t == m2t || mem_s m2t m1t s1
  | SEmpty, _ -> false (* the only true is alredy covered by the previous case *)
  | SNode (l1, mr1, r1), SNode (l2, mr2, r2) ->
      if mr2.tag > mr1.tag then
        if m2t lxor m1t > m2t lxor mr2.tag then
          if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
            if m2t > mr1.tag then is_supset_aux mr1.tag r1 m2t s2
            else is_subset_aux m2t s2 mr1.tag r1
          else false
        else mr1.tag lxor m1t > mr1.tag lxor mr2.tag &&
          is_supset_aux m1t l1 m2t l2 && is_supset_aux mr1.tag r1 mr2.tag r2
      else (* if mr2.tag < mr1.tag *)
      if m2t lxor m1t > m2t lxor mr1.tag then is_subset_aux m2t s2 mr1.tag r1
      else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
        (is_supset_aux m1t l1 m2t l2) && (is_subset_aux mr2.tag r2 mr1.tag r1)
      else is_supset_aux m1t l1 m2t s2
let is_subset set1 set2 = match set1, set2 with
  | Empty, _ -> true
  | _, Empty -> false (* the only true is alredy covered by the previous case *)
  | Node (m1, s1), Node (m2, s2) ->
      if m2.tag > m1.tag then is_subset_aux m1.tag s1 m2.tag s2
      else is_supset_aux m2.tag s2 m1.tag s1

(** diff is not a symmetric operation therefore we need two auxiliary funcitons *)
let rec diff_aux m1 s1 m2t s2 = match s1, s2 with
  (* invariant: m2t >= m1.tag; computes union of m1 and s1 minus the     *)
  (* union of m2t and s2                                                   *)
  | SEmpty, _ -> if m1.tag == m2t then Empty else Node (m1, SEmpty)
  | _, SEmpty -> if m1.tag == m2t then set_of_s s1
      else (try Node (m1, remove_s m2t m1.tag s1) with Unchanged -> Node (m1, s1))
  | SNode (l1, mr1, r1), SNode (l2, mr2, r2) ->
      if mr2.tag > mr1.tag then
        if m2t lxor m1.tag > m2t lxor mr2.tag then
          if mr1.tag lxor m1.tag > mr1.tag lxor mr2.tag then
            if m2t > mr1.tag then join (Node (m1, l1)) (diff_aux mr1 r1 m2t s2)
            else join (Node (m1, l1)) (diffi_aux m2t s2 mr1 r1)
          else Node (m1, s1)
        else if mr1.tag lxor m1.tag > mr1.tag lxor mr2.tag then
          join (diff_aux m1 l1 m2t l2) (diff_aux mr1 r1 mr2.tag r2)
        else diff_aux m1 s1 m2t l2
      else (* if mr2.tag < mr1.tag *)
      if m2t lxor m1.tag > m2t lxor mr1.tag then
        join (Node (m1, l1)) (diffi_aux m2t s2 mr1 r1)
      else if mr2.tag lxor m1.tag > mr2.tag lxor mr1.tag then
        join (diff_aux m1 l1 m2t l2) (diffi_aux mr2.tag r2 mr1 r1)
      else join (diff_aux m1 l1 m2t s2) (Node (mr1, r1))
and diffi_aux m1t s1 m2 s2 = match s1, s2 with
  (* invariant: m2.tag >= m1t; computes union of m2 and s2 minus the     *)
  (* union of m1 and s1                                                    *)
  | SEmpty, _ -> if m1t == m2.tag then set_of_s s2 else Node (m2, s2)
  | _, SEmpty -> if m1t == m2.tag || mem_s m2.tag m1t s1 then Empty
      else Node (m2, SEmpty)
  | SNode (l1, mr1, r1), SNode (l2, mr2, r2) ->
      if mr2.tag > mr1.tag then
        if m2.tag lxor m1t > m2.tag lxor mr2.tag then
          if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
            if m2.tag > mr1.tag then diffi_aux mr1.tag r1 m2 s2
            else diff_aux m2 s2 mr1.tag r1
          else Node (m2, s2)
        else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
          join (diffi_aux m1t l1 m2 l2) (diffi_aux mr1.tag r1 mr2 r2)
        else join (diffi_aux m1t s1 m2 l2) (Node (mr2, r2))
      else (* if mr2.tag < mr1.tag *)
      if m2.tag lxor m1t > m2.tag lxor mr1.tag then diff_aux m2 s2 mr1.tag r1
      else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
        join (diffi_aux m1t l1 m2 l2) (diff_aux mr2 r2 mr1.tag r1)
      else diffi_aux m1t l1 m2 s2
let diff set1 set2 = match set1, set2 with
  | Empty, _ -> Empty
  | _, Empty -> set1
  | Node (m1, s1), Node (m2, s2) ->
      if m2.tag > m1.tag then diff_aux m1 s1 m2.tag s2
      else diffi_aux m2.tag s2 m1 s1

(** iter_diff is not a symmetric operation therefore we need two auxiliary funcitons *)
let rec iter_diff_aux f m1 s1 m2t s2 = match s1, s2 with
  (* invariant: m2t >= m1.tag; computes union of m1 and s1 minus the     *)
  (* union of m2 and s2                                                    *)
  | SEmpty, _ -> if m1.tag == m2t then () else f m1
  | _, SEmpty -> if m1.tag == m2t then iter_s f s1 else
        (f m1; iter_remove_s f m2t m1.tag s1)
  | SNode (l1, mr1, r1), SNode (l2, mr2, r2) ->
      if mr2.tag > mr1.tag then
        if m2t lxor m1.tag > m2t lxor mr2.tag then
          if mr1.tag lxor m1.tag > mr1.tag lxor mr2.tag then
            if m2t > mr1.tag then (f m1; iter_s f l1; iter_diff_aux f mr1 r1 m2t s2)
            else (f m1; iter_s f l1; iter_diffi_aux f m2t s2 mr1 r1)
          else (f m1; iter_s f s1)
        else if mr1.tag lxor m1.tag > mr1.tag lxor mr2.tag then
          (iter_diff_aux f m1 l1 m2t l2; iter_diff_aux f mr1 r1 mr2.tag r2)
        else iter_diff_aux f m1 s1 m2t l2
      else (* if mr2.tag < mr1.tag *)
      if m2t lxor m1.tag > m2t lxor mr1.tag then
        (f m1; iter_s f l1; iter_diffi_aux f m2t s2 mr1 r1)
      else if mr2.tag lxor m1.tag > mr2.tag lxor mr1.tag then
        (iter_diff_aux f m1 l1 m2t l2; iter_diffi_aux f mr2.tag r2 mr1 r1)
      else (iter_diff_aux f m1 l1 m2t s2; f mr1; iter_s f r1)
and iter_diffi_aux f m1t s1 m2 s2 = match s1, s2 with
  (* invariant: m2.tag >= m1t; computes union of m2 and s2 minus the     *)
  (* union of m1 and s1                                                    *)
  | SEmpty, _ -> if m1t == m2.tag then iter_s f s2 else (f m2; iter_s f s2)
  | _, SEmpty -> if not (m1t == m2.tag || mem_s m2.tag m1t s1) then f m2
  | SNode (l1, mr1, r1), SNode (l2, mr2, r2) ->
      if mr2.tag > mr1.tag then
        if m2.tag lxor m1t > m2.tag lxor mr2.tag then
          if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
            if m2.tag > mr1.tag then iter_diffi_aux f mr1.tag r1 m2 s2
            else iter_diff_aux f m2 s2 mr1.tag r1
          else (f m2; iter_s f s2)
        else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
          (iter_diffi_aux f m1t l1 m2 l2; iter_diffi_aux f mr1.tag r1 mr2 r2)
        else (iter_diffi_aux f m1t s1 m2 l2; f mr2; iter_s f r2)
      else (* if mr2.tag < mr1.tag *)
      if m2.tag lxor m1t > m2.tag lxor mr1.tag then iter_diff_aux f m2 s2 mr1.tag r1
      else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
        (iter_diffi_aux f m1t l1 m2 l2; iter_diff_aux f mr2 r2 mr1.tag r1)
      else iter_diffi_aux f m1t l1 m2 s2
let iter_diff f set1 set2 = match set1, set2 with
  | Empty, _ -> ()
  | _, Empty -> iter f set1
  | Node (m1, s1), Node (m2, s2) ->
      if m2.tag > m1.tag then iter_diff_aux f m1 s1 m2.tag s2
      else iter_diffi_aux f m2.tag s2 m1 s1

let rec fold_s f s accu =
  match s with
  | SEmpty -> accu
  | SNode (l, mr, r) -> fold_s f r (f mr (fold_s f l accu))
let fold f set accu =
  match set with
  | Empty -> accu
  | Node (m, s) -> fold_s f s (f m accu)

let rec for_all_s p = function
  | SEmpty -> true
  | SNode (l, mr, r) -> p mr && for_all_s p l && for_all_s p r
let for_all p = function
  | Empty -> true
  | Node (m, s) -> p m && for_all_s p s

let rec exists_s p = function
  | SEmpty -> false
  | SNode (l, mr, r) -> p mr || exists_s p l || exists_s p r
let exists p = function
  | Empty -> false
  | Node (m, s) -> p m || exists_s p s

let rec filter_s p m = function
  | SEmpty -> if p m then Node (m, SEmpty) else Empty
  | SNode (l, mr, r) -> join (filter_s p m l) (filter_s p mr r)
let filter p = function
  | Empty -> Empty
  | Node (m, s) -> filter_s p m s

let rec part_aux p m = function
  | SEmpty -> if p m then Node (m, SEmpty), Empty else Empty, Node (m, SEmpty)
  | SNode (l, mr, r) ->
      let lt, lf = part_aux p m l in
      let rt, rf = part_aux p mr r in
      join lt rt, join lf rf
let partition p = function
  | Empty -> (Empty, Empty)
  | Node (m, s) -> part_aux p m s

let rec cardinal_s = function
  | SEmpty -> 0
  | SNode (l, _, r) -> cardinal_s l + 1 + cardinal_s r
let cardinal = function
  | Empty -> 0
  | Node (_, s) -> cardinal_s s + 1

let rec elements_s accu = function
  | SEmpty -> accu
  | SNode (l, mr, r) -> elements_s (mr :: elements_s accu r) l
let elements = function
  | Empty -> []
  | Node (m, s) -> m :: elements_s [] s

let min_elt = function
  | Empty -> raise Not_found
  | Node (m, s) -> m

let rec max_elt_s = function
  | SEmpty -> raise Not_found
  | SNode (l, mr, r) ->
      if r == SEmpty then mr
      else max_elt_s r
let max_elt = function
  | Empty -> raise Not_found
  | Node (m, s) ->
      if s == SEmpty then m
      else max_elt_s s

let choose = min_elt

(* Splitting. split x s returns a triple (l, present, r) where - l is the  *)
(* set of elements of s that are < x - r is the set of elements of s that  *)
(* are > x - present is false if s contains no element equal to x, or true *)
(* if s contains an element equal to x.                                    *)

let rec split_aux xt m = function
  (* invariant: xt > m.tag *)
  | SEmpty -> Node (m, SEmpty), false, Empty
  | SNode (l, mr, r) ->
      if xt > mr.tag then
        let rl, rpres, rr = split_aux xt mr r in
        join (Node (m, l)) rl, rpres, rr
      else if xt == mr.tag then
        Node (m, l), true, set_of_s r
      else (* if xt < mr.tag *)
      let ll, lpres, lr = split_aux xt m l in
      ll, lpres, join lr (Node (mr, r))
let split x = function
  | Empty -> Empty, false, Empty
  | Node (m, s) as set ->
      if x.tag > m.tag then split_aux x.tag m s
      else if x == m then Empty, true, set_of_s s
      else (* if x.tag < m.tag *) Empty, false, set

(* ordering Empty < Node *)
let rec compare_s s1 s2 = match s1, s2 with
  | SEmpty, SEmpty -> 0
  | SEmpty, _ -> - 1
  | _, SEmpty -> 1
  | SNode (l1, mr1, r1), SNode (l2, mr2, r2) ->
      if mr1 <> mr2 then mr1.tag - mr2.tag
      else let c = compare_s l1 l2 in
        if c <> 0 then c else compare_s r1 r2
let compare set1 set2 = match set1, set2 with
  | Empty, Empty -> 0
  | Empty, _ -> - 1
  | _, Empty -> 1
  | Node (m1, s1), Node (m2, s2) ->
      if m1 <> m2 then m1.tag - m2.tag else compare_s s1 s2

let rec equal_s s1 s2 = match s1, s2 with
  | SEmpty, SEmpty -> true
  | SNode (l1, mr1, r1), SNode (l2, mr2, r2) ->
      mr1 == mr2 && equal_s l1 l2 && equal_s r1 r2
  | _ -> false
let equal set1 set2 = match set1, set2 with
  | Empty, Empty -> true
  | Node (m1, s1), Node (m2, s2) ->
      m1 == m2 && equal_s s1 s2
  | _ -> false

let rec hash_param_s d s accu =
  match s with
  | SEmpty -> accu
  | SNode (l, mr, r) -> mr.tag + accu +
      (if d <> 1 then hash_param_s (d - 1) l (hash_param_s (d - 1) r accu) else 0)
let hash_param d = function
  | Empty -> 0
  | Node (m, s) -> hash_param_s d s m.tag

let hash set = hash_param 0 set

let rec equal_right_s s1 s2 s3 = match s1, s2, s3 with
  (* assertion: compare_s s1 s2 >= 0; compare_s s1 s3 <=0 returns 0 if     *)
  (* equal_s s2 s3 = 0, > 0 if s1 lxor s2 > s1 lxor s3, else < 0                 *)
  | _, _, SEmpty -> 0 (* since s1 = SEmpty and s2 = SEmpty *)
  | SEmpty, _, _ -> - 1 (* since s2 = SEmpty and s3 <> SEmpty *)
  | _, SEmpty, _ -> 1 (* since s1 <> SEmpty and s3 <> SEmpty *)
  | SNode (l1, mr1, r1), SNode (l2, mr2, r2), SNode (l3, mr3, r3) ->
      if mr2 == mr3 then
        let er = equal_right_s l1 l2 l3 in
        if er <> 0 then er
        else equal_right_s r1 r2 r3
      else if mr1.tag lxor mr2.tag > mr1.tag lxor mr3.tag then 1 else - 1
let right set1 set2 set3 = match set1, set2, set3 with
  | _, _, Empty -> true (* when set1 = set2 = set3 = Empty *)
  | _, Empty, _ -> false (* since set1 = Empty, but set3 <> Empty *)
  | Empty, _, _ -> true (* since set2 <> Empty and set3 <> Empty *)
  | Node (m1, s1), Node (m2, s2), Node (m3, s3) ->
      if m2 == m3 then equal_right_s s1 s2 s3 > 0
      else m1.tag lxor m2.tag > m1.tag lxor m3.tag

module type T = sig
  type t
end

module type S = sig
  type elt
  type s = elt _s
  type t = elt _t
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val mem_s: int -> int -> s -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val is_singleton : t -> bool
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val divide : t -> t * t
  val diff: t -> t -> t
  val is_subset: t -> t -> bool
  (* iteration in the increasing order of indexes *)
  val iter_incr: (elt -> unit) -> t -> unit
  (* iteration in the decreasing order of indexes *)
  val iter_decr: (elt -> unit) -> t -> unit
  val iter: (elt -> unit) -> t -> unit
  val iter2 : (elt -> unit) -> t -> t -> unit
  val iter_diff : (elt -> unit) -> t -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val filter: (elt -> bool) -> t -> t
  val partition: (elt -> bool) -> t -> t * t
  val cardinal: t -> int
  val elements: t -> elt list
  val min_elt: t -> elt
  val max_elt: t -> elt
  val choose: t -> elt
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash_param: int -> t -> int
  val hash: t -> int
  val right: t -> t -> t -> bool
end

module Make (T : T) = struct
  type elt = T.t _elt
  type s = elt _s
  type t = elt _t
  let empty = empty
  let is_empty = is_empty
  let mem = mem
  let mem_s = mem_s
  let add = add
  let singleton = singleton
  let is_singleton = is_singleton
  let remove = remove
  let union = union
  let iter_incr = iter_incr
  let iter_decr = iter_decr
  let inter = inter
  let divide = divide
  let diff = diff
  let is_subset = is_subset
  let iter = iter
  let iter2 = iter2
  let iter_diff = iter_diff
  let fold = fold
  let for_all = for_all
  let exists = exists
  let filter = filter
  let partition = partition
  let cardinal = cardinal
  let elements = elements
  let min_elt = min_elt
  let max_elt = max_elt
  let choose = choose
  let equal = equal
  let compare = compare
  let hash_param = hash_param
  let hash = hash
  let right = right
end