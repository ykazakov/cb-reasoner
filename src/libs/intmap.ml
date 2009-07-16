(* maps on integers based on a variation of Patricia trees *)

module type S = sig
  type key = int
  type 'a t
  exception Remove
  val empty: 'a t
  val is_empty: 'a t -> bool
  val singleton : key -> 'a -> 'a t
  val is_singleton : 'a t -> bool
  val add: key -> 'a -> 'a t -> 'a t
  val add_f : key -> (unit -> 'a) -> ('a -> 'a) -> 'a t -> 'a t
  val replace : key -> 'a -> 'a t -> 'a t
  val replace_f : key -> ('a -> 'a) -> 'a t -> 'a t
  val find: key -> 'a t -> 'a
  val remove: key -> 'a t -> 'a t
  val mem: key -> 'a t -> bool
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val iter2 : (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val filter : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val diff : 'a t -> 'b t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val choose : 'a t -> (key * 'a)
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val right: ('a -> 'a -> bool) -> ('a -> 'a -> 'a -> bool) -> 'a t -> 'a t -> 'a t -> bool
  module Set : Iset.S with type elt = key
  val iter_s : (key -> 'a -> unit) -> 'a t -> Set.t -> unit
end

type key = int
let intright x y z = x lxor y > x lxor z

type 'a s =
  | SEmpty
  | SNode of 'a s * key * 'a * 'a s

type 'a t =
  | Empty
  | Node of key * 'a * 'a s

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let singleton k x = Node (k, x, SEmpty)

let is_singleton = function Node (_, _, SEmpty) -> true | _ -> false

let rec mem_s k m = function
  (* invariant: k > m *)
  | SEmpty -> false
  | SNode (l, mr, xr, r) ->
      if k > mr then mem_s k mr r
      else k == mr || mem_s k m l
let mem k = function
  | Empty -> false
  | Node (m, x, s) ->
      if k > m then mem_s k m s else k == m

let rec find_s k m = function
  (* invariant: k > m *)
  | SEmpty -> raise Not_found
  | SNode (l, mr, xr, r) ->
      if k > mr then find_s k mr r
      else if k == mr then xr
      else find_s k m l
let find k = function
  | Empty -> raise Not_found
  | Node (m, x, s) ->
      if k > m then find_s k m s
      else if k == m then x
      else raise Not_found

let rec replace_s k y m = function
  (* invariant: k > m *)
  | SEmpty -> raise Not_found
  | SNode (l, mr, xr, r) ->
      if k > mr then SNode (l, mr, xr, replace_s k y mr r)
      else if k == mr then SNode (l, mr, y, r)
      else SNode (replace_s k y m l, mr, xr, r)
let replace k y = function
  | Empty -> raise Not_found
  | Node (m, x, s) ->
      if k > m then Node (m, x, replace_s k y m s)
      else if k == m then Node (m, y, s)
      else raise Not_found

let rec iter_s f = function
  | SEmpty -> ()
  | SNode (l, mr, xr, r) -> iter_s f l; f mr xr; iter_s f r
let iter f = function
  | Empty -> ()
  | Node (m, x, s) -> f m x; iter_s f s

let rec iter2_aux f m1 x1 s1 m2 x2 s2 = match s1, s2 with
  (* invariant m2 >= m1 *)
  | SEmpty, _ -> if m1 == m2 then f m1 x1 x2 else ()
  | _, SEmpty -> if m1 == m2 then f m1 x1 x2
      else (try let y1 = find_s m2 m1 s1 in f m2 y1 x2
      with Not_found -> ())
  | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
      if mr2 > mr1 then
        if intright m2 m1 mr2 then
          (*| if intright mr1 m1 mr2 then*)
          if m2 > mr1 then iter2_aux f mr1 xr1 r1 m2 x2 s2
          else iter2i_aux f m2 x2 s2 mr1 xr1 r1
        (*| else ()*)
        else if intright mr1 m1 mr2 then
          (iter2_aux f m1 x1 l1 m2 x2 l2; iter2_aux f mr1 xr1 r1 mr2 xr2 r2)
        else iter2_aux f m1 x1 s1 m2 x2 l2
      else (* if mr2 <= mr1 *)
      if intright m2 m1 mr1 then iter2i_aux f m2 x2 s2 mr1 xr1 r1
      else if intright mr2 m1 mr1 then
        (iter2_aux f m1 x1 l1 m2 x2 l2; iter2i_aux f mr2 xr2 r2 mr1 xr1 r1)
      else iter2_aux f m1 x1 l1 m2 x2 s2
and iter2i_aux f m1 x1 s1 m2 x2 s2 = match s1, s2 with
  (* invariant m2 >= m1 *)
  | SEmpty, _ -> if m1 == m2 then f m1 x2 x1 else ()
  | _, SEmpty -> if m1 == m2 then f m1 x2 x1
      else (try let y1 = find_s m2 m1 s1 in f m2 x2 y1
      with Not_found -> ())
  | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
      if mr2 > mr1 then
        if intright m2 m1 mr2 then
          (*| if intright mr1 m1 mr2 then*)
          if m2 > mr1 then iter2i_aux f mr1 xr1 r1 m2 x2 s2
          else iter2_aux f m2 x2 s2 mr1 xr1 r1
        (*| else ()*)
        else if intright mr1 m1 mr2 then
          (iter2i_aux f m1 x1 l1 m2 x2 l2; iter2i_aux f mr1 xr1 r1 mr2 xr2 r2)
        else iter2i_aux f m1 x1 s1 m2 x2 l2
      else (* if mr2 <= mr1 *)
      if intright m2 m1 mr1 then iter2_aux f m2 x2 s2 mr1 xr1 r1
      else if intright mr2 m1 mr1 then
        (iter2i_aux f m1 x1 l1 m2 x2 l2; iter2_aux f mr2 xr2 r2 mr1 xr1 r1)
      else iter2i_aux f m1 x1 l1 m2 x2 s2
let iter2 f map1 map2 = match map1, map2 with
  | Empty, _ -> ()
  | _, Empty -> ()
  | Node (m1, x1, s1), Node (m2, x2, s2) ->
      if m2 > m1 then iter2_aux f m1 x1 s1 m2 x2 s2
      else iter2i_aux f m2 x2 s2 m1 x1 s1

let rec add_s_min k y m = function
  | SEmpty -> SNode (SEmpty, k, y, SEmpty)
  | SNode (l, mr, xr, r) as s ->
      if intright k m mr then SNode (SEmpty, k, y, s)
      else SNode (add_s_min k y m l, mr, xr, r)
let rec add_s k y m = function
  (* invariant: k > m *)
  | SEmpty -> SNode (SEmpty, k, y, SEmpty)
  | SNode (l, mr, xr, r) as s ->
      if k > mr then
        if intright mr m k then SNode (l, mr, xr, add_s k y mr r)
        else SNode (s, k, y, SEmpty)
      else if k == mr then SNode (l, mr, y, r)
      else (* if k < mr *)
      if intright k m mr then SNode (l, k, y, add_s_min mr xr k r)
      else SNode (add_s k y m l, mr, xr, r)
let add k y = function
  | Empty -> Node (k, y, SEmpty)
  | Node (m, x, s) ->
      if k > m then Node (m, x, add_s k y m s)
      else if k == m then Node (m, y, s)
      else (* if k < m *)
      Node (k, y, add_s_min m x k s)

let rec add_s_f k fresh replace m = function
  (* invariant: k > m *)
  | SEmpty -> SNode (SEmpty, k, fresh (), SEmpty)
  | SNode (l, mr, xr, r) as s ->
      if k > mr then
        if intright mr m k then SNode (l, mr, xr, add_s_f k fresh replace mr r)
        else SNode (s, k, fresh (), SEmpty)
      else if k == mr then SNode (l, mr, replace xr, r)
      else (* if k < mr *)
      if intright k m mr then SNode (l, k, fresh (), add_s_min mr xr k r)
      else SNode (add_s_f k fresh replace m l, mr, xr, r)
let add_f k fresh replace = function
  | Empty -> Node (k, fresh (), SEmpty)
  | Node (m, x, s) ->
      if k > m then Node (m, x, add_s_f k fresh replace m s)
      else if k == m then Node (m, replace x, s)
      else (* if k < m *)
      Node (k, fresh (), add_s_min m x k s)

let rec pop_min = function
  | SEmpty -> raise Not_found
  | SNode (l, mr, xr, r) ->
      if l == SEmpty then mr, xr, r
      else let ml, xl, ll = pop_min l
        in ml, xl, SNode (ll, mr, xr, r)

let rec map_of_s = function
  | SEmpty -> Empty
  | SNode (l, mr, xr, r) ->
      if l == SEmpty then Node (mr, xr, r)
      else let ml, xl, ll = pop_min l in
        Node (ml, xl, SNode (ll, mr, xr, r))

exception Unchanged

let rec remove_s k m = function
  (* invariant: k > m *)
  | SEmpty -> raise Unchanged
  | SNode (l, mr, xr, r) ->
      if k > mr then SNode (l, mr, xr, remove_s k mr r)
      else if k < mr then SNode (remove_s k m l, mr, xr, r)
      else (* if k == mr *)
      if r == SEmpty then l
      else let mrr, xrr, rr = pop_min r in SNode (l, mrr, xrr, rr)
let remove k = function
  | Empty -> Empty
  | Node (m, x, s) as map ->
      if k > m then
        try Node (m, x, remove_s k m s)
        with Unchanged -> map
      else if k == m then map_of_s s
      else map

let rec iter_remove_s f k m = function
  (* invariant: k > m *)
  | SEmpty -> ()
  | SNode (l, mr, xr, r) ->
      if k > mr then (iter_s f l; f mr xr; iter_remove_s f k mr r)
      else if k < mr then (iter_remove_s f k m l; f mr xr; iter_s f r)
      else (* if k == mr *)
      (iter_s f l; iter_s f r)
let iter_remove f k = function
  | Empty -> ()
  | Node (m, x, s) ->
      if k > m then (f m x; iter_remove_s f k m s)
      else if k == m then iter_s f s
      else (f m x; iter_s f s)

exception Remove

let rec replace_s_f k repl m = function
  (* invariant: k > m *)
  | SEmpty -> raise Not_found
  | SNode (l, mr, xr, r) ->
      if k > mr then SNode (l, mr, xr, replace_s_f k repl mr r)
      else if k == mr then
        try SNode (l, mr, repl xr, r)
        with Remove -> if r == SEmpty then l
            else let mrr, xrr, rr = pop_min r in
              SNode (l, mrr, xrr, rr)
      else (* if k < mr *) SNode (replace_s_f k repl m l, mr, xr, r)
let replace_f k repl = function
  | Empty -> raise Not_found
  | Node (m, x, s) ->
      if k > m then Node (m, x, replace_s_f k repl m s)
      else if k == m then
        try Node (m, repl x, s)
        with Remove -> map_of_s s
      else (* if k < m *) raise Not_found

let join map1 map2 = match map1, map2 with
  | Empty, _ -> map2
  | _, Empty -> map1
  | Node (m1, x1, s1), Node (m2, x2, s2) -> Node (m1, x1, SNode (s1, m2, x2, s2))

let rec filter_aux f m1 x1 s1 m2 x2 s2 = match s1, s2 with
  (* invariant m2 >= m1 *)
  | SEmpty, _ -> if m1 == m2 then Node (m1, f x1 x2, SEmpty) else Empty
  | _, SEmpty -> if m1 == m2 then Node (m1, f x1 x2, SEmpty) else
        (try let y1 = find_s m2 m1 s1 in Node (m2, f y1 x2, SEmpty)
        with Not_found -> Empty)
  | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
      if mr2 > mr1 then
        if intright m2 m1 mr2 then
          (*| if intright mr1 m1 mr2 then*)
          if m2 > mr1 then filter_aux f mr1 xr1 r1 m2 x2 s2
          else filteri_aux f m2 x2 s2 mr1 xr1 r1
        (*| else Empty*)
        else if intright mr1 m1 mr2 then
          join (filter_aux f m1 x1 l1 m2 x2 l2) (filter_aux f mr1 xr1 r1 mr2 xr2 r2)
        else filter_aux f m1 x1 s1 m2 x2 l2
      else (* if mr2 <= mr1 *)
      if intright m2 m1 mr1 then filteri_aux f m2 x2 s2 mr1 xr1 r1
      else if intright mr2 m1 mr1 then
        join (filter_aux f m1 x1 l1 m2 x2 l2) (filteri_aux f mr2 xr2 r2 mr1 xr1 r1)
      else filter_aux f m1 x1 l1 m2 x2 s2
and filteri_aux f m1 x1 s1 m2 x2 s2 = match s1, s2 with
  (* invariant m2 >= m1 *)
  | SEmpty, _ -> if m1 == m2 then Node (m1, f x2 x1, SEmpty) else Empty
  | _, SEmpty -> if m1 == m2 then Node (m1, f x2 x1, SEmpty) else
        (try let y1 = find_s m2 m1 s1 in Node (m2, f x2 y1, SEmpty)
        with Not_found -> Empty)
  | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
      if mr2 > mr1 then
        if intright m2 m1 mr2 then
          (*| if intright mr1 m1 mr2 then*)
          if m2 > mr1 then filteri_aux f mr1 xr1 r1 m2 x2 s2
          else filter_aux f m2 x2 s2 mr1 xr1 r1
        (*| else Empty*)
        else if intright mr1 m1 mr2 then
          join (filteri_aux f m1 x1 l1 m2 x2 l2) (filteri_aux f mr1 xr1 r1 mr2 xr2 r2)
        else filteri_aux f m1 x1 s1 m2 x2 l2
      else (* if mr2 <= mr1 *)
      if intright m2 m1 mr1 then filter_aux f m2 x2 s2 mr1 xr1 r1
      else if intright mr2 m1 mr1 then
        join (filteri_aux f m1 x1 l1 m2 x2 l2) (filter_aux f mr2 xr2 r2 mr1 xr1 r1)
      else filteri_aux f m1 x1 l1 m2 x2 s2
let filter f map1 map2 = match map1, map2 with
  | Empty, _ -> Empty
  | _, Empty -> Empty
  | Node (m1, x1, s1), Node (m2, x2, s2) ->
      if m2 > m1 then filter_aux f m1 x1 s1 m2 x2 s2
      else filteri_aux f m2 x2 s2 m1 x1 s1

let rec union_s f m1 x1 s1 m2 x2 s2 = match s1, s2 with
  (* invariant: m2 >= m1; computes the union of s1, m2, s2 minus m1; f is  *)
  (* a function resolving clashes                                          *)
  | SEmpty, _ -> if m1 == m2 then s2 else add_s_min m2 x2 m1 s2
  | _, SEmpty -> if m1 == m2 then s1 else add_s_f m2 (fun () -> x2) (fun x1 -> f x1 x2) m1 s1
  | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
      if mr2 > mr1 then
        if intright m2 m1 mr2 then
          if intright mr1 m1 mr2 then
            if m2 > mr1 then SNode (l1, mr1, xr1, union_s f mr1 xr1 r1 m2 x2 s2)
            else (* if m2 <= mr1 *) let xr = if m2 == mr1 then f xr1 x2 else x2 in
            SNode (l1, m2, xr, unioni_s f m2 x2 s2 mr1 xr1 r1)
          else SNode (s1, m2, x2, s2)
        else if intright mr1 m1 mr2 then
          SNode (union_s f m1 x1 l1 m2 x2 l2, mr1, xr1, union_s f mr1 xr1 r1 mr2 xr2 r2)
        else SNode (union_s f m1 x1 s1 m2 x2 l2, mr2, xr2, r2)
      else (* if mr2 <= mr1 *)
      if intright m2 m1 mr1 then SNode (l1, m2, x2, unioni_s f m2 x2 s2 mr1 xr1 r1)
      else if intright mr2 m1 mr1 then
        let xr = if mr2 == mr1 then f xr1 xr2 else xr2 in
        SNode (union_s f m1 x1 l1 m2 x2 l2, mr2, xr, unioni_s f mr2 xr2 r2 mr1 xr1 r1)
      else SNode (union_s f m1 x1 l1 m2 x2 s2, mr1, xr1, r1)
and unioni_s f m1 x1 s1 m2 x2 s2 = match s1, s2 with
  (* invariant: m2 >= m1; computes the union of s1, m2, s2 minus m1; f is  *)
  (* a function resolving clashes                                          *)
  | SEmpty, _ -> if m1 == m2 then s2 else add_s_min m2 x2 m1 s2
  | _, SEmpty -> if m1 == m2 then s1 else add_s_f m2 (fun () -> x2) (fun x1 -> f x2 x1) m1 s1
  | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
      if mr2 > mr1 then
        if intright m2 m1 mr2 then
          if intright mr1 m1 mr2 then
            if m2 > mr1 then SNode (l1, mr1, xr1, unioni_s f mr1 xr1 r1 m2 x2 s2)
            else (* if m2 <= mr1 *) let xr = if m2 == mr1 then f x2 xr1 else x2 in
            SNode (l1, m2, xr, union_s f m2 x2 s2 mr1 xr1 r1)
          else SNode (s1, m2, x2, s2)
        else if intright mr1 m1 mr2 then
          SNode (unioni_s f m1 x1 l1 m2 x2 l2, mr1, xr1, unioni_s f mr1 xr1 r1 mr2 xr2 r2)
        else SNode (unioni_s f m1 x1 s1 m2 x2 l2, mr2, xr2, r2)
      else (* if mr2 <= mr1 *)
      if intright m2 m1 mr1 then SNode (l1, m2, x2, union_s f m2 x2 s2 mr1 xr1 r1)
      else if intright mr2 m1 mr1 then
        let xr = if mr2 == mr1 then f xr2 xr1 else xr2 in
        SNode (unioni_s f m1 x1 l1 m2 x2 l2, mr2, xr, union_s f mr2 xr2 r2 mr1 xr1 r1)
      else SNode (unioni_s f m1 x1 l1 m2 x2 s2, mr1, xr1, r1)
let union f map1 map2 = match map1, map2 with
  | Empty, _ -> map2
  | _, Empty -> map1
  | Node (m1, x1, s1), Node (m2, x2, s2) ->
      if m2 > m1 then Node (m1, x1, union_s f m1 x1 s1 m2 x2 s2)
      else if m2 == m1 then Node (m1, f x1 x2, union_s f m1 x1 s1 m2 x2 s2)
      else (* if m2 < m1 *) Node (m2, x2, unioni_s f m2 x2 s2 m1 x1 s1)

(** diff is not a symmetric operation therefore we need two auxiliary funcitons *)
let rec diff_aux m1 x1 s1 m2 s2 = match s1, s2 with
  (* invariant: m2 >= m1; computes union of m1 and s1 minus the union of   *)
  (* m2 and s2                                                             *)
  | SEmpty, _ -> if m1 == m2 then Empty else Node (m1, x1, SEmpty)
  | _, SEmpty -> if m1 == m2 then map_of_s s1
      else (try Node (m1, x1, remove_s m2 m1 s1) with Unchanged -> Node (m1, x1, s1))
  | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, _, r2) ->
      if mr2 > mr1 then
        if intright m2 m1 mr2 then
          if intright mr1 m1 mr2 then
            if m2 > mr1 then join (Node (m1, x1, l1)) (diff_aux mr1 xr1 r1 m2 s2)
            else join (Node (m1, x1, l1)) (diffi_aux m2 s2 mr1 xr1 r1)
          else Node (m1, x1, s1)
        else if intright mr1 m1 mr2 then
          join (diff_aux m1 x1 l1 m2 l2) (diff_aux mr1 xr1 r1 mr2 r2)
        else diff_aux m1 x1 s1 m2 l2
      else (* if mr2 < mr1 *)
      if intright m2 m1 mr1 then join (Node (m1, x1, l1)) (diffi_aux m2 s2 mr1 xr1 r1)
      else if intright mr2 m1 mr1 then
        join (diff_aux m1 x1 l1 m2 l2) (diffi_aux mr2 r2 mr1 xr1 r1)
      else join (diff_aux m1 x1 l1 m2 s2) (Node (mr1, xr1, r1))
and diffi_aux m1 s1 m2 x2 s2 = match s1, s2 with
  (* invariant: m2 >= m1; computes union of m2 and s2 minus the union of   *)
  (* m1 and s1                                                             *)
  | SEmpty, _ -> if m1 == m2 then map_of_s s2 else Node (m2, x2, s2)
  | _, SEmpty -> if m1 == m2 || mem_s m2 m1 s1 then Empty else Node (m2, x2, SEmpty)
  | SNode (l1, mr1, _, r1), SNode (l2, mr2, xr2, r2) ->
      if mr2 > mr1 then
        if intright m2 m1 mr2 then
          if intright mr1 m1 mr2 then
            if m2 > mr1 then diffi_aux mr1 r1 m2 x2 s2
            else diff_aux m2 x2 s2 mr1 r1
          else Node (m2, x2, s2)
        else if intright mr1 m1 mr2 then
          join (diffi_aux m1 l1 m2 x2 l2) (diffi_aux mr1 r1 mr2 xr2 r2)
        else join (diffi_aux m1 s1 m2 x2 l2) (Node (mr2, xr2, r2))
      else (* if mr2 < mr1 *)
      if intright m2 m1 mr1 then diff_aux m2 x2 s2 mr1 r1
      else if intright mr2 m1 mr1 then
        join (diffi_aux m1 l1 m2 x2 l2) (diff_aux mr2 xr2 r2 mr1 r1)
      else diffi_aux m1 l1 m2 x2 s2
let diff map1 map2 = match map1, map2 with
  | Empty, _ -> Empty
  | _, Empty -> map1
  | Node (m1, x1, s1), Node (m2, _, s2) ->
      if m2 > m1 then diff_aux m1 x1 s1 m2 s2
      else diffi_aux m2 s2 m1 x1 s1

(** iter_diff is not a symmetric operation therefore we need two auxiliary funcitons *)
let rec iter_diff_aux f m1 x1 s1 m2 s2 = match s1, s2 with
  (* invariant: m2 >= m1; computes union of m1 and s1 minus the union of   *)
  (* m2 and s2                                                             *)
  | SEmpty, _ -> if m1 == m2 then () else f m1 x1
  | _, SEmpty -> if m1 == m2 then iter_s f s1 else (f m1 x1; iter_remove_s f m2 m1 s1)
  | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, _, r2) ->
      if mr2 > mr1 then
        if intright m2 m1 mr2 then
          if intright mr1 m1 mr2 then
            if m2 > mr1 then (f m1 x1; iter_s f l1; iter_diff_aux f mr1 xr1 r1 m2 s2)
            else (f m1 x1; iter_s f l1; iter_diffi_aux f m2 s2 mr1 xr1 r1)
          else (f m1 x1; iter_s f s1)
        else if intright mr1 m1 mr2 then
          (iter_diff_aux f m1 x1 l1 m2 l2; iter_diff_aux f mr1 xr1 r1 mr2 r2)
        else iter_diff_aux f m1 x1 s1 m2 l2
      else (* if mr2 < mr1 *)
      if intright m2 m1 mr1 then (f m1 x1; iter_s f l1; iter_diffi_aux f m2 s2 mr1 xr1 r1)
      else if intright mr2 m1 mr1 then
        (iter_diff_aux f m1 x1 l1 m2 l2; iter_diffi_aux f mr2 r2 mr1 xr1 r1)
      else (iter_diff_aux f m1 x1 l1 m2 s2; f mr1 xr1; iter_s f r1)
and iter_diffi_aux f m1 s1 m2 x2 s2 = match s1, s2 with
  (* invariant: m2 >= m1; computes union of m2 and s2 minus the union of   *)
  (* m1 and s1                                                             *)
  | SEmpty, _ -> if m1 == m2 then iter_s f s2 else (f m2 x2; iter_s f s2)
  | _, SEmpty -> if not (m1 == m2 || mem_s m2 m1 s1) then f m2 x2
  | SNode (l1, mr1, _, r1), SNode (l2, mr2, xr2, r2) ->
      if mr2 > mr1 then
        if intright m2 m1 mr2 then
          if intright mr1 m1 mr2 then
            if m2 > mr1 then iter_diffi_aux f mr1 r1 m2 x2 s2
            else iter_diff_aux f m2 x2 s2 mr1 r1
          else (f m2 x2; iter_s f s2)
        else if intright mr1 m1 mr2 then
          (iter_diffi_aux f m1 l1 m2 x2 l2; iter_diffi_aux f mr1 r1 mr2 xr2 r2)
        else (iter_diffi_aux f m1 s1 m2 x2 l2; f mr2 xr2; iter_s f r2)
      else (* if mr2 < mr1 *)
      if intright m2 m1 mr1 then iter_diff_aux f m2 x2 s2 mr1 r1
      else if intright mr2 m1 mr1 then
        (iter_diffi_aux f m1 l1 m2 x2 l2; iter_diff_aux f mr2 xr2 r2 mr1 r1)
      else iter_diffi_aux f m1 l1 m2 x2 s2
let iter_diff f map1 map2 = match map1, map2 with
  | Empty, _ -> ()
  | _, Empty -> iter f map1
  | Node (m1, x1, s1), Node (m2, _, s2) ->
      if m2 > m1 then iter_diff_aux f m1 x1 s1 m2 s2
      else iter_diffi_aux f m2 s2 m1 x1 s1

let rec map_s f = function
  | SEmpty -> SEmpty
  | SNode (l, mr, xr, r) -> SNode (map_s f l, mr, f xr, map_s f r)
let map f = function
  | Empty -> Empty
  | Node (m, x, s) -> Node (m, f x, map_s f s)

let rec mapi_s f = function
  | SEmpty -> SEmpty
  | SNode (l, mr, xr, r) -> SNode (mapi_s f l, mr, f mr xr, mapi_s f r)
let mapi f = function
  | Empty -> Empty
  | Node (m, x, s) -> Node (m, f m x, mapi_s f s)

let rec fold_s f s accu =
  match s with
  | SEmpty -> accu
  | SNode (l, mr, xr, r) -> fold_s f r (f mr xr (fold_s f l accu))
let fold f map accu =
  match map with
  | Empty -> accu
  | Node (m, x, s) -> fold_s f s (f m x accu)

let rec part_aux p m x = function
  | SEmpty -> if p m then Node (m, x, SEmpty), Empty else Empty, Node (m, x, SEmpty)
  | SNode (l, mr, xr, r) ->
      let lt, lf = part_aux p m x l in
      let rt, rf = part_aux p mr xr r in
      join lt rt, join lf rf
let part p = function
  | Empty -> (Empty, Empty)
  | Node (m, x, s) -> part_aux p m x s

let rec cardinal_s = function
  | SEmpty -> 0
  | SNode (l, _, _, r) -> cardinal_s l + 1 + cardinal_s r
let cardinal = function
  | Empty -> 0
  | Node (_, _, s) -> cardinal_s s + 1

let rec elements_s accu = function
  | SEmpty -> accu
  | SNode (l, mr, xr, r) -> elements_s ((mr, xr) :: elements_s accu r) l
let elements = function
  | Empty -> []
  | Node (m, x, s) -> (m, x) :: elements_s [] s

let rec choose = function
  | Empty -> raise Not_found
  | Node (m, x, s) -> (m, x)

(* ordering Empty < Node *)
let rec compare_s cmp s1 s2 = match s1, s2 with
  | SEmpty, SEmpty -> 0
  | SEmpty, _ -> - 1
  | _, SEmpty -> 1
  | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
      if mr1 <> mr2 then mr1 - mr2
      else let c = cmp xr1 xr2 in
        if c <> 0 then c
        else let c = compare_s cmp l1 l2 in
          if c <> 0 then c else compare_s cmp r1 r2
let compare cmp map1 map2 = match map1, map2 with
  | Empty, Empty -> 0
  | Empty, _ -> - 1
  | _, Empty -> 1
  | Node (m1, x1, s1), Node (m2, x2, s2) ->
      if m1 <> m2 then m1 - m2
      else let c = cmp x1 x2 in
        if c <> 0 then c
        else compare_s cmp s1 s2

let rec equal_s eq s1 s2 = match s1, s2 with
  | SEmpty, SEmpty -> true
  | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
      mr1 == mr2 && eq xr1 xr2 && equal_s eq l1 l2 && equal_s eq r1 r2
  | _ -> false
let equal eq map1 map2 = match map1, map2 with
  | Empty, Empty -> true
  | Node (m1, x1, s1), Node (m2, x2, s2) ->
      m1 == m2 && eq x1 x2 && equal_s eq s1 s2
  | _ -> false

let rec equal_right_s eq rt s1 s2 s3 = match s1, s2, s3 with
  (* assertion: compare_s s1 s2 >= 0; compare_s s1 s3 <=0 returns 0 if     *)
  (* equal_s s2 s3 = 0, > 0 if intright s1 s2 s3, else < 0                    *)
  | _, _, SEmpty -> 0 (* since s1 = SEmpty and s2 = SEmpty *)
  | SEmpty, _, _ -> - 1 (* since s2 = SEmpty and s3 <> SEmpty *)
  | _, SEmpty, _ -> 1 (* since s1 <> SEmpty and s3 <> SEmpty *)
  | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2), SNode (l3, mr3, xr3, r3) ->
      if mr2 == mr3
      then if eq xr2 xr3 then
          let er = equal_right_s eq rt l1 l2 l3 in
          if er <> 0 then er
          else equal_right_s eq rt r1 r2 r3
        else if rt xr1 xr2 xr3 then 1 else - 1
      else if intright mr1 mr2 mr3 then 1 else - 1
let right eq rt map1 map2 map3 = match map1, map2, map3 with
  | _, _, Empty -> true (* when map1 = map2 = map3 = Empty *)
  | _, Empty, _ -> false (* since map1 = Empty, but map3 <> Empty *)
  | Empty, _, _ -> true (* since map2 <> Empty and map3 <> Empty *)
  | Node (m1, x1, s1), Node (m2, x2, s2), Node (m3, x3, s3) ->
      if m2 == m3 then
        equal_right_s eq rt s1 s2 s3 > 0
      else intright m1 m2 m3

module Set = Intset

let rec iter_s_aux f m1 x1 s1 m2 s2 = match s1, s2 with
  (* invariant m2 >= m1 *)
  | SEmpty, _ -> if m1 == m2 then f m1 x1 else ()
  | _, Set.SEmpty -> if m1 == m2 then f m1 x1
      else (try let y1 = find_s m2 m1 s1 in f m2 y1
      with Not_found -> ())
  | SNode (l1, mr1, xr1, r1), Set.SNode (l2, mr2, r2) ->
      if mr2 > mr1 then
        if intright m2 m1 mr2 then
          (*| if intright mr1 m1 mr2 then*)
          if m2 > mr1 then iter_s_aux f mr1 xr1 r1 m2 s2
          else iter_si_aux f m2 s2 mr1 xr1 r1
        (*| else ()*)
        else if intright mr1 m1 mr2 then
          (iter_s_aux f m1 x1 l1 m2 l2; iter_s_aux f mr1 xr1 r1 mr2 r2)
        else iter_s_aux f m1 x1 s1 m2 l2
      else (* if mr2 <= mr1 *)
      if intright m2 m1 mr1 then iter_si_aux f m2 s2 mr1 xr1 r1
      else if intright mr2 m1 mr1 then
        (iter_s_aux f m1 x1 l1 m2 l2; iter_si_aux f mr2 r2 mr1 xr1 r1)
      else iter_s_aux f m1 x1 l1 m2 s2
and iter_si_aux f m1 s1 m2 x2 s2 = match s1, s2 with
  (* invariant m2 >= m1 *)
  | Set.SEmpty, _ -> if m1 == m2 then f m1 x2 else ()
  | _, SEmpty -> if m1 == m2 then f m1 x2
      else if Set.mem_s m2 m1 s1 then f m2 x2 else ()
  | Set.SNode (l1, mr1, r1), SNode (l2, mr2, xr2, r2) ->
      if mr2 > mr1 then
        if intright m2 m1 mr2 then
          (*| if intright mr1 m1 mr2 then*)
          if m2 > mr1 then iter_si_aux f mr1 r1 m2 x2 s2
          else iter_s_aux f m2 x2 s2 mr1 r1
        (*| else ()*)
        else if intright mr1 m1 mr2 then
          (iter_si_aux f m1 l1 m2 x2 l2; iter_si_aux f mr1 r1 mr2 xr2 r2)
        else iter_si_aux f m1 s1 m2 x2 l2
      else (* if mr2 <= mr1 *)
      if intright m2 m1 mr1 then iter_s_aux f m2 x2 s2 mr1 r1
      else if intright mr2 m1 mr1 then
        (iter_si_aux f m1 l1 m2 x2 l2; iter_s_aux f mr2 xr2 r2 mr1 r1)
      else iter_si_aux f m1 l1 m2 x2 s2
let iter_s f map set = match map, set with
  | Empty, _ -> ()
  | _, Set.Empty -> ()
  | Node (m1, x1, s1), Set.Node (m2, s2) ->
      if m2 > m1 then iter_s_aux f m1 x1 s1 m2 s2
      else iter_si_aux f m2 s2 m1 x1 s1
