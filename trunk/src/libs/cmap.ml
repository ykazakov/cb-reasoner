(* maps on conced values based on a variation of Patricia trees *)

open Consed

module type Type = sig
  type t
end

module type S = sig
  type key
  type 'a t
  val empty: 'a t
  val is_empty: 'a t -> bool
  val singleton : key -> 'a -> 'a t
  val is_singleton : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val process : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val replace : key -> 'a -> 'a t -> 'a t
  val find: key -> 'a t -> 'a
  val find_pred: key -> 'a t -> key * 'a
  val remove: key -> 'a t -> 'a t
  val mem: key -> 'a t -> bool
  (* iterating in the increasing order of indexes *)
  val iter_incr : (key -> 'a -> unit) -> 'a t -> unit
  (* iterating in the decreasing order of indexes *)
  val iter_decr : (key -> 'a -> unit) -> 'a t -> unit
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val iter2 : (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val diff : 'a t -> 'b t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val find_max : 'a t -> key * 'a
  val find_min : 'a t -> key * 'a
  val choose : 'a t -> key * 'a
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val right: ('a -> 'a -> bool) -> ('a -> 'a -> 'a -> bool) -> 'a t -> 'a t -> 'a t -> bool
  module Set : Cset.S with type elt = key
  val iter_s : (key -> 'a -> unit) -> 'a t -> Set.t -> unit
end

module Make (T : Type) = struct
  
  type key = T.t consed
  let intright x y z = x lxor y > x lxor z
  
  type 'a s =
    | SEmpty
    | SNode of 'a s * key * 'a * 'a s
  
  type 'a t =
    | Empty
    | Node of key * 'a * 'a s
  
  let empty = Empty
  
  let is_empty = function Empty -> true | _ -> false
  
  let singleton key x = Node (key, x, SEmpty)
  
  let is_singleton = function Node (_, _, SEmpty) -> true | _ -> false
  
  let rec mem_s kt mt = function
    (* invariant: kt > mt *)
    | SEmpty -> false
    | SNode (l, mr, xr, r) ->
        if kt > mr.tag then mem_s kt mr.tag r
        else kt == mr.tag || mem_s kt mt l
  let mem k = function
    | Empty -> false
    | Node (m, x, s) ->
        if k.tag > m.tag then mem_s k.tag m.tag s else k == m
  
  let rec find_s kt mt = function
    (* invariant: kt > mt *)
    | SEmpty -> raise Not_found
    | SNode (l, mr, xr, r) ->
        if kt > mr.tag then find_s kt mr.tag r
        else if kt == mr.tag then xr
        else find_s kt mt l
  let find k = function
    | Empty -> raise Not_found
    | Node (m, x, s) ->
        if k.tag > m.tag then find_s k.tag m.tag s
        else if k == m then x
        else raise Not_found
  
  let rec find_pred_s kt m x = function
    (* invariant: kt > m.tag *)
    | SEmpty -> m, x
    | SNode (l, mr, xr, r) ->
        if kt > mr.tag then find_pred_s kt mr xr r
        else find_pred_s kt m x l
  let find_pred k = function
    | Empty -> raise Not_found
    | Node (m, x, s) ->
        if k.tag > m.tag then find_pred_s k.tag m x s
        else raise Not_found
  
  let rec replace_s kt y mt = function
    (* invariant: kt > mt *)
    | SEmpty -> raise Not_found
    | SNode (l, mr, xr, r) ->
        if kt > mr.tag then SNode (l, mr, xr, replace_s kt y mr.tag r)
        else if kt == mr.tag then SNode (l, mr, y, r)
        else SNode (replace_s kt y mt l, mr, xr, r)
  let replace k y = function
    | Empty -> raise Not_found
    | Node (m, x, s) ->
        if k.tag > m.tag then Node (m, x, replace_s k.tag y m.tag s)
        else if k == m then Node (m, y, s)
        else raise Not_found
  
  let rec iter_incr_s f = function
    | SEmpty -> ()
    | SNode (l, mr, xr, r) -> iter_incr_s f l; f mr xr; iter_incr_s f r
  let iter_incr f = function
    | Empty -> ()
    | Node (m, x, s) -> f m x; iter_incr_s f s
  
  let rec iter_decr_s f = function
    | SEmpty -> ()
    | SNode (l, mr, xr, r) -> iter_decr_s f r; f mr xr; iter_decr_s f l
  let iter_decr f = function
    | Empty -> ()
    | Node (m, x, s) -> iter_decr_s f s; f m x
  
  let iter_s = iter_incr_s
  let iter = iter_incr
  
  let rec iter2_aux f m1t x1 s1 m2 x2 s2 = match s1, s2 with
    (* invariant m2.tag >= m1t *)
    | SEmpty, _ -> if m1t == m2.tag then f m2 x1 x2 else ()
    | _, SEmpty -> if m1t == m2.tag then f m2 x1 x2
        else (try let y1 = find_s m2.tag m1t s1 in f m2 y1 x2
        with Not_found -> ())
    | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
        if mr2.tag > mr1.tag then
          if m2.tag lxor m1t > m2.tag lxor mr2.tag then
            (*| if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then*)
            if m2.tag > mr1.tag then iter2_aux f mr1.tag xr1 r1 m2 x2 s2
            else iter2i_aux f m2.tag x2 s2 mr1 xr1 r1
          (*| else ()*)
          else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
            (iter2_aux f m1t x1 l1 m2 x2 l2; iter2_aux f mr1.tag xr1 r1 mr2 xr2 r2)
          else iter2_aux f m1t x1 s1 m2 x2 l2
        else (* if mr2.tag <= mr1.tag *)
        if m2.tag lxor m1t > m2.tag lxor mr1.tag then iter2i_aux f m2.tag x2 s2 mr1 xr1 r1
        else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
          (iter2_aux f m1t x1 l1 m2 x2 l2; iter2i_aux f mr2.tag xr2 r2 mr1 xr1 r1)
        else iter2_aux f m1t x1 l1 m2 x2 s2
  and iter2i_aux f m1t x1 s1 m2 x2 s2 = match s1, s2 with
    (* invariant m2.tag >= m1t *)
    | SEmpty, _ -> if m1t == m2.tag then f m2 x2 x1 else ()
    | _, SEmpty -> if m1t == m2.tag then f m2 x2 x1
        else (try let y1 = find_s m2.tag m1t s1 in f m2 x2 y1
        with Not_found -> ())
    | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
        if mr2.tag > mr1.tag then
          if m2.tag lxor m1t > m2.tag lxor mr2.tag then
            (*| if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then*)
            if m2.tag > mr1.tag then iter2i_aux f mr1.tag xr1 r1 m2 x2 s2
            else iter2_aux f m2.tag x2 s2 mr1 xr1 r1
          (*| else ()*)
          else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
            (iter2i_aux f m1t x1 l1 m2 x2 l2; iter2i_aux f mr1.tag xr1 r1 mr2 xr2 r2)
          else iter2i_aux f m1t x1 s1 m2 x2 l2
        else (* if mr2.tag <= mr1.tag *)
        if m2.tag lxor m1t > m2.tag lxor mr1.tag then iter2_aux f m2.tag x2 s2 mr1 xr1 r1
        else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
          (iter2i_aux f m1t x1 l1 m2 x2 l2; iter2_aux f mr2.tag xr2 r2 mr1 xr1 r1)
        else iter2i_aux f m1t x1 l1 m2 x2 s2
  let iter2 f map1 map2 = match map1, map2 with
    | Empty, _ -> ()
    | _, Empty -> ()
    | Node (m1, x1, s1), Node (m2, x2, s2) ->
        if m2.tag > m1.tag then iter2_aux f m1.tag x1 s1 m2 x2 s2
        else iter2i_aux f m2.tag x2 s2 m1 x1 s1
  
  let rec add_s_min k y mt = function
    | SEmpty -> SNode (SEmpty, k, y, SEmpty)
    | SNode (l, mr, xr, r) as s ->
        if k.tag lxor mt > k.tag lxor mr.tag then SNode (SEmpty, k, y, s)
        else SNode (add_s_min k y mt l, mr, xr, r)
  let rec add_s k y mt = function
    (* invariant: k.tag > mt *)
    | SEmpty -> SNode (SEmpty, k, y, SEmpty)
    | SNode (l, mr, xr, r) as s ->
        if k.tag > mr.tag then
          if mr.tag lxor mt > mr.tag lxor k.tag then SNode (l, mr, xr, add_s k y mr.tag r)
          else SNode (s, k, y, SEmpty)
        else if k == mr then SNode (l, mr, y, r)
        else (* if k.tag < mr.tag *)
        if k.tag lxor mt > k.tag lxor mr.tag then SNode (l, k, y, add_s_min mr xr k.tag r)
        else SNode (add_s k y mt l, mr, xr, r)
  let add k y = function
    | Empty -> Node (k, y, SEmpty)
    | Node (m, x, s) ->
        if k.tag > m.tag then Node (m, x, add_s k y m.tag s)
        else if k == m then Node (m, y, s)
        else (* if k.tag < m.tag *)
        Node (k, y, add_s_min m x k.tag s)
  
  let rec add_s_f k fresh replace mt = function
    (* invariant: k.tag > mt *)
    | SEmpty -> SNode (SEmpty, k, fresh (), SEmpty)
    | SNode (l, mr, xr, r) as s ->
        if k.tag > mr.tag then
          if mr.tag lxor mt > mr.tag lxor k.tag then
            SNode (l, mr, xr, add_s_f k fresh replace mr.tag r)
          else SNode (s, k, fresh (), SEmpty)
        else if k == mr then SNode (l, mr, replace xr, r)
        else (* if k.tag < mr.tag *)
        if k.tag lxor mt > k.tag lxor mr.tag then
          SNode (l, k, fresh (), add_s_min mr xr k.tag r)
        else SNode (add_s_f k fresh replace mt l, mr, xr, r)
  let add_f k fresh replace = function
    | Empty -> Node (k, fresh (), SEmpty)
    | Node (m, x, s) ->
        if k.tag > m.tag then Node (m, x, add_s_f k fresh replace m.tag s)
        else if k == m then Node (m, replace x, s)
        else (* if k.tag < m.tag *)
        Node (k, fresh (), add_s_min m x k.tag s)
  
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
  
  let rec remove_s kt mt = function
    (* invariant: kt > mt *)
    | SEmpty -> raise Unchanged
    | SNode (l, mr, xr, r) ->
        if kt > mr.tag then SNode (l, mr, xr, remove_s kt mr.tag r)
        else if kt < mr.tag then SNode (remove_s kt mt l, mr, xr, r)
        else (* if kt == mr.tag *)
        if r == SEmpty then l
        else let mrr, xrr, rr = pop_min r in SNode (l, mrr, xrr, rr)
  let remove k = function
    | Empty -> Empty
    | Node (m, x, s) as map ->
        if k.tag > m.tag then
          try Node (m, x, remove_s k.tag m.tag s)
          with Unchanged -> map
        else if k == m then map_of_s s
        else map
  
  let rec iter_remove_s f kt mt = function
    (* invariant: kt > mt *)
    | SEmpty -> ()
    | SNode (l, mr, xr, r) ->
        if kt > mr.tag then (iter_s f l; f mr xr; iter_remove_s f kt mr.tag r)
        else if kt < mr.tag then (iter_remove_s f kt mt l; f mr xr; iter_s f r)
        else (* if kt == mr.tag *)
        (iter_s f l; iter_s f r)
  let iter_remove f k = function
    | Empty -> ()
    | Node (m, x, s) ->
        if k.tag > m.tag then (f m x; iter_remove_s f k.tag m.tag s)
        else if k == m then iter_s f s
        else (f m x; iter_s f s)
  
  let rec add_s_f k fresh replace m = function
    (* invariant: k.tag > m *)
    | SEmpty -> SNode (SEmpty, k, fresh (), SEmpty)
    | SNode (l, mr, xr, r) as s ->
        if k.tag > mr.tag then
          if mr.tag lxor m > mr.tag lxor k.tag then
            SNode (l, mr, xr, add_s_f k fresh replace mr.tag r)
          else SNode (s, k, fresh (), SEmpty)
        else if k == mr then SNode (l, mr, replace xr, r)
        else (* if k.tag < mr.tag then *)
        if k.tag lxor m > k.tag lxor mr.tag then
          SNode (l, k, fresh (), add_s_min mr xr k.tag r)
        else SNode (add_s_f k fresh replace m l, mr, xr, r)
  let add_f k fresh replace = function
    | Empty -> Node (k, fresh (), SEmpty)
    | Node (m, x, s) ->
        if k.tag > m.tag then Node (m, x, add_s_f k fresh replace m.tag s)
        else if k == m then Node (m, replace x, s)
        else (* if k.tag < m.tag *)
        Node (k, fresh (), add_s_min m x k.tag s)
  
  let rec process_s k f mt = function
    (* invariant: k.tag > mt *)
    | SEmpty -> begin match f None with
          | None -> raise Unchanged
          | Some y -> SNode (SEmpty, k, y, SEmpty)
        end
    | SNode (l, mr, xr, r) as s ->
        if k.tag > mr.tag then
          if mr.tag lxor mt > mr.tag lxor k.tag then SNode (l, mr, xr, process_s k f mr.tag r)
          else (* potentially a new entrance *)
          match f None with
          | None -> raise Unchanged
          | Some y -> SNode (s, k, y, SEmpty)
        else if k == mr then
          match f (Some xr) with
          | None -> (* removing the binding *)
              if r == SEmpty then l
              else let mrr, xrr, rr = pop_min r in
                SNode (l, mrr, xrr, rr)
          | Some y -> SNode (l, mr, y, r)
        else (* if k.tag < mr.tag then *)
        if k.tag lxor mt > k.tag lxor mr.tag then (* potentially a new entrance *)
        match f None with
        | None -> raise Unchanged
        | Some y -> SNode (l, k, y, add_s_min mr xr k.tag r)
        else SNode (process_s k f mt l, mr, xr, r)
  
  let process k f = function
    | Empty -> begin match f None with
          | None -> Empty
          | Some y -> singleton k y
        end
    | Node (m, x, s) as map ->
        if k.tag > m.tag then
          try Node (m, x, process_s k f m.tag s)
          with Unchanged -> map
        else if k == m then
          match f (Some x) with
          | None -> (* removing the binding *)
              map_of_s s
          | Some y -> Node (m, y, s)
        else (* if k.tag < m.tag *) (* potentially a new entrance *)
        match f None with
        | None -> map
        | Some y -> Node (k, y, add_s_min m x k.tag s)
  
  let join map1 map2 = match map1, map2 with
    | Empty, _ -> map2
    | _, Empty -> map1
    | Node (m1, x1, s1), Node (m2, x2, s2) -> Node (m1, x1, SNode (s1, m2, x2, s2))
  
  let rec inter_aux f m1t x1 s1 m2 x2 s2 = match s1, s2 with
    (* invariant m2.tag >= m1t *)
    | SEmpty, _ -> if m1t == m2.tag then Node (m2, f x1 x2, SEmpty) else Empty
    | _, SEmpty -> if m1t == m2.tag then Node (m2, f x1 x2, SEmpty) else
          (try let y1 = find_s m2.tag m1t s1 in Node (m2, f y1 x2, SEmpty)
          with Not_found -> Empty)
    | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
        if mr2.tag > mr1.tag then
          if m2.tag lxor m1t > m2.tag lxor mr2.tag then
            (*| if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then*)
            if m2.tag > mr1.tag then inter_aux f mr1.tag xr1 r1 m2 x2 s2
            else interi_aux f m2.tag x2 s2 mr1 xr1 r1
          (*| else Empty*)
          else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
            join (inter_aux f m1t x1 l1 m2 x2 l2) (inter_aux f mr1.tag xr1 r1 mr2 xr2 r2)
          else inter_aux f m1t x1 s1 m2 x2 l2
        else (* if mr2.tag <= mr1.tag *)
        if m2.tag lxor m1t > m2.tag lxor mr1.tag then interi_aux f m2.tag x2 s2 mr1 xr1 r1
        else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
          join (inter_aux f m1t x1 l1 m2 x2 l2) (interi_aux f mr2.tag xr2 r2 mr1 xr1 r1)
        else inter_aux f m1t x1 l1 m2 x2 s2
  and interi_aux f m1t x1 s1 m2 x2 s2 = match s1, s2 with
    (* invariant m2.tag >= m1t *)
    | SEmpty, _ -> if m1t == m2.tag then Node (m2, f x2 x1, SEmpty) else Empty
    | _, SEmpty -> if m1t == m2.tag then Node (m2, f x2 x1, SEmpty) else
          (try let y1 = find_s m2.tag m1t s1 in Node (m2, f x2 y1, SEmpty)
          with Not_found -> Empty)
    | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
        if mr2.tag > mr1.tag then
          if m2.tag lxor m1t > m2.tag lxor mr2.tag then
            (*| if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then*)
            if m2.tag > mr1.tag then interi_aux f mr1.tag xr1 r1 m2 x2 s2
            else inter_aux f m2.tag x2 s2 mr1 xr1 r1
          (*| else Empty*)
          else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
            join (interi_aux f m1t x1 l1 m2 x2 l2) (interi_aux f mr1.tag xr1 r1 mr2 xr2 r2)
          else interi_aux f m1t x1 s1 m2 x2 l2
        else (* if mr2.tag <= mr1.tag *)
        if m2.tag lxor m1t > m2.tag lxor mr1.tag then inter_aux f m2.tag x2 s2 mr1 xr1 r1
        else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
          join (interi_aux f m1t x1 l1 m2 x2 l2) (inter_aux f mr2.tag xr2 r2 mr1 xr1 r1)
        else interi_aux f m1t x1 l1 m2 x2 s2
  let inter f map1 map2 = match map1, map2 with
    | Empty, _ -> Empty
    | _, Empty -> Empty
    | Node (m1, x1, s1), Node (m2, x2, s2) ->
        if m2.tag > m1.tag then inter_aux f m1.tag x1 s1 m2 x2 s2
        else interi_aux f m2.tag x2 s2 m1 x1 s1
  
  let rec union_s f m1t x1 s1 m2 x2 s2 = match s1, s2 with
    (* invariant: m2.tag >= m1t; computes the union of s1, m2, s2 minus    *)
    (* m1t; f is a function resolving clashes                              *)
    | SEmpty, _ -> if m1t == m2.tag then s2 else add_s_min m2 x2 m1t s2
    | _, SEmpty -> if m1t == m2.tag then s1 else
          add_s_f m2 (fun () -> x2) (fun x1 -> f x1 x2) m1t s1
    | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
        if mr2.tag > mr1.tag then
          if m2.tag lxor m1t > m2.tag lxor mr2.tag then
            if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
              if m2.tag > mr1.tag then
                SNode (l1, mr1, xr1, union_s f mr1.tag xr1 r1 m2 x2 s2)
              else (* if m2.tag <= mr1.tag *)
              let xr = if m2 == mr1 then f xr1 x2 else x2 in
              SNode (l1, m2, xr, unioni_s f m2.tag x2 s2 mr1 xr1 r1)
            else SNode (s1, m2, x2, s2)
          else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
            SNode (union_s f m1t x1 l1 m2 x2 l2, mr1, xr1,
              union_s f mr1.tag xr1 r1 mr2 xr2 r2)
          else SNode (union_s f m1t x1 s1 m2 x2 l2, mr2, xr2, r2)
        else (* if mr2.tag <= mr1.tag *)
        if m2.tag lxor m1t > m2.tag lxor mr1.tag then
          SNode (l1, m2, x2, unioni_s f m2.tag x2 s2 mr1 xr1 r1)
        else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
          let xr = if mr2 == mr1 then f xr1 xr2 else xr2 in
          SNode (union_s f m1t x1 l1 m2 x2 l2, mr2, xr,
            unioni_s f mr2.tag xr2 r2 mr1 xr1 r1)
        else SNode (union_s f m1t x1 l1 m2 x2 s2, mr1, xr1, r1)
  and unioni_s f m1t x1 s1 m2 x2 s2 = match s1, s2 with
    (* invariant: m2.tag >= m1t; computes the union of s1, m2, s2 minus    *)
    (* m1t; f is a function resolving clashes                              *)
    | SEmpty, _ -> if m1t == m2.tag then s2 else add_s_min m2 x2 m1t s2
    | _, SEmpty -> if m1t == m2.tag then s1 else
          add_s_f m2 (fun () -> x2) (fun x1 -> f x2 x1) m1t s1
    | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
        if mr2.tag > mr1.tag then
          if m2.tag lxor m1t > m2.tag lxor mr2.tag then
            if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
              if m2.tag > mr1.tag then
                SNode (l1, mr1, xr1, unioni_s f mr1.tag xr1 r1 m2 x2 s2)
              else (* if m2.tag <= mr1.tag *)
              let xr = if m2 == mr1 then f x2 xr1 else x2 in
              SNode (l1, m2, xr, union_s f m2.tag x2 s2 mr1 xr1 r1)
            else SNode (s1, m2, x2, s2)
          else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
            SNode (unioni_s f m1t x1 l1 m2 x2 l2, mr1, xr1,
              unioni_s f mr1.tag xr1 r1 mr2 xr2 r2)
          else SNode (unioni_s f m1t x1 s1 m2 x2 l2, mr2, xr2, r2)
        else (* if mr2.tag <= mr1.tag *)
        if m2.tag lxor m1t > m2.tag lxor mr1.tag then
          SNode (l1, m2, x2, union_s f m2.tag x2 s2 mr1 xr1 r1)
        else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
          let xr = if mr2 == mr1 then f xr2 xr1 else xr2 in
          SNode (unioni_s f m1t x1 l1 m2 x2 l2, mr2, xr,
            union_s f mr2.tag xr2 r2 mr1 xr1 r1)
        else SNode (unioni_s f m1t x1 l1 m2 x2 s2, mr1, xr1, r1)
  let union f map1 map2 = match map1, map2 with
    | Empty, _ -> map2
    | _, Empty -> map1
    | Node (m1, x1, s1), Node (m2, x2, s2) ->
        if m2.tag > m1.tag then Node (m1, x1, union_s f m1.tag x1 s1 m2 x2 s2)
        else if m2 == m1 then Node (m1, f x1 x2, union_s f m1.tag x1 s1 m2 x2 s2)
        else (* if m2.tag < m1.tag *) Node (m2, x2, unioni_s f m2.tag x2 s2 m1 x1 s1)
  
  (** diff is not a symmetric operation therefore we need two auxiliary funcitons *)
  let rec diff_aux m1 x1 s1 m2t s2 = match s1, s2 with
    (* invariant: m2t >= m1.tag; computes union of m1 and s1 minus the     *)
    (* union of m2 and s2                                                  *)
    | SEmpty, _ -> if m1.tag == m2t then Empty else Node (m1, x1, SEmpty)
    | _, SEmpty -> if m1.tag == m2t then map_of_s s1
        else (try Node (m1, x1, remove_s m2t m1.tag s1)
        with Unchanged -> Node (m1, x1, s1))
    | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, _, r2) ->
        if mr2.tag > mr1.tag then
          if m2t lxor m1.tag > m2t lxor mr2.tag then
            if mr1.tag lxor m1.tag > mr1.tag lxor mr2.tag then
              if m2t > mr1.tag then join (Node (m1, x1, l1)) (diff_aux mr1 xr1 r1 m2t s2)
              else join (Node (m1, x1, l1)) (diffi_aux m2t s2 mr1 xr1 r1)
            else Node (m1, x1, s1)
          else if mr1.tag lxor m1.tag > mr1.tag lxor mr2.tag then
            join (diff_aux m1 x1 l1 m2t l2) (diff_aux mr1 xr1 r1 mr2.tag r2)
          else diff_aux m1 x1 s1 m2t l2
        else (* if mr2.tag < mr1.tag *)
        if m2t lxor m1.tag > m2t lxor mr1.tag then
          join (Node (m1, x1, l1)) (diffi_aux m2t s2 mr1 xr1 r1)
        else if mr2.tag lxor m1.tag > mr2.tag lxor mr1.tag then
          join (diff_aux m1 x1 l1 m2t l2) (diffi_aux mr2.tag r2 mr1 xr1 r1)
        else join (diff_aux m1 x1 l1 m2t s2) (Node (mr1, xr1, r1))
  and diffi_aux m1t s1 m2 x2 s2 = match s1, s2 with
    (* invariant: m2.tag >= m1t; computes union of m2 and s2 minus the     *)
    (* union of m1 and s1                                                  *)
    | SEmpty, _ -> if m1t == m2.tag then map_of_s s2 else Node (m2, x2, s2)
    | _, SEmpty -> if m1t == m2.tag || mem_s m2.tag m1t s1 then Empty
        else Node (m2, x2, SEmpty)
    | SNode (l1, mr1, _, r1), SNode (l2, mr2, xr2, r2) ->
        if mr2.tag > mr1.tag then
          if m2.tag lxor m1t > m2.tag lxor mr2.tag then
            if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
              if m2.tag > mr1.tag then diffi_aux mr1.tag r1 m2 x2 s2
              else diff_aux m2 x2 s2 mr1.tag r1
            else Node (m2, x2, s2)
          else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
            join (diffi_aux m1t l1 m2 x2 l2) (diffi_aux mr1.tag r1 mr2 xr2 r2)
          else join (diffi_aux m1t s1 m2 x2 l2) (Node (mr2, xr2, r2))
        else (* if mr2.tag < mr1.tag *)
        if m2.tag lxor m1t > m2.tag lxor mr1.tag then diff_aux m2 x2 s2 mr1.tag r1
        else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
          join (diffi_aux m1t l1 m2 x2 l2) (diff_aux mr2 xr2 r2 mr1.tag r1)
        else diffi_aux m1t l1 m2 x2 s2
  let diff map1 map2 = match map1, map2 with
    | Empty, _ -> Empty
    | _, Empty -> map1
    | Node (m1, x1, s1), Node (m2, _, s2) ->
        if m2.tag > m1.tag then diff_aux m1 x1 s1 m2.tag s2
        else diffi_aux m2.tag s2 m1 x1 s1
  
  (** iter_diff is not a symmetric operation therefore we need two auxiliary funcitons *)
  let rec iter_diff_aux f m1 x1 s1 m2t s2 = match s1, s2 with
    (* invariant: m2t >= m1.tag; computes union of m1 and s1 minus the     *)
    (* union of m2 and s2                                                  *)
    | SEmpty, _ -> if m1.tag == m2t then () else f m1 x1
    | _, SEmpty -> if m1.tag == m2t then iter_s f s1
        else (f m1 x1; iter_remove_s f m2t m1.tag s1)
    | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, _, r2) ->
        if mr2.tag > mr1.tag then
          if m2t lxor m1.tag > m2t lxor mr2.tag then
            if mr1.tag lxor m1.tag > mr1.tag lxor mr2.tag then
              if m2t > mr1.tag then (f m1 x1; iter_s f l1; iter_diff_aux f mr1 xr1 r1 m2t s2)
              else (f m1 x1; iter_s f l1; iter_diffi_aux f m2t s2 mr1 xr1 r1)
            else (f m1 x1; iter_s f s1)
          else if mr1.tag lxor m1.tag > mr1.tag lxor mr2.tag then
            (iter_diff_aux f m1 x1 l1 m2t l2; iter_diff_aux f mr1 xr1 r1 mr2.tag r2)
          else iter_diff_aux f m1 x1 s1 m2t l2
        else (* if mr2.tag < mr1.tag *)
        if m2t lxor m1.tag > m2t lxor mr1.tag then
          (f m1 x1; iter_s f l1; iter_diffi_aux f m2t s2 mr1 xr1 r1)
        else if mr2.tag lxor m1.tag > mr2.tag lxor mr1.tag then
          (iter_diff_aux f m1 x1 l1 m2t l2; iter_diffi_aux f mr2.tag r2 mr1 xr1 r1)
        else (iter_diff_aux f m1 x1 l1 m2t s2; f mr1 xr1; iter_s f r1)
  and iter_diffi_aux f m1t s1 m2 x2 s2 = match s1, s2 with
    (* invariant: m2.tag >= m1t; computes union of m2 and s2 minus the     *)
    (* union of m1 and s1                                                  *)
    | SEmpty, _ -> if m1t == m2.tag then iter_s f s2 else (f m2 x2; iter_s f s2)
    | _, SEmpty -> if not (m1t == m2.tag || mem_s m2.tag m1t s1) then f m2 x2
    | SNode (l1, mr1, _, r1), SNode (l2, mr2, xr2, r2) ->
        if mr2.tag > mr1.tag then
          if m2.tag lxor m1t > m2.tag lxor mr2.tag then
            if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
              if m2.tag > mr1.tag then iter_diffi_aux f mr1.tag r1 m2 x2 s2
              else iter_diff_aux f m2 x2 s2 mr1.tag r1
            else (f m2 x2; iter_s f s2)
          else if mr1.tag lxor m1t > mr1.tag lxor mr2.tag then
            (iter_diffi_aux f m1t l1 m2 x2 l2; iter_diffi_aux f mr1.tag r1 mr2 xr2 r2)
          else (iter_diffi_aux f m1t s1 m2 x2 l2; f mr2 xr2; iter_s f r2)
        else (* if mr2.tag < mr1.tag *)
        if m2.tag lxor m1t > m2.tag lxor mr1.tag then iter_diff_aux f m2 x2 s2 mr1.tag r1
        else if mr2.tag lxor m1t > mr2.tag lxor mr1.tag then
          (iter_diffi_aux f m1t l1 m2 x2 l2; iter_diff_aux f mr2 xr2 r2 mr1.tag r1)
        else iter_diffi_aux f m1t l1 m2 x2 s2
  let iter_diff f map1 map2 = match map1, map2 with
    | Empty, _ -> ()
    | _, Empty -> iter f map1
    | Node (m1, x1, s1), Node (m2, _, s2) ->
        if m2.tag > m1.tag then iter_diff_aux f m1 x1 s1 m2.tag s2
        else iter_diffi_aux f m2.tag s2 m1 x1 s1
  
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
  
  let find_min = function
    | Empty -> raise Not_found
    | Node (m, x, s) -> (m, x)
  
  let rec find_max_s m x = function
    | SEmpty -> m, x
    | SNode (l, mr, xr, r) ->
        find_max_s mr xr r
  let find_max = function
    | Empty -> raise Not_found
    | Node (m, x, s) ->
        find_max_s m x s
  
  let choose = find_min
  
  (* ordering Empty < Node *)
  let rec compare_s cmp s1 s2 = match s1, s2 with
    | SEmpty, SEmpty -> 0
    | SEmpty, _ -> - 1
    | _, SEmpty -> 1
    | SNode (l1, mr1, xr1, r1), SNode (l2, mr2, xr2, r2) ->
        if mr1 <> mr2 then mr1.tag - mr2.tag
        else let c = cmp xr1 xr2 in
          if c <> 0 then c
          else let c = compare_s cmp l1 l2 in
            if c <> 0 then c else compare_s cmp r1 r2
  let compare cmp map1 map2 = match map1, map2 with
    | Empty, Empty -> 0
    | Empty, _ -> - 1
    | _, Empty -> 1
    | Node (m1, x1, s1), Node (m2, x2, s2) ->
        if m1 <> m2 then m1.tag - m2.tag
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
    (* assertion: compare_s s1 s2 >= 0; compare_s s1 s3 <=0 returns 0 if   *)
    (* equal_s s2 s3 = 0, > 0 if s1 lxor s2 > s1 lxor s3, else < 0         *)
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
        else if mr1.tag lxor mr2.tag > mr1.tag lxor mr3.tag then 1 else - 1
  let right eq rt map1 map2 map3 = match map1, map2, map3 with
    | _, _, Empty -> true (* when map1 = map2 = map3 = Empty *)
    | _, Empty, _ -> false (* since map1 = Empty, but map3 <> Empty *)
    | Empty, _, _ -> true (* since map2 <> Empty and map3 <> Empty *)
    | Node (m1, x1, s1), Node (m2, x2, s2), Node (m3, x3, s3) ->
        if m2 == m3 then
          equal_right_s eq rt s1 s2 s3 > 0
        else m1.tag lxor m2.tag > m1.tag lxor m3.tag
  
  module Set = Cset.Make(T)
    
  let rec iter_s_aux f m1t x1 s1 m2 m2t s2 = match s1, s2 with
    (* invariant m2t >= m1t *)
    | SEmpty, _ -> if m1t == m2t then f m2 x1 else ()
    | _, Cset.SEmpty -> if m1t == m2t then f m2 x1
        else (try let y1 = find_s m2t m1t s1 in f m2 y1
        with Not_found -> ())
    | SNode (l1, mr1, xr1, r1), Cset.SNode (l2, mr2, r2) ->
        let mr1t = mr1.tag in let mr2t = mr2.tag in
        if mr2t > mr1t then
          if m2t lxor m1t > m2t lxor mr2t then
            (*| if mr1t lxor m1t > mr1t lxor mr2t then*)
            if m2t > mr1t then iter_s_aux f mr1t xr1 r1 m2 m2t s2
            else iter_si_aux f m2t s2 mr1 mr1t xr1 r1
          (*| else ()*)
          else if mr1t lxor m1t > mr1t lxor mr2t then
            (iter_s_aux f m1t x1 l1 m2 m2t l2; iter_s_aux f mr1t xr1 r1 mr2 mr2t r2)
          else iter_s_aux f m1t x1 s1 m2 m2t l2
        else (* if mr2t <= mr1t *)
        if m2t lxor m1t > m2t lxor mr1t then iter_si_aux f m2t s2 mr1 mr1t xr1 r1
        else if mr2t lxor m1t > mr2t lxor mr1t then
          (iter_s_aux f m1t x1 l1 m2 m2t l2; iter_si_aux f mr2t r2 mr1 mr1t xr1 r1)
        else iter_s_aux f m1t x1 l1 m2 m2t s2
  and iter_si_aux f m1t s1 m2 m2t x2 s2 = match s1, s2 with
    (* invariant m2t >= m1t *)
    | Cset.SEmpty, _ -> if m1t == m2t then f m2 x2 else ()
    | _, SEmpty -> if m1t == m2t then f m2 x2
        else if Set.mem_s m2t m1t s1 then f m2 x2 else ()
    | Cset.SNode (l1, mr1, r1), SNode (l2, mr2, xr2, r2) ->
        let mr1t = mr1.tag in let mr2t = mr2.tag in
        if mr2t > mr1t then
          if m2t lxor m1t > m2t lxor mr2t then
            (*| if mr1t lxor m1t > mr1t lxor mr2t then*)
            if m2t > mr1t then iter_si_aux f mr1t r1 m2 m2t x2 s2
            else iter_s_aux f m2t x2 s2 mr1 mr1t r1
          (*| else ()*)
          else if mr1t lxor m1t > mr1t lxor mr2t then
            (iter_si_aux f m1t l1 m2 m2t x2 l2; iter_si_aux f mr1t r1 mr2 mr2t xr2 r2)
          else iter_si_aux f m1t s1 m2 m2t x2 l2
        else (* if mr2t <= mr1t *)
        if m2t lxor m1t > m2t lxor mr1t then iter_s_aux f m2t x2 s2 mr1 mr1t r1
        else if mr2t lxor m1t > mr2t lxor mr1t then
          (iter_si_aux f m1t l1 m2 m2t x2 l2; iter_s_aux f mr2t xr2 r2 mr1 mr1t r1)
        else iter_si_aux f m1t l1 m2 m2t x2 s2
  let iter_s f map set = match map, set with
    | Empty, _ -> ()
    | _, Cset.Empty -> ()
    | Node (m1, x1, s1), Cset.Node (m2, s2) ->
        let m1t = m1.tag in let m2t = m2.tag in
        if m2t > m1t then iter_s_aux f m1t x1 s1 m2 m2t s2
        else iter_si_aux f m2.tag s2 m1 m1t x1 s1
  
end