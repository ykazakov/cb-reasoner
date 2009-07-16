(* sets of consed elements based on a variation of Patricia trees *)

open Consed.T
type 'a elt = 'a consed
type 'a s =
  | SEmpty
  | SNode of 'a s * 'a * 'a s
type 'a t =
  | Empty
  | Node of 'a * 'a s
type 'a _elt = 'a elt
type 'a _s = 'a s
type 'a _t = 'a t

val empty: 'a t
val is_empty: 'a t -> bool
val mem: 'a elt -> 'a elt t -> bool
val mem_s: int -> int -> 'a elt s -> bool
val add: 'a elt -> 'a elt t -> 'a elt t
val singleton: 'a -> 'a t
val is_singleton : 'a t -> bool
val remove: 'a elt -> 'a elt t -> 'a elt t
val union: 'a elt t -> 'a elt t -> 'a elt t
val inter: 'a elt t -> 'a elt t -> 'a elt t
val divide : 'a t -> 'a t * 'a t
val diff: 'a elt t -> 'a elt t -> 'a elt t
val is_subset: 'a elt t -> 'a elt t -> bool
(* iteration in the increasing order of indexes *)
val iter_incr: ('a -> unit) -> 'a t -> unit
(* iteration in the decreasing order of indexes *)
val iter_decr: ('a -> unit) -> 'a t -> unit
val iter: ('a -> unit) -> 'a t -> unit
val iter2 : ('a elt -> unit) -> 'a elt t -> 'a elt t -> unit
val iter_diff : ('a elt -> unit) -> 'a elt t -> 'a elt t -> unit
val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val for_all: ('a -> bool) -> 'a t -> bool
val exists: ('a -> bool) -> 'a t -> bool
val filter: ('a -> bool) -> 'a t -> 'a t
val partition: ('a -> bool) -> 'a t -> 'a t * 'a t
val cardinal: 'a t -> int
val elements: 'a t -> 'a list
val choose: 'a t -> 'a
val equal: 'a elt t -> 'a elt t -> bool
val compare: 'a elt t -> 'a elt t -> int
val hash_param: int -> 'a elt t -> int
val hash: 'a elt t -> int
val right: 'a elt t -> 'a elt t -> 'a elt t -> bool

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
  val choose: t -> elt
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash_param: int -> t -> int
  val hash: t -> int
  val right: t -> t -> t -> bool
end

module Make (T : T) : S with type elt = T.t consed