(* sets of integers based on a variation of Patricia trees *)

module type S = sig
  type elt = int
  type s = private
    | SEmpty
    | SNode of s * elt * s
  type t = private
    | Empty
    | Node of elt * s
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val mem_s: elt -> elt -> s -> bool
  val add: elt -> t -> t
  val add_f : elt -> (unit -> unit) -> (unit -> unit) -> t -> t
  val singleton: elt -> t
  val is_singleton : t -> bool
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val subset: t -> t -> bool
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
  val right: t -> t -> t -> bool
end

include S