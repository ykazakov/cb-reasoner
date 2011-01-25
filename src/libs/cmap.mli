(* maps on integers based on a variation of Patricia trees *)

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

module Make (T : Type) : S with type key = T.t consed