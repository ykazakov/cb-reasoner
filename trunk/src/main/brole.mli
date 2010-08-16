(* module for roles represented as a pair [(a, ata)] where [a] is an       *)
(* atomic role and [ata] is the boolean encoding whether ther role is      *)
(* inverse: [(a,true)] represents role [a], [(a,false)] represents role    *)
(* [inverse a]                                                             *)

open Owl2

type t = ObjectProperty.t * bool

val to_elt : ObjectPropertyExpression.t -> t
val inv: t -> t
val str: t -> string

module Set : sig
  type elt = t
  type t = ObjectProperty.Set.t * ObjectProperty.Set.t
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
(*|  val add_f : elt -> (unit -> unit) -> (unit -> unit) -> t -> t*)
  val singleton: elt -> t
  val is_singleton : t -> bool
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val is_subset: t -> t -> bool
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
  val union : t -> t -> t
  val diff : t -> t -> t
  val choose : t -> elt
  val iter : (elt -> unit) -> t -> unit
  val iter2 : (elt -> unit) -> t -> t -> unit
  val equal: t -> t -> bool
  val inv : t -> t
  val symm : t -> t
  val atomic : t -> ObjectProperty.Set.t
  val str : t -> string
end

module Map : sig
  type key = t
  type 'a t = 'a ObjectProperty.Map.t * 'a ObjectProperty.Map.t
  val empty: 'a t
  val is_empty: 'a t -> bool
  val singleton : key -> 'a -> 'a t
  val is_singleton : 'a t -> bool
  val add: key -> 'a -> 'a t -> 'a t
  val process : key -> ('a option -> 'a option) -> 'a t -> 'a t  
  val replace : key -> 'a -> 'a t -> 'a t
  val find: key -> 'a t -> 'a
  val remove: key -> 'a t -> 'a t
  val mem: key -> 'a t -> bool
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val iter2 : (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val diff : 'a t -> 'b t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val choose : 'a t -> (key * 'a)
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val right: ('a -> 'a -> bool) -> ('a -> 'a -> 'a -> bool) -> 'a t -> 'a t -> 'a t -> bool
  module Set : sig
    type elt
    type t = ObjectProperty.Set.t * ObjectProperty.Set.t
  end with type elt = key
  val iter_s : (key -> 'a -> unit) -> 'a t -> Set.t -> unit
  val inv : 'a t -> 'a t
  val atomic : ('a -> 'a -> 'a) -> 'a t -> 'a ObjectProperty.Map.t
end