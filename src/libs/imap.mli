module type IType = sig
  type t
  val compare : t -> t -> int
  val right : t -> t -> t -> bool
end

module type S = sig
  type key
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

module Make (I: IType) : S with type key = I.t