(* Weak hash maps; inspired by Weak module from the standard library *)

open CommonTypes

module T : sig
  type 'a consed = private {
      data : 'a;
      tag : int;
    }
end

module type OHT = sig
  open T
  val compare : 'a consed -> 'a consed -> int
  val equal : 'a consed -> 'a consed -> bool
  val hash : 'a consed -> int
end
module OHT : OHT

open T
include OHT


module type HashedType =
sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
end

module type S = sig
  type t
  type key
  val create: int -> t
  val cons: t -> key -> key consed
  val iter: (key consed -> unit) -> t -> unit
end

module Make(H: HashedType): (S with type key = H.t)