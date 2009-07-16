(* Weak hash maps; inspired by Weak module from the standard library *)

module T : sig
  type 'a consed = private {
      data : 'a;
      tag : int;
    }
end
open T

module type HashedType =
sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
end

module type S =
sig
  type elt
  val cons: elt -> elt consed
end

module Make(H: HashedType): (S with type elt = H.t)