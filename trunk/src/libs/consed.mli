(* Weak hash maps using the module Weak from the standard library *)

type 'a consed = private {
		data : 'a;
		tag : int;
	}

val compare : 'a consed -> 'a consed -> int
val equal : 'a consed -> 'a consed -> bool
val hash : 'a consed -> int

module type HashedType = sig
	type t
	val equal: t -> t -> bool
	val hash: t -> int
end

module type S = sig
	type key
	type t = key consed
	val cons: key -> t
	val compare : t -> t -> int
	val equal : t -> t -> bool
	val hash : t -> int
end

module Make(H: HashedType): (S with type key = H.t)