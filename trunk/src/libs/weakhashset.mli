(* Weak hash maps; inspired by Weak module from the standard library *)

module type S = sig
  type elt
  (** The type of the elements stored in the table. *)
  type t
  (** The type of tables that contain elements of type [data].
  Note that weak hash tables cannot be marshaled using
  {!Pervasives.output_value } or the functions of the {!Marshal }
  module. *)
  val create : int -> t
  (** [create n] creates a new empty weak hash table, of initial
  size [n]. The table will grow as needed. *)
  val clear : t -> unit
  (** Remove all elements from the table. *)
  val merge : t -> elt -> elt
  (** [merge t x] returns an instance of [x] found in [t] if any,
  or else adds [x] to [t] and return [x]. *)
end

(** The output signature of the functor {!Weak.Make}. *)

module Make (H : Hashtbl.HashedType) : S with type elt = H.t;;
(** Functor building an implementation of the weak hash table structure. *)