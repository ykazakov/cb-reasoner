(* hash sets based on hashtbl in the standard library *)
(** Hash sets.

Hash sets are hashed association sets, with in - place modification.
*)

(** {6 Generic interface} *)

type 'a t
(** The type of hash set of elements of type ['a]. *)

val create : int -> 'a t
(** [Hashtbl.create n] creates a new, empty hash set, with
initial size [n]. For best results, [n] should be on the
order of the expected number of elements that will be in
the set. The set grows as needed, so [n] is just an
initial guess. *)

val clear : 'a t -> unit
(** Empty a hash set. *)

val add : 'a t -> 'a -> unit
(** [Hashset.add set x] adds an element [x] to set [set].
It does nothing if [x] was already in [set] *)

val copy : 'a t -> 'a t
(** Return a copy of the given hashset. *)

val mem : 'a t -> 'a -> bool
(** [Hashset.mem set x] checks if [x] is in [set]. *)

val find : 'a t -> 'a -> 'a
(** [Hashset.find set x] seraches returns an element [y] in [set]
whcih is equal to [y]. *)

val remove : 'a t -> 'a -> unit
(** [Hashset.remove set x] removes [x] from [set].
It does nothing if [x] is not in [set]. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [Hashset.iter f set] applies [f] to all elements in set [set].
The order in which the elements are passed to [f] is unspecified. *)

val fold : ('a -> 'c -> 'c) -> 'a t -> 'c -> 'c
(** [Hashset.fold f set init] computes
[(f xN ... (f x1 init)...)],
where [x1 ... xN] are elements of [set].
The order in which the elements are passed to [f] is unspecified. *)

val length : 'a t -> int
(** [Hashset.length set] returns the cardinality of [set]. *)

(** {6 Functorial interface} *)

module type HashedType =
sig
  type t
  type v
  val equal: t -> t -> bool
  val hash: t -> int
  val key: v -> t
end
(** The input signature of the functor {!Hashset.Make}. *)

module type S =
sig
  type key
  type elt
  type t
  val create: int -> t
  val clear: t -> unit
  val copy: t -> t
  val add: t -> elt -> unit
  val remove: t -> key -> unit
  val mem : t -> key -> bool
  val find : t -> key -> elt
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val length: t -> int
end
(** The output signature of the functor {!Hashset.Make}. *)

module Make (H : HashedType) : S with type key = H.t and type elt = H.v
(** Functor building an implementation of the hashset structure.
The functor [Hashset.Make] returns a structure containing
a type [key] of keys and a type [t] of hash sets over
elements of type [key].
The operations perform similarly to those of the generic
interface, but use the hashing and equality functions
specified in the functor argument [H] instead of generic
equality and hashing. *)

(** {6 The polymorphic hash primitive} *)

val hash : 'a -> int
(** [Hashset.hash x] associates a positive integer to any value of
any type. It is guaranteed that
if [x = y] or [Pervasives.compare x y = 0], then [hash x = hash y].
Moreover, [hash] always terminates, even on cyclic
structures. *)

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"
(** [Hashset.hash_param n m x] computes a hash value for [x], with the
same properties as for [hash]. The two extra parameters [n] and
[m] give more precise control over hashing. Hashing performs a
depth - first, right - to - left traversal of the structure [x], stopping
after [n] meaningful nodes were encountered, or [m] nodes,
meaningful or not, were encountered. Meaningful nodes are: integers;
floating - point numbers; strings; characters; booleans; and constant
constructors. Larger values of [m] and [n] means that more
nodes are taken into account to compute the final hash
value, and therefore collisions are less likely to happen.
However, hashing takes longer. The parameters [m] and [n]
govern the tradeoff between accuracy and speed. *)
