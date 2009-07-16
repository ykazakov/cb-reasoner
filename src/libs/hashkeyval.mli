module type S =
  sig
    type key
    type 'a t
    val create : int -> 'a t
    val length : 'a t -> int
    val cons : 'a t -> key -> (unit -> 'a) -> key * 'a
    val iter : (key * 'a -> unit) -> 'a t -> unit
  end
module Make (H : CommonTypes.HashedType) : S with type key = H.t    