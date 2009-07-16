module type OrderedType = sig type t val compare : t -> t -> int end
module type HashedType =
  sig type t val equal : t -> t -> bool val hash : t -> int end
module type OrderedHashedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end
module type OrderedHashedTypeStr =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
    val str : t -> string
  end
module OrderedList :
  sig
    module type S = OrderedHashedTypeStr
    module Make :
      functor (O : OrderedHashedTypeStr) ->
        sig
          type t = O.t list
          val equal : t -> t -> bool
          val hash : t -> int
          val compare : t -> t -> int
          val str : t -> string
        end
  end
module OrderedPair :
  sig
    module type S = OrderedHashedTypeStr
    module Make :
      functor (O1 : OrderedHashedTypeStr) ->
        functor (O2 : OrderedHashedTypeStr) ->
          sig
            type t = O1.t * O2.t
            val equal : t -> t -> bool
            val hash : t -> int
            val compare : t -> t -> int
            val str : t -> string
          end
  end
