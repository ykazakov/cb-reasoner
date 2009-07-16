(** functions for manipulations with polarities **)

  type t = Positive | Negative | Both
  val inverse : t -> t
  val str : t -> string
  val is_positive : t -> bool
  val is_negative : t -> bool

module Counter :
sig
  type p = t
  type t  
  val get_pos : t -> int
  val get_neg : t -> int
  val get_total : t -> int
  val zero : t
  val inverse : t -> t
  val symm : t -> t
  val to_elt : p -> t
  val succ : t -> p -> t
  val pred : t -> p -> t
  val sum : t -> t -> t  
end