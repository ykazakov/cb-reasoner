open Owl2
type t
val create : int -> t
val saturate : Ontology.t -> t
val find_subproperties : t -> ObjectProperty.HMap.key -> Brole.Set.t
val find_sub_trans : t -> ObjectProperty.HMap.key -> Brole.Set.t
val find_funct_roles : t -> ObjectProperty.Set.t
val find_inv_funct_roles : t -> ObjectProperty.Set.t
val print : t -> Ontology.t -> out_channel -> unit
