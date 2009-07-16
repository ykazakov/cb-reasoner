type t
val create : int -> t
val saturate : Ontology.t -> t
val find_subproperties : t -> OwlSyntax.ObjectProperty.Hashtbl.key -> Brole.Set.t
val find_sub_trans : t -> OwlSyntax.ObjectProperty.Hashtbl.key -> Brole.Set.t
val find_funct_roles : t -> OwlSyntax.ObjectProperty.Set.t
val find_inv_funct_roles : t -> OwlSyntax.ObjectProperty.Set.t
val print : t -> Ontology.t -> out_channel -> unit
