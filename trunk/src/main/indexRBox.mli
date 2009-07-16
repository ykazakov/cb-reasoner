type t = {
  (* assigns for every atomic role [a] the set of its subproperties *)
  subproperties : Brole.Set.t OwlSyntax.ObjectProperty.Hashtbl.t;
  (*| assigns for every atomic role [a] a pair consisting of:                 *)
  (*| - a map from roles [r] to the set of role [S] so that [r o S => a]      *)
  (*| - a map from roles [r] to the set of role [S] so that [r o S => (inv a)]*)
  subcomps : ((Brole.Set.t Brole.Map.t) * (Brole.Set.t Brole.Map.t)) OwlSyntax.ObjectProperty.Hashtbl.t;  
  (* a set of transitive atomic roles *)
  mutable trans_roles : OwlSyntax.ObjectProperty.Set.t;
  (* the set of functional roles *)
  mutable funct_roles : OwlSyntax.ObjectProperty.Set.t;
  (* the set of inverse functional roles *)
  mutable inv_funct_roles : OwlSyntax.ObjectProperty.Set.t;
}
val create : int -> t
val init : Ontology.t -> t
