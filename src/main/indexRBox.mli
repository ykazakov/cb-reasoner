open Owl2

type role_record = {
  (* the set of subproperties *)
  mutable subprop : Brole.Set.t;  
}

type t = {
  hrr : role_record ObjectProperty.HMap.t;
  (* the set of transitive atomic roles *)
  mutable trans_roles : ObjectProperty.Set.t;
  (* the set of functional roles *)
  mutable funct_roles : ObjectProperty.Set.t;
  (* the set of inverse functional roles *)
  mutable inv_funct_roles : ObjectProperty.Set.t;
}

val create : int -> t
val init : Ontology.t -> t
