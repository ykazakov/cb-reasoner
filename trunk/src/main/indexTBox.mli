open Owl2
type concept_record = private {
    c_impl : ClassExpression.Set.t;
    c_conj : ClassExpression.t ClassExpression.Map.t;
(*|    c_conja : ClassExpression.t ClassExpression.HMap.t;*)
    c_succ :
    (ClassExpression.Set.t * ClassExpression.Set.t)
    ObjectProperty.Map.t;
    c_succi :
    (ClassExpression.Set.t * ClassExpression.Set.t)
    ObjectProperty.Map.t;
  }
type role_record = private {
    r_succ :
    (ClassExpression.Set.t * ClassExpression.Set.t)
    ClassExpression.Map.t;
    r_succi :
    (ClassExpression.Set.t * ClassExpression.Set.t)
    ClassExpression.Map.t;
    r_sibl : ObjectProperty.Set.t;
    r_isibl : ObjectProperty.Set.t;
    r_sibli : ObjectProperty.Set.t;
    r_isibli : ObjectProperty.Set.t;
  }
type t = private {
    hcr : concept_record ClassExpression.Hashtbl.t;
    hrr : role_record ObjectProperty.Hashtbl.t;
  }
(*|val empty_concept_record : concept_record*)
(*|val empty_role_record : role_record      *)
val find_concept_record : t -> ClassExpression.t -> concept_record
val find_role_record : t -> ObjectProperty.t -> role_record
val init : Ontology.t -> t
val print_statistics : t -> unit
