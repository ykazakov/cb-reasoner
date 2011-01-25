open Owl
type concept_record = private {
    mutable c_impl : ClassExpression.Set.t;    
    c_conj : ClassExpression.Set.t ClassExpression.HMap.t;
    mutable c_succ :
    (ClassExpression.Set.t * ClassExpression.Set.t)
    ObjectProperty.Map.t;
    mutable c_succi :
    (ClassExpression.Set.t * ClassExpression.Set.t)
    ObjectProperty.Map.t;
  }
type role_record = private {    
    r_succ :
    (ClassExpression.Set.t * ClassExpression.Set.t)
    ClassExpression.HMap.t;
    r_succi :
    (ClassExpression.Set.t * ClassExpression.Set.t)
    ClassExpression.HMap.t;
    mutable r_sibl : ObjectProperty.Set.t;
    mutable r_isibl : ObjectProperty.Set.t;
    mutable r_sibli : ObjectProperty.Set.t;
    mutable r_isibli : ObjectProperty.Set.t;
  }
type t = private {
    hcr : concept_record ClassExpression.HMap.t;
    hrr : role_record ObjectProperty.HMap.t;
  }
(*|val empty_concept_record : concept_record*)
(*|val empty_role_record : role_record      *)
val find_concept_record : t -> ClassExpression.t -> concept_record
val find_role_record : t -> ObjectProperty.t -> role_record
val init : Ontology.t -> t
val print_statistics : t -> unit
