type concept_record = private {
    c_impl : OwlSyntax.ClassExpression.Set.t;
    c_conj : OwlSyntax.ClassExpression.t OwlSyntax.ClassExpression.Map.t;
(*|    c_conja : OwlSyntax.ClassExpression.t OwlSyntax.ClassExpression.HMap.t;*)
    c_succ :
    (OwlSyntax.ClassExpression.Set.t * OwlSyntax.ClassExpression.Set.t)
    OwlSyntax.ObjectProperty.Map.t;
    c_succi :
    (OwlSyntax.ClassExpression.Set.t * OwlSyntax.ClassExpression.Set.t)
    OwlSyntax.ObjectProperty.Map.t;
  }
type role_record = private {
    r_succ :
    (OwlSyntax.ClassExpression.Set.t * OwlSyntax.ClassExpression.Set.t)
    OwlSyntax.ClassExpression.Map.t;
    r_succi :
    (OwlSyntax.ClassExpression.Set.t * OwlSyntax.ClassExpression.Set.t)
    OwlSyntax.ClassExpression.Map.t;
    r_sibl : OwlSyntax.ObjectProperty.Set.t;
    r_isibl : OwlSyntax.ObjectProperty.Set.t;
    r_sibli : OwlSyntax.ObjectProperty.Set.t;
    r_isibli : OwlSyntax.ObjectProperty.Set.t;
  }
type t = private {
    hcr : concept_record OwlSyntax.ClassExpression.Hashtbl.t;
    hrr : role_record OwlSyntax.ObjectProperty.Hashtbl.t;
  }
(*|val empty_concept_record : concept_record*)
(*|val empty_role_record : role_record      *)
val find_concept_record : t -> OwlSyntax.ClassExpression.t -> concept_record
val find_role_record : t -> OwlSyntax.ObjectProperty.t -> role_record
val init : Ontology.t -> t
val print_statistics : t -> unit
