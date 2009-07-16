type t
val find_implied : t -> OwlSyntax.ClassExpression.t -> OwlSyntax.ClassExpression.Set.t
val find_option_top : t -> OwlSyntax.ClassExpression.t option
val saturate : Ontology.t -> t
