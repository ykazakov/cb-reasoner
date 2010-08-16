open Owl2
type t
val find_implied : t -> ClassExpression.t -> ClassExpression.HSet.t
val find_option_top : t -> ClassExpression.t option
val saturate : Ontology.t -> t
