open Owl

type t
val find_implied : t -> ClassExpression.t -> ClassExpression.HSet.t
val find_option_top : t -> ClassExpression.t option
val compute : ?message:string -> Progress_tracker.t list -> Ontology.t -> t