val load_ontology : in_channel -> Ontology.t

val str_of_object_property : OwlSyntax.ObjectProperty.t -> string
val str_of_class : OwlSyntax.Class.t -> string
val str_of_individual : OwlSyntax.Individual.t -> string
val str_of_object_property_expression : OwlSyntax.ObjectPropertyExpression.t -> string
val str_of_class_expression : OwlSyntax.ClassExpression.t -> string
val str_of_object_property_expression_axiom : OwlSyntax.ObjectPropertyAxiom.t -> string
val str_of_class_expression_axiom : OwlSyntax.ClassExpressionAxiom.t -> string
val str_of_assertion : OwlSyntax.Assertion.t -> string
val print_ontology_ch : Ontology.t -> out_channel -> unit
val save_ontology : Ontology.t -> string -> unit
val print_ontology : Ontology.t -> out_channel -> unit
