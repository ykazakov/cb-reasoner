open Owl

val load_Ontology_from_channel : ?message: string ->
Progress_tracker.t list -> in_channel -> Ontology.t

val fprint_IRI :
Format.formatter -> IRI.t -> unit
val fprint_Datatype :
Format.formatter -> Datatype.t -> unit
val fprint_ConstrainingFacet :
Format.formatter ->
ConstrainingFacet.t -> unit
val fprint_ObjectProperty :
Format.formatter ->
ObjectProperty.t -> unit
val fprint_DataProperty :
Format.formatter -> DataProperty.t -> unit
val fprint_AnnotationProperty :
Format.formatter ->
AnnotationProperty.t -> unit
val fprint_Class :
Format.formatter -> Class.t -> unit
val fprint_Individual :
Format.formatter -> Individual.t -> unit
val fprint_Literal :
Format.formatter -> Literal.t -> unit
val fprint_ObjectPropertyExpression :
Format.formatter ->
ObjectPropertyExpression.t -> unit
val fprint_subObjectPropertyExpression :
Format.formatter ->
ObjectPropertyExpression.t list ->
unit
val fprint_DataPropertyExpression :
Format.formatter ->
DataPropertyExpression.t -> unit
val fprint_DataRange :
Format.formatter -> DataRange.t -> unit
val fprint_ClassExpression :
Format.formatter ->
ClassExpression.t -> unit
val fprint_ClassAxiom :
Format.formatter ->
ClassAxiom.t -> unit
val fprint_ObjectPropertyAxiom :
Format.formatter ->
ObjectPropertyAxiom.t -> unit
val fprint_DataPropertyAxiom :
Format.formatter ->
DataPropertyAxiom.t -> unit
val fprint_Assertion :
Format.formatter -> Assertion.t -> unit
val fprint_ontology : ?message: string -> Progress_tracker.t list ->
Format.formatter -> Ontology.t -> unit
val print_ontology_ch : ?message: string -> Progress_tracker.t list ->
Ontology.t -> out_channel -> unit
val save_ontology : ?message: string -> Progress_tracker.t list ->
Ontology.t -> string -> unit
val fprint_class_taxonomy : ?message: string -> Progress_tracker.t list ->
Format.formatter -> Class_taxonomy.t -> unit
val print_class_taxonomy_ch : ?message: string -> Progress_tracker.t list ->
Class_taxonomy.t -> out_channel -> unit

val str_of_IRI : IRI.t -> string
val str_of_Datatype :
Datatype.t -> string
val str_of_ConstrainingFacet :
ConstrainingFacet.t -> string
val str_of_ObjectProperty :
ObjectProperty.t -> string
val str_of_DataProperty :
DataProperty.t -> string
val str_of_Class : Class.t -> string
val str_of_Individual :
Individual.t -> string
val str_of_Literal :
Literal.t -> string
val str_of_ObjectPropertyExpression :
ObjectPropertyExpression.t -> string
val str_of_DataPropertyExpression :
DataPropertyExpression.t -> string
val str_of_DataRange :
DataRange.t -> string
val str_of_ClassExpression :
ClassExpression.t -> string
val str_of_ClassAxiom :
ClassAxiom.t -> string
val str_of_ObjectPropertyAxiom :
ObjectPropertyAxiom.t -> string
val str_of_DataPropertyAxiom :
DataPropertyAxiom.t -> string
val str_of_Assertion :
Assertion.t -> string
