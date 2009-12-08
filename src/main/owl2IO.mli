open Owl2
val load_ontology : in_channel -> Ontology.t
val fprint_Datatype :
  Format.formatter -> Datatype.Constructor.t Consed.T.consed -> unit
val fprint_ConstrainingFacet :
  Format.formatter ->
  ConstrainingFacet.Constructor.t Consed.T.consed -> unit
val fprint_ObjectProperty :
  Format.formatter ->
  ObjectProperty.Constructor.t Consed.T.consed -> unit
val fprint_DataProperty :
  Format.formatter -> DataProperty.Constructor.t Consed.T.consed -> unit
val fprint_AnnotationProperty :
  Format.formatter ->
  AnnotationProperty.Constructor.t Consed.T.consed -> unit
val fprint_Class :
  Format.formatter -> Class.Constructor.t Consed.T.consed -> unit
val fprint_Individual :
  Format.formatter -> Individual.Constructor.t Consed.T.consed -> unit
val fprint_Literal :
  Format.formatter -> Literal.Constructor.t Consed.T.consed -> unit
val fprint_ObjectPropertyExpression :
  Format.formatter ->
  ObjectPropertyExpression.Constructor.t Consed.T.consed -> unit
val fprint_subObjectPropertyExpression :
  Format.formatter ->
  ObjectPropertyExpression.Constructor.t Consed.T.consed list ->
  unit
val fprint_DataPropertyExpression :
  Format.formatter ->
  DataPropertyExpression.Constructor.t Consed.T.consed -> unit
val fprint_DataRange :
  Format.formatter -> DataRange.Constructor.t Consed.T.consed -> unit
val fprint_ClassExpression :
  Format.formatter ->
  ClassExpression.Constructor.t Consed.T.consed -> unit
val fprint_ClassExpressionAxiom :
  Format.formatter ->
  ClassExpressionAxiom.Constructor.t Consed.T.consed -> unit
val fprint_ObjectPropertyAxiom :
  Format.formatter ->
  ObjectPropertyAxiom.Constructor.t Consed.T.consed -> unit
val fprint_DataPropertyAxiom :
  Format.formatter ->
  DataPropertyAxiom.Constructor.t Consed.T.consed -> unit
val fprint_Assertion :
  Format.formatter -> Assertion.Constructor.t Consed.T.consed -> unit
val fprint_ontology : Format.formatter -> Ontology.t -> unit
val print_ontology_ch : Ontology.t -> out_channel -> unit
val save_ontology : Ontology.t -> string -> unit
val str_of_Datatype :
  Datatype.Constructor.t Consed.T.consed -> string
val str_of_ConstrainingFacet :
  ConstrainingFacet.Constructor.t Consed.T.consed -> string
val str_of_ObjectProperty :
  ObjectProperty.Constructor.t Consed.T.consed -> string
val str_of_DataProperty :
  DataProperty.Constructor.t Consed.T.consed -> string
val str_of_Class : Class.Constructor.t Consed.T.consed -> string
val str_of_Individual :
  Individual.Constructor.t Consed.T.consed -> string
val str_of_Literal :
  Literal.Constructor.t Consed.T.consed -> string
val str_of_ObjectPropertyExpression :
  ObjectPropertyExpression.Constructor.t Consed.T.consed -> string
val str_of_DataPropertyExpression :
  DataPropertyExpression.Constructor.t Consed.T.consed -> string
val str_of_DataRange :
  DataRange.Constructor.t Consed.T.consed -> string
val str_of_ClassExpression :
  ClassExpression.Constructor.t Consed.T.consed -> string
val str_of_ClassExpressionAxiom :
  ClassExpressionAxiom.Constructor.t Consed.T.consed -> string
val str_of_ObjectPropertyAxiom :
  ObjectPropertyAxiom.Constructor.t Consed.T.consed -> string
val str_of_DataPropertyAxiom :
  DataPropertyAxiom.Constructor.t Consed.T.consed -> string
val str_of_Assertion :
  Assertion.Constructor.t Consed.T.consed -> string
