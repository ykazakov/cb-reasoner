open Owl2

module ClassMap : Map.S with type key = Class.t
module ClassSet : Set.S with type elt = Class.t

type t
val create : unit -> t

(**======================= consing =========================**)

val cons_IRI : t -> IRI.elt -> IRI.t
val cons_NodeID : t -> NodeID.elt -> NodeID.t
val cons_ObjectProperty : t -> ObjectProperty.elt -> ObjectProperty.t
val cons_Literal : t -> Literal.elt -> Literal.t
val cons_ObjectPropertyExpression : t -> ObjectPropertyExpression.elt -> ObjectPropertyExpression.t
val cons_DataPropertyExpression : t -> DataPropertyExpression.elt -> DataPropertyExpression.t
val cons_DataRange : t -> DataRange.elt -> DataRange.t
val cons_ClassExpression : t -> ClassExpression.elt -> ClassExpression.t
val cons_ClassExpressionAxiom : t -> ClassExpressionAxiom.elt -> ClassExpressionAxiom.t
val cons_ObjectPropertyAxiom : t -> ObjectPropertyAxiom.elt -> ObjectPropertyAxiom.t
val cons_DataPropertyAxiom : t -> DataPropertyAxiom.elt -> DataPropertyAxiom.t
val cons_DatatypeDefinition : t -> DatatypeDefinition.elt -> DatatypeDefinition.t
val cons_Key : t -> Key.elt -> Key.t
val cons_Assertion : t -> Assertion.elt -> Assertion.t
val cons_AnnotationSubject : t -> AnnotationSubject.elt -> AnnotationSubject.t
val cons_AnnotationValue : t -> AnnotationValue.elt -> AnnotationValue.t
val cons_Annotation : t -> Annotation.elt -> Annotation.t
val cons_AnnotationAxiom : t -> AnnotationAxiom.elt -> AnnotationAxiom.t

(**====================== iterators ========================**)

val iter_record_ObjectProperty : (ObjectProperty.t -> int -> unit) -> t -> unit
val iter_record_Class : (Class.t -> int -> unit) -> t -> unit
val iter_record_Individual : (Individual.t -> int -> unit) -> t -> unit

val iter_record_ComplexObjectPropertyExpression : (ObjectPropertyExpression.t -> Polarity.Counter.t -> unit) -> t -> unit
val iter_record_ComplexClassExpression : (ClassExpression.t -> Polarity.Counter.t -> unit) -> t -> unit

val iter_record_ObjectPropertyAxiom : (ObjectPropertyAxiom.t -> unit) -> t -> unit
val iter_record_ClassExpressionAxiom : (ClassExpressionAxiom.t -> unit) -> t -> unit
val iter_record_Assertion : (Assertion.t -> unit) -> t -> unit

(**============== statistical information ================**)

val has_positive_Nothing : t -> bool
val has_positive_ComplementOf : t -> bool
val has_negative_Thing : t -> bool

val total_ObjectPropertyIRI : t -> int
val total_ClassIRI : t -> int
val total_IndividualIRI : t -> int

val count_TopObjectProperty : t -> Polarity.Counter.t
val count_BottomObjectProperty : t -> Polarity.Counter.t
val count_Thing : t -> Polarity.Counter.t
val count_Nothing : t -> Polarity.Counter.t

val count_InverseObjectProperty : t -> Polarity.Counter.t
val total_InverseObjectProperty : t -> int

val count_ObjectIntersectionOf : t -> Polarity.Counter.t
val count_ObjectUnionOf : t -> Polarity.Counter.t
val count_ObjectComplementOf : t -> Polarity.Counter.t
val count_ObjectOneOf : t -> Polarity.Counter.t
val count_ObjectSomeValuesFrom : t -> Polarity.Counter.t
val count_ObjectAllValuesFrom : t -> Polarity.Counter.t
val count_ObjectHasValue : t -> Polarity.Counter.t
val count_ObjectHasSelf : t -> Polarity.Counter.t
val count_ObjectMinCardinality : t -> Polarity.Counter.t
val count_ObjectMaxCardinality : t -> Polarity.Counter.t
val count_ObjectExactCardinality : t -> Polarity.Counter.t
val count_DataSomeValuesFrom : t -> Polarity.Counter.t
val count_DataAllValuesFrom : t -> Polarity.Counter.t
val count_DataHasValue : t -> Polarity.Counter.t
val count_DataMinCardinality : t -> Polarity.Counter.t
val count_DataMaxCardinality : t -> Polarity.Counter.t
val count_DataExactCardinality : t -> Polarity.Counter.t

val total_ObjectIntersectionOf : t -> int
val total_ObjectUnionOf : t -> int
val total_ObjectComplementOf : t -> int
val total_ObjectOneOf : t -> int
val total_ObjectSomeValuesFrom : t -> int
val total_ObjectAllValuesFrom : t -> int
val total_ObjectHasValue : t -> int
val total_ObjectHasSelf : t -> int
val total_ObjectMinCardinality : t -> int
val total_ObjectMaxCardinality : t -> int
val total_ObjectExactCardinality : t -> int
val total_DataSomeValuesFrom : t -> int
val total_DataAllValuesFrom : t -> int
val total_DataHasValue : t -> int
val total_DataMinCardinality : t -> int
val total_DataMaxCardinality : t -> int
val total_DataExactCardinality : t -> int
                  
val total_SubPropertyOf : t -> int                              
val total_EquivalentProperties : t -> int
val total_InverseProperties : t -> int
val total_FunctionalProperty : t -> int
val total_TransitiveProperty : t -> int
val total_RoleComposition : t -> int
val total_SubClassOf : t -> int
val total_EquivalentClasses : t -> int
val total_ClassAssertion : t -> int
val total_PropertyAssertion : t -> int

(**================= insertion of axioms ====================**)

val add_ObjectPropertyAxiom : t -> ObjectPropertyAxiom.t -> unit
val add_ClassExpressionAxiom : t -> ClassExpressionAxiom.t -> unit
val add_Assertion : t -> Assertion.t -> unit

(**=============== printing of statistics ===================**)
val print_staticstics : t -> out_channel -> unit
