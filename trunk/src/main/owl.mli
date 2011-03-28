(* Modules for manipulating with OWL 2 objects such as classes, class      *)
(* expressions and axioms; uses camlp4 preprocessor to generate hash,      *)
(* equality and comparison functions                                       *)

open Consed

(**================= Common Interfaces ==================**)

module type OrderedHashedType = sig
	type t
	val compare : t -> t -> int
	val equal : t -> t -> bool
	val hash : t -> int
end

module CommonExtended : sig
	module type S = sig
		include OrderedHashedType
		module Set : Set.S with type elt = t
		module Map : Map.S with type key = t
		module HSet : Hashsetlp.S with type key = t and type elt = t
		module HMap : Hashtbllp.S with type key = t
	end
end

module CommonConsed : sig
	module type S = sig
		include Consed.S
		module Set : Cset.S with type elt = t
		module Map : Cmap.S with type key = t and module Set = Set
		module HSet : Chashsetlp.S with type key = t
		module HMap : Chashtbllp.S with type key = t
	end
end

(**================= Auxiliary Modules ==================**)

module O_string : sig
	type t = string
end

module O_int : sig
	type t = int
end
  
module O_list : sig
	type 'a t = 'a list
end

module O_option : sig
	type 'a t = 'a option
end

(**======================== IRIs ========================**)

module rec IRI_Constructor : sig
  save
 	| IRI of O_string
  make_type	
end 

and IRI : sig	
	include CommonConsed.S with type key = IRI_Constructor.t
	val str_of : t -> string
end

(**====================== NodeIDs =======================**)

and NodeID_Constructor : sig	
  save
   | NodeID of O_string
  make_type
end
	
and NodeID : sig	
	include CommonConsed.S with type key = NodeID_Constructor.t
	val str_of : t -> string
end

(**====================== Datatypes =====================**)

and Datatype_Constructor : sig
  save
    | IRI of IRI
    | Rdfs_Literal
    | Owl_real
    | Owl_rational
    | Xsd_decimal
    | Xsd_integer
    | Xsd_nonNegativeInteger
    | Xsd_nonPositiveInteger
    | Xsd_positiveInteger
    | Xsd_negativeInteger
    | Xsd_long
    | Xsd_int
    | Xsd_short
    | Xsd_byte
    | Xsd_unsignedLong
    | Xsd_unsignedInt
    | Xsd_unsignedShort
    | Xsd_unsignedByte
    | Xsd_double
    | Xsd_float
    | Rdf_PlainLiteral
    | Xsd_string
    | Xsd_normalizedString
    | Xsd_token
    | Xsd_language
    | Xsd_Name
    | Xsd_NCName
    | Xsd_NMTOKEN
    | Xsd_boolean
    | Xsd_hexBinary
    | Xsd_base64Binary
    | Xsd_anyURI
    | Xsd_dateTime
    | Xsd_dateTimeStamp
    | Rdf_XMLLiteral
  make_type
	make_cases	
end

and Datatype : sig 
	include CommonExtended.S with type t = Datatype_Constructor.t
	val str_of : t -> string	
end

(**================== Constraining Facets ===============**)

and ConstrainingFacet_Constructor : sig
		save
			| IRI of IRI
			| Xsd_minInclusive
			| Xsd_maxInclusive
			| Xsd_minExclusive
			| Xsd_maxExclusive
			| Xsd_length
			| Xsd_minLength
			| Xsd_maxLength
			| Xsd_pattern
			| Rdf_langRange
		make_type
		make_cases
end

and ConstrainingFacet : sig	
	include CommonExtended.S with type t = ConstrainingFacet_Constructor.t
	val str_of : t -> string
end

(**================= Object Properties ==================**)

and ObjectProperty_Constructor : sig
		save
			| IRI of IRI
			| TopObjectProperty
			| BottomObjectProperty
		make_type
		make_cases
end

and ObjectProperty : sig
  include CommonConsed.S with type key = ObjectProperty_Constructor.t
  val str_of : t -> string
end

(**=================== Data Properties ==================**)

and DataProperty_Constructor : sig
		save
			| IRI of IRI
			| TopDataProperty
			| BottomDataProperty
		make_type
		make_cases
end

and DataProperty : sig 
	include CommonExtended.S with type t = DataProperty_Constructor.t
	val str_of : t -> string
end

(**================ Annotation Properties ===============**)

and AnnotationProperty_Constructor : sig
		save
			| IRI of IRI
			| Rdfs_label
			| Rdfs_comment
			| Rdfs_seeAlso
			| Rdfs_isDefinedBy
			| Owl_deprecated
			| Owl_versionInfo
			| Owl_priorVersion
			| Owl_backwardCompatibleWith
			| Owl_incompatibleWith
		make_type
		make_cases
end

and AnnotationProperty : sig	
	include CommonExtended.S with type t = AnnotationProperty_Constructor.t
	val str_of : t -> string
end

(**====================== Classes =======================**)

and Class_Constructor : sig
		save
			| IRI of IRI
			| Thing
			| Nothing
		make_type
		make_cases
end

and Class : sig 
	include CommonExtended.S with type t = Class_Constructor.t
	val str_of : t -> string
end

(**==================== Individuals =====================**)

and Individual_Constructor : sig
		save
			| NamedIndividual of IRI
			| AnonymousIndividual of NodeID
		make_type
		make_cases
end

and Individual : sig
	include CommonExtended.S with type t = Individual_Constructor.t
	val str_of : t -> string
end

(**======================= Literals =====================**)

and Literal_Constructor : sig
		save
			| TypedLiteral of O_string * Datatype
			| StringLiteralNoLanguage of O_string
			| StringLiteralWithLanguage of O_string * O_string
		make_type
		make_cases
end

and Literal : sig
  include CommonExtended.S with type t = Literal_Constructor.t
  val str_of : t -> string
end

(**=================== Declarations =====================**)

and Declaration_Constructor : sig
    save
      | Class of Class        
      | Datatype of Datatype
      | ObjectProperty of ObjectProperty
      | DataProperty of DataProperty
      | AnnotationProperty of AnnotationProperty
      | NamedIndividual of Individual
      make_ohtype
      make_cases
end
    
and Declaration : (CommonConsed.S with type key = Declaration_Constructor.t)
   
(**============ Object Property Expressions =============**)

and ObjectPropertyExpression_Constructor : sig
		save
			| ObjectProperty of ObjectProperty
			| ObjectInverseOf of ObjectProperty
		make_type
		make_cases
end

and ObjectPropertyExpression : (CommonConsed.S with type key = ObjectPropertyExpression_Constructor.t)

(**============= Data Property Expressions ==============**)

and DataPropertyExpression_Constructor : sig
		save
			| DataProperty of DataProperty
		make_type
		make_cases
end

and DataPropertyExpression : (CommonConsed.S with type key = DataPropertyExpression_Constructor.t)

(**==================== Data Ranges =====================**)

and DataRange_Constructor : sig
		save
			| Datatype of Datatype
			| DataIntersectionOf of DataRange O_list
			| DataUnionOf of DataRange O_list
			| DataComplementOf of DataRange
			| DataOneOf of Literal O_list
			| DatatypeRestriction of Datatype * ((ConstrainingFacet * Literal) O_list)
		make_type
		make_cases
end

and DataRange : (CommonConsed.S with type key = DataRange_Constructor.t)

(**==================== Class Expressions ==================**)

and ClassExpression_Constructor : sig
	 save
	    | Class of Class
			| ObjectIntersectionOf of ClassExpression * ClassExpression
			| ObjectIntersectionOfrec of ClassExpression * ClassExpression
			| ObjectUnionOf of ClassExpression * ClassExpression
			| ObjectUnionOfrec of ClassExpression * ClassExpression
			| ObjectComplementOf of ClassExpression
			| ObjectOneOf of Individual O_list
			| ObjectSomeValuesFrom of ObjectPropertyExpression * ClassExpression
			| ObjectAllValuesFrom of ObjectPropertyExpression * ClassExpression
			| ObjectHasValue of ObjectPropertyExpression * Individual
			| ObjectHasSelf of ObjectPropertyExpression
			| ObjectMinCardinality of O_int * ObjectPropertyExpression * ClassExpression O_option
			| ObjectMaxCardinality of O_int * ObjectPropertyExpression * ClassExpression O_option
			| ObjectExactCardinality of O_int * ObjectPropertyExpression * ClassExpression O_option
			| DataSomeValuesFrom of DataPropertyExpression O_list * DataRange
			| DataAllValuesFrom of DataPropertyExpression O_list * DataRange
			| DataHasValue of DataPropertyExpression * Literal
			| DataMinCardinality of O_int * DataPropertyExpression * DataRange O_option
			| DataMaxCardinality of O_int * DataPropertyExpression * DataRange O_option
			| DataExactCardinality of O_int * DataPropertyExpression * DataRange O_option
   make_type
	 make_cases
end

and ClassExpression : (CommonConsed.S with type key = ClassExpression_Constructor.t)

(**================= Class ExpressionAxioms ================**)

and ClassAxiom_Constructor : sig
		save
			| SubClassOf of ClassExpression * ClassExpression
			| EquivalentClasses of ClassExpression O_list
			| DisjointClasses of ClassExpression O_list
			| DisjointUnion of Class * ClassExpression O_list
		make_type
		make_cases
end

and ClassAxiom : (CommonConsed.S with type key = ClassAxiom_Constructor.t)

(**================= Object Property Axioms ================**)

and ObjectPropertyAxiom_Constructor : sig
		save
			| SubObjectPropertyOf of ObjectPropertyExpression O_list * ObjectPropertyExpression
			| EquivalentObjectProperties of ObjectPropertyExpression O_list
			| DisjointObjectProperties of ObjectPropertyExpression O_list
			| InverseObjectProperties of ObjectPropertyExpression * ObjectPropertyExpression
			| ObjectPropertyDomain of ObjectPropertyExpression * ClassExpression
			| ObjectPropertyRange of ObjectPropertyExpression * ClassExpression
			| FunctionalObjectProperty of ObjectPropertyExpression
			| InverseFunctionalObjectProperty of ObjectPropertyExpression
			| ReflexiveObjectProperty of ObjectPropertyExpression
			| IrreflexiveObjectProperty of ObjectPropertyExpression
			| SymmetricObjectProperty of ObjectPropertyExpression
			| AsymmetricObjectProperty of ObjectPropertyExpression
			| TransitiveObjectProperty of ObjectPropertyExpression
		make_type
		make_cases
end

and ObjectPropertyAxiom : (CommonConsed.S with type key = ObjectPropertyAxiom_Constructor.t)

(**================= Data Property Axioms ================**)

and DataPropertyAxiom_Constructor : sig
		save
			| SubDataPropertyOf of DataPropertyExpression * DataPropertyExpression
			| EquivalentDataProperties of DataPropertyExpression O_list
			| DisjointDataProperties of DataPropertyExpression O_list
			| DataPropertyDomain of DataPropertyExpression * ClassExpression
			| DataPropertyRange of DataPropertyExpression * DataRange
			| FunctionalDataProperty of DataPropertyExpression
		make_type
		make_cases
end

and DataPropertyAxiom : (CommonConsed.S with type key = DataPropertyAxiom_Constructor.t)

(**================= Datatype Definitions =================**)

and DatatypeDefinition_Constructor : sig
	save
			| DatatypeDefinition of Datatype * DataRange
	make_type
	make_cases
end

and DatatypeDefinition : (CommonConsed.S with type key = DatatypeDefinition_Constructor.t)

(**========================= Keys =========================**)

and Key_Constructor : sig
		save
			| HasKey of ClassExpression * ObjectPropertyExpression O_list * DataPropertyExpression O_list
		make_type
		make_cases
end

and Key : (CommonConsed.S with type key = Key_Constructor.t)

(**====================== Assertions ======================**)

and Assertion_Constructor : sig
		save
			| SameIndividual of Individual O_list
			| DifferentIndividuals of Individual O_list
			| ClassAssertion of ClassExpression * Individual
			| ObjectPropertyAssertion of ObjectPropertyExpression * Individual * Individual
			| NegativeObjectPropertyAssertion of ObjectPropertyExpression * Individual * Individual
			| DataPropertyAssertion of DataPropertyExpression * Individual * Literal
			| NegativeDataPropertyAssertion of DataPropertyExpression * Individual * Literal
		make_type
		make_cases
end

and Assertion : (CommonConsed.S with type key = Assertion_Constructor.t)

(**================= Annotation Subjects ==================**)

and AnnotationSubject_Constructor : sig
		save
			| IRI of IRI
			| AnonymousIndividual of NodeID
		make_type
		make_cases
end

and AnnotationSubject : (CommonConsed.S with type key = AnnotationSubject_Constructor.t)

(**================== Annotation Values ===================**)

and AnnotationValue_Constructor : sig
		save
			| AnonymousIndividual of NodeID
			| IRI of IRI
			| Literal of Literal
		make_type
		make_cases
end

and AnnotationValue : (CommonConsed.S with type key = AnnotationValue_Constructor.t)

(**===================== Annotations ======================**)

and Annotation_Constructor : sig
		save
			| Annotation of Annotation O_list * AnnotationProperty * AnnotationValue
		make_type
		make_cases
end

and Annotation : (CommonConsed.S with type key = Annotation_Constructor.t)

(**================== Annotation Axioms ===================**)

and AnnotationAxiom_Constructor : sig
		save
			| AnnotationAssertion of Annotation O_list * AnnotationProperty * AnnotationSubject * AnnotationValue
			| SubAnnotationPropertyOf of Annotation O_list * AnnotationProperty * AnnotationProperty
			| AnnotationPropertyDomain of Annotation O_list * AnnotationProperty * IRI
			| AnnotationPropertyRange of Annotation O_list * AnnotationProperty * IRI
		make_type
		make_cases
end

and AnnotationAxiom : (CommonConsed.S with type key = AnnotationAxiom_Constructor.t)