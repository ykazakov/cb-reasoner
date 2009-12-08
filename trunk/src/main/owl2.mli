open Consed.T

(**================= Common Interfaces ==================**)

module Common : sig
  module type S = sig
    type elt
    type t = elt Consed.T.consed
    val compare: t -> t -> int
    val hash: t -> int
    val equal: t -> t -> bool
    val right: t -> t -> t -> bool
    val cons: elt -> t
    module Hashtbl : Chashmap.S with type key = t
    module OSet : Set.S with type elt = t
    module OMap : Map.S with type key = t
    module Set : Cset.S with type elt = t
    module Map : Cmap.S with type key = t and module Set = Set
    module HSet : Hashsetlp.S with type key = t and type elt = t
    module HMap : Chashmap.S with type key = t
  end
end

(**====================== Datatypes =====================**)

module Datatype : sig
  module Constructor : sig
    type t =
      | IRI of string
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
  end
  include Common.S with type elt = Constructor.t
end

(**================== Constraining Facets ===============**)

module ConstrainingFacet : sig
  module Constructor : sig
    type t =
      | IRI of string
      | Xsd_minInclusive
      | Xsd_maxInclusive
      | Xsd_minExclusive
      | Xsd_maxExclusive
      | Xsd_length
      | Xsd_minLength
      | Xsd_maxLength
      | Xsd_pattern
      | Rdf_langRange
  end
  include Common.S with type elt = Constructor.t
end

(**================= Object Properties ==================**)

module ObjectProperty : sig
  module Constructor : sig
    type t =
      | IRI of string
      | TopObjectProperty
      | BottomObjectProperty
  end
  include Common.S with type elt = Constructor.t
end

(**=================== Data Properties ==================**)

module DataProperty : sig
  module Constructor : sig
    type t =
      | IRI of string
      | TopDataProperty
      | BottomDataProperty
  end
  include Common.S with type elt = Constructor.t
end

(**================ Annotation Properties ===============**)

module AnnotationProperty : sig
  module Constructor : sig
    type t =
      | IRI of string
      | Rdfs_label
      | Rdfs_comment
      | Rdfs_seeAlso
      | Rdfs_isDefinedBy
      | Owl_deprecated
      | Owl_versionInfo
      | Owl_priorVersion
      | Owl_backwardCompatibleWith
      | Owl_incompatibleWith
  end
  include Common.S with type elt = Constructor.t
end

(**====================== Classes =======================**)

module Class : sig
  module Constructor : sig
    type t =
      | IRI of string
      | Thing
      | Nothing
  end
  include Common.S with type elt = Constructor.t
end

(**==================== Individuals =====================**)

module Individual : sig
  module Constructor : sig
    type t =
      | NamedIndividual of string
      | AnonymousIndividual of string
  end
  include Common.S with type elt = Constructor.t
end

(**======================= Literals =====================**)

module Literal : sig
  module Constructor : sig
    type t =
      | TypedLiteral of string * Datatype.t
      | StringLiteralNoLanguage of string
      | StringLiteralWithLanguage of string * string
  end
  include Common.S with type elt = Constructor.t
end

(**============ Object Property Expressions =============**)

module ObjectPropertyExpression : sig
  module Constructor : sig
    type t =
      | ObjectProperty of ObjectProperty.t
      | InverseObjectProperty of ObjectProperty.t
  end
  include (Common.S with type elt = Constructor.t)
end

(**============= Data Property Expressions ==============**)

module DataPropertyExpression : sig
  module Constructor : sig
    type t =
      | DataProperty of DataProperty.t
  end
  include (Common.S with type elt = Constructor.t)
end

(**==================== Data Ranges =====================**)

module DataRange : sig
  module Constructor : sig
    type t =
      | Datatype of Datatype.t
      | DataIntersectionOf of t consed Cset.t
      | DataUnionOf of t consed Cset.t
      | DataComplementOf of t consed
      | DataOneOf of Literal.Set.t
      | DatatypeRestriction of Datatype.t * ((ConstrainingFacet.t * Literal.t) list)
  end
  include (Common.S with type elt = Constructor.t)
end

(**==================== Class Expressions ==================**)

module ClassExpression : sig
  module Constructor : sig
    type t =
      | Class of Class.t
      | ObjectIntersectionOf of t consed Cset.t
      | ObjectUnionOf of t consed Cset.t
      | ObjectComplementOf of t consed
      | ObjectOneOf of Individual.Set.t
      | ObjectSomeValuesFrom of ObjectPropertyExpression.t * (t consed)
      | ObjectAllValuesFrom of ObjectPropertyExpression.t * (t consed)
      | ObjectHasValue of ObjectPropertyExpression.t * Individual.t
      | ObjectHasSelf of ObjectPropertyExpression.t
      | ObjectMinCardinality of int * ObjectPropertyExpression.t * (t consed option)
      | ObjectMaxCardinality of int * ObjectPropertyExpression.t * (t consed option)
      | ObjectExactCardinality of int * ObjectPropertyExpression.t * (t consed option)
      | DataSomeValuesFrom of DataPropertyExpression.t list * DataRange.t
      | DataAllValuesFrom of DataPropertyExpression.t list * DataRange.t
      | DataHasValue of DataPropertyExpression.t * Literal.t
      | DataMinCardinality of int * DataPropertyExpression.t * (DataRange.t option)
      | DataMaxCardinality of int * DataPropertyExpression.t * (DataRange.t option)
      | DataExactCardinality of int * DataPropertyExpression.t * (DataRange.t option)
  end
  include (Common.S with type elt = Constructor.t)
end

(**================= Class ExpressionAxioms ================**)

module ClassExpressionAxiom : sig
  module Constructor : sig
    type t =
      | SubClassOf of ClassExpression.t * ClassExpression.t
      | EquivalentClasses of ClassExpression.t Cset.t
      | DisjointClasses of ClassExpression.t Cset.t
      | DisjointUnion of Class.t * ClassExpression.t Cset.t
  end
  include (Common.S with type elt = Constructor.t)
end

(**================= Object Property Axioms ================**)

module ObjectPropertyAxiom : sig
  module Constructor : sig
    type t =
      | SubObjectPropertyOf of ObjectPropertyExpression.t list * ObjectPropertyExpression.t
      | EquivalentObjectProperties of ObjectPropertyExpression.t Cset.t
      | DisjointObjectProperties of ObjectPropertyExpression.t Cset.t
      | InverseObjectProperties of ObjectPropertyExpression.t * ObjectPropertyExpression.t
      | ObjectPropertyDomain of ObjectPropertyExpression.t * ClassExpression.t
      | ObjectPropertyRange of ObjectPropertyExpression.t * ClassExpression.t
      | FunctionalObjectProperty of ObjectPropertyExpression.t
      | InverseFunctionalObjectProperty of ObjectPropertyExpression.t
      | ReflexiveObjectProperty of ObjectPropertyExpression.t
      | IrreflexiveObjectProperty of ObjectPropertyExpression.t
      | SymmetricObjectProperty of ObjectPropertyExpression.t
      | AsymmetricObjectProperty of ObjectPropertyExpression.t
      | TransitiveObjectProperty of ObjectPropertyExpression.t
  end
  include (Common.S with type elt = Constructor.t)
end

(**================= Data Property Axioms ================**)

module DataPropertyAxiom : sig
  module Constructor : sig
    type t =
      | SubDataPropertyOf of DataPropertyExpression.t * DataPropertyExpression.t
      | EquivalentDataProperties of DataPropertyExpression.t Cset.t
      | DisjointDataProperties of DataPropertyExpression.t Cset.t
      | DataPropertyDomain of DataPropertyExpression.t * ClassExpression.t
      | DataPropertyRange of DataPropertyExpression.t * DataRange.t
      | FunctionalDataProperty of DataPropertyExpression.t
  end
  include (Common.S with type elt = Constructor.t)
end

(**================= Datatype Definitions =================**)

module DatatypeDefinition : sig
  module Constructor : sig
    type t =
      | DatatypeDefinition of Datatype.t * DataRange.t
  end
  include (Common.S with type elt = Constructor.t)
end

(**========================= Keys =========================**)

module Key : sig
  module Constructor : sig
    type t =
      | HasKey of ClassExpression.t * ObjectPropertyExpression.t Cset.t * DataPropertyExpression.t Cset.t
  end
  include (Common.S with type elt = Constructor.t)
end

(**====================== Assertions ======================**)

module Assertion : sig
  module Constructor : sig
    type t =
      | SameIndividual of Individual.t Cset.t
      | DifferentIndividuals of Individual.t Cset.t
      | ClassAssertion of ClassExpression.t * Individual.t
      | ObjectPropertyAssertion of ObjectPropertyExpression.t * Individual.t * Individual.t
      | NegativeObjectPropertyAssertion of ObjectPropertyExpression.t * Individual.t * Individual.t
      | DataPropertyAssertion of DataPropertyExpression.t * Individual.t * Literal.t
      | NegativeDataPropertyAssertion of DataPropertyExpression.t * Individual.t * Literal.t
  end
  include (Common.S with type elt = Constructor.t)
end

(**================= Annotation Subjects ==================**)

module AnnotationSubject : sig
  module Constructor : sig
    type t =
      | IRI of string
      | AnonymousIndividual of string
  end
  include (Common.S with type elt = Constructor.t)
end

(**================== Annotation Values ===================**)

module AnnotationValue : sig
  module Constructor : sig
    type t =
      | AnonymousIndividual of string
      | IRI of string
      | Literal of Literal.t
  end
  include (Common.S with type elt = Constructor.t)
end

(**===================== Annotations ======================**)

module Annotation : sig
  module Constructor : sig
    type t =
      | Annotation of t consed list * AnnotationProperty.t * AnnotationValue.t
  end
  include (Common.S with type elt = Constructor.t)
end

(**================== Annotation Axioms ===================**)

module AnnotationAxiom : sig
  module Constructor : sig
    type t =
      | AnnotationAssertion of t consed list * AnnotationProperty.t * AnnotationSubject.t * AnnotationValue.t
      | SubAnnotationPropertyOf of t consed list * AnnotationProperty.t * AnnotationProperty.t
      | AnnotationPropertyDomain of t consed list * AnnotationProperty.t * string
      | AnnotationPropertyRange of t consed list * AnnotationProperty.t * string
  end
  include (Common.S with type elt = Constructor.t)
end
