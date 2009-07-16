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

(**================= Object Properties ==================**)

module ObjectProperty : sig
  module Constructor : sig
    type t =
      | ObjectPropertyIRI of string
      | TopObjectProperty
      | BottomObjectProperty
  end
  include Common.S with type elt = Constructor.t
end

(**====================== Classes =======================**)

module Class : sig
  module Constructor : sig
    type t =
      | ClassIRI of string
      | Thing
      | Nothing
  end
  include Common.S with type elt = Constructor.t
end

(**==================== Individuals =====================**)

module Individual : sig
  module Constructor : sig
    type t =
      | IndividualIRI of string
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
      | DataSomeValuesFrom
      | DataAllValuesFrom
      | DataHasValue
      | DataMinCardinality
      | DataMaxCardinality
      | DataExactCardinality
  end
  include (Common.S with type elt = Constructor.t)
end

(**================= Object Property Axioms ================**)

module ObjectPropertyAxiom : sig
  module Constructor : sig
    type t =
      | SubObjectPropertyOf of ObjectPropertyExpression.t list * ObjectPropertyExpression.t
      | EquivalentObjectProperties of ObjectPropertyExpression.t Cset.t
      | DisjointObjectProperties
      | InverseObjectProperties of ObjectPropertyExpression.t * ObjectPropertyExpression.t
      | ObjectPropertyDomain
      | ObjectPropertyRange
      | FunctionalObjectProperty of ObjectPropertyExpression.t
      | InverseFunctionalObjectProperty of ObjectPropertyExpression.t
      | ReflexiveObjectProperty
      | IrreflexiveObjectProperty
      | SymmetricObjectProperty
      | AsymmetricObjectProperty
      | TransitiveObjectProperty of ObjectPropertyExpression.t
  end
  include (Common.S with type elt = Constructor.t)
end

(**================= Class ExpressionAxioms ================**)

module ClassExpressionAxiom : sig
  module Constructor : sig
    type t =
        SubClassOf of ClassExpression.t * ClassExpression.t
      | EquivalentClasses of ClassExpression.Set.t
      | DisjointClasses
      | DisjointUnion
  end
  include (Common.S with type elt = Constructor.t)
end

(**====================== Assertions ======================**)

module Assertion : sig
  module Constructor : sig
    type t =
      | SameIndividual
      | DifferentIndividuals
      | ClassAssertion of ClassExpression.t * Individual.t
      | ObjectPropertyAssertion of ObjectPropertyExpression.t * Individual.t * Individual.t
      | NegativeObjectPropertyAssertion
      | DataPropertyAssertion
      | NegativeDataPropertyAssertion
  end
  include (Common.S with type elt = Constructor.t)
end