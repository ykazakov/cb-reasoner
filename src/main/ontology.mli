open Owl

module type OrderedMapSet = sig
    type t
    module Map : Map.S with type key = t
    module Set : Set.S with type elt = t
end

module OrderedClass : OrderedMapSet with type t = Class.t
module OrderedDatatype : OrderedMapSet with type t = Datatype.t
module OrderedConstrainingFacet : OrderedMapSet with type t = ConstrainingFacet.t
module OrderedObjectProperty : OrderedMapSet with type t = ObjectProperty.t
module OrderedDataProperty : OrderedMapSet with type t = DataProperty.t
module OrderedIndividual : OrderedMapSet with type t = Individual.t
module OrderedLiteral : OrderedMapSet with type t = Literal.t

type t
val create : unit -> t

(**====================== iterators ========================**)

val iter_record_Class : (Class.t -> Polarity.Counter.t -> unit) -> t -> unit
val iter_record_Datatype : (Datatype.t -> Polarity.Counter.t -> unit) -> t -> unit
val iter_record_ConstrainingFacet : (ConstrainingFacet.t -> Polarity.Counter.t -> unit) -> t -> unit
val iter_record_ObjectProperty : (ObjectProperty.t -> Polarity.Counter.t -> unit) -> t -> unit
val iter_record_DataProperty : (DataProperty.t -> Polarity.Counter.t -> unit) -> t -> unit
val iter_record_Individual : (Individual.t -> Polarity.Counter.t -> unit) -> t -> unit

val iter_record_ObjectPropertyExpression : (ObjectPropertyExpression.t -> Polarity.Counter.t -> unit) -> t -> unit
val iter_record_ClassExpression : (ClassExpression.t -> Polarity.Counter.t -> unit) -> t -> unit

val iter_record_Declaration : (Declaration.t -> int -> unit) -> t -> unit
val iter_record_ObjectPropertyAxiom : (ObjectPropertyAxiom.t -> int -> unit) -> t -> unit
val iter_record_ClassAxiom : (ClassAxiom.t -> int -> unit) -> t -> unit
val iter_record_Assertion : (Assertion.t -> int -> unit) -> t -> unit

(**============== statistical information ================**)

val count_Class : t -> int
val count_Datatype : t -> int
val count_ConstrainingFacet : t -> int
val count_ObjectProperty : t -> int
val count_DataProperty : t -> int
val count_Individual : t -> int

val count_ObjectPropertyExpression : t -> int
val count_ClassExpression : t -> int

val count_ObjectPropertyAxiom : t -> int
val count_ClassAxiom : t -> int
val count_Assertion : t -> int

val has_positive_Nothing : t -> bool
val has_negative_Thing : t -> bool
val has_positive_ObjectComplementOf : t -> bool

(**========== insertion and deletion of axioms ==============**)

val add_Declaration : t -> Declaration.t -> unit
val remove_Declaration : t -> Declaration.t -> unit
val add_ObjectPropertyAxiom : t -> ObjectPropertyAxiom.t -> unit
val remove_ObjectPropertyAxiom : t -> ObjectPropertyAxiom.t -> unit
val add_ClassAxiom : t -> ClassAxiom.t -> unit
val remove_ClassAxiom : t -> ClassAxiom.t -> unit
val add_Assertion : t -> Assertion.t -> unit
val remove_Assertion : t -> Assertion.t -> unit

(**=============== printing of statistics ===================**)
type info_type =
    | All            (* about all OWL objects *)
    | Used           (* about those used in the ontology *)
    | Unsupported    (* about unsupported about unsupported *)

val print_info : ?info_type: info_type -> t -> out_channel -> unit
