(* Modules for manipulating with OWL 2 objects such as classes, class      *)
(* expressions and axioms; uses camlp4 preprocessor to generate hash,      *)
(* equality and comparison functions                                       *)

open Consed

(* auxiliary function for computing hash multipliers *)
let hm i = max_int - i

(**================= Common Interfaces ==================**)

module type OrderedHashedType = sig
    type t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
end

(* extending the type with useful datastructures *)
module CommonExtended = struct
	module type S = sig
		include OrderedHashedType
		module Set : Set.S with type elt = t
		module Map : Map.S with type key = t
		module HSet : Hashsetlp.S with type key = t and type elt = t
		module HMap : Hashtbllp.S with type key = t
	end
		
	module Make(H: OrderedHashedType): (S with type t = H.t) = struct
		include H
		module Set = Set.Make (H)
		module Map = Map.Make (H)
		module HSet = Hashsetlp.Make (struct
				include H
				type elt = H.t
				let key x = x
			end)
		module HMap = Hashtbllp.Make (H)
	end
end

(* extending the type to a consed type *)
module CommonConsed = struct
	module type S = sig
		include Consed.S
		module Set : Cset.S with type elt = t
		module Map : Cmap.S with type key = t and module Set = Set
		module HSet : Chashsetlp.S with type key = t
		module HMap : Chashtbllp.S with type key = t
	end
	
	module Make(H: HashedType): (S with type key = H.t) = struct		
		include Consed.Make (H)		
		module Hashtbl = Chashmap.Make (H)
		module Set = Cset.Make (H)
		module Map = Cmap.Make (H)
		module HSet = Chashsetlp.Make (H)
		module HMap = Chashtbllp.Make (H)
	end
end

(** hash function for list of integers *)

(* Jenkins hash function: works but slightly slower *)
let jenkins_hash lst =
	let h = ref 0 in
	List.iter (fun x ->
					h := !h + x;
					h := !h + (!h lsl 10);
					h := !h lxor (!h lsr 6);
		) lst;
	h := !h + (!h lsl 3);
	h := !h lxor (!h lsr 11);
	h := !h + (!h lsl 15);
	!h

let rec list_hash accu i = function
	| [] -> accu
	| x :: tl -> list_hash (accu + (max_int - i) * x) (succ i) tl 
let list_hash lst = list_hash 1 1 lst

let rec list_hash accu = function
	| [] -> accu
	| x :: tl -> list_hash (accu + x) tl 
let list_hash lst = list_hash 1 lst

(**================= Auxiliary Modules ==================**)

module O_string = struct
	type t = string
	let compare = String.compare
	let equal = (=)
	let hash = Hashtbl.hash
end

module O_int = struct
	type t = int
	let compare x y = x - y
	let equal = (=)
	let hash = Hashtbl.hash
end
  
module O_list = struct
	type 'a t = 'a list
	let hash hf t = list_hash (List.map hf t)
	let rec equal eq s t = match s, t with
		| x :: s, y :: t -> eq x y && equal eq s t
    | [], [] -> true
		| _ -> false
	let rec compare cmp s t = match s, t with
		| x :: s, y :: t -> let c = cmp x y in if c <> 0 then c else compare cmp s t
		| [], [] -> 0
		| [], _ -> - 1
		| _ -> 1 
end	

module O_array = struct
	type 'a t = 'a array
	let rec hash_list hf t accu i =
		if i < 0 then accu
		else hash_list hf t ((hf t[i]) :: accu) (pred i)
	let hash hf t = list_hash (hash_list hf t [] (pred (Array.length t)))
	let rec equal eq s t l i =
		i == l || (eq s[i] t[i] && equal eq s t l (succ i))
	let equal eq s t =
		let l = Array.length s in
		(l == Array.length t && equal eq s t l 0)
	let rec compare cmp s t l i =
		if (i == l) then 0
		else let c = cmp s[i] t[i] in
			if c <> 0 then c else compare cmp s t l (succ i)
	let compare cmp s t =
		let l = Array.length s in
		let c = (l - Array.length t) in
		if c <> 0 then c else compare cmp s t l 0
end

module O_option = struct
	type 'a t = 'a option
	let hash hf = function
		| Some h -> list_hash [1; hf h]
		| None -> list_hash [2]
	let equal eq s t = match s, t with
		| Some s, Some t -> eq s t
		| None, None -> true
		| _ -> false
	let compare cmp s t = match s, t with
		| Some s, Some t -> cmp s t
		| Some _, _ -> - 1
		| _, Some _ -> 1
		| None, None -> 0
end
			
(**======================== IRIs ========================**)

module rec IRI_Constructor : sig
  save
 	| IRI of O_string
  make_ohtype
	val str_of : t -> string
end = struct 
  make_type
  make_compare
  make_equal
  make_hash list_hash 
  let str_of = function
    | IRI st -> st
	let init_size () = 127
end 

and IRI : sig	
	include CommonConsed.S with type key = IRI_Constructor.t
	val str_of : t -> string
end = struct 
	include CommonConsed.Make (IRI_Constructor)
	let str_of id = IRI_Constructor.str_of id.data
end

(**====================== NodeIDs =======================**)

and NodeID_Constructor : sig	
  save
   | NodeID of O_string
  make_ohtype
	val str_of : t -> string
	end = struct 
  make_type
  make_compare
  make_equal
  make_hash list_hash
	let str_of = function
    | NodeID st -> st
  let init_size () = 13
end 

and NodeID : sig	
	include CommonConsed.S with type key = NodeID_Constructor.t
	val str_of : t -> string
end = struct 
	include CommonConsed.Make (NodeID_Constructor)
	let str_of id = NodeID_Constructor.str_of id.data
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
  make_ohtype
	val str_of : t -> string
	end = struct
			make_type
			make_compare
			make_equal
		  make_hash list_hash
		let str_of = function
			| IRI iri -> IRI.str_of iri
			| Rdfs_Literal -> "rdfs:Literal"
			| Owl_real -> "owl:real"
			| Owl_rational -> "owl:rational"
			| Xsd_decimal -> "owl:decimal"
			| Xsd_integer -> "xsd:integer"
			| Xsd_nonNegativeInteger -> "xsd:nonNegativeInteger"
			| Xsd_nonPositiveInteger -> "xsd:nonPositiveInteger"
			| Xsd_positiveInteger -> "xsd:positiveInteger"
			| Xsd_negativeInteger -> "xsd:negativeInteger"
			| Xsd_long -> "xsd:long"
			| Xsd_int -> "xsd:int"
			| Xsd_short -> "xsd:short"
			| Xsd_byte -> "xsd:byte"
			| Xsd_unsignedLong -> "xsd:unsignedLong"
			| Xsd_unsignedInt -> "xsd:unsignedInt"
			| Xsd_unsignedShort -> "xsd:unsignedShort"
			| Xsd_unsignedByte -> "xsd:unsignedByte"
			| Xsd_double -> "xsd:double"
			| Xsd_float -> "xsd:float"
			| Rdf_PlainLiteral -> "xsd:PlainLiteral"
			| Xsd_string -> "xsd:string"
			| Xsd_normalizedString -> "xsd:normalizedString"
			| Xsd_token -> "xsd:token"
			| Xsd_language -> "xsd:language"
			| Xsd_Name -> "xsd:Name"
			| Xsd_NCName -> "xsd:NCName"
			| Xsd_NMTOKEN -> "xsd:NMTOKEN"
			| Xsd_boolean -> "xsd:boolean"
			| Xsd_hexBinary -> "xsd:hexBinary"
			| Xsd_base64Binary -> "xsd:base64Binary"
			| Xsd_anyURI -> "xsd:anyURI"
			| Xsd_dateTime -> "xsd:dateTime"
			| Xsd_dateTimeStamp -> "xsd:dateTimeStamp"
			| Rdf_XMLLiteral -> "rdf:XMLLiteral"
    let init_size () = 13
end

and Datatype : sig 
	include CommonExtended.S with type t = Datatype_Constructor.t
	val str_of : t -> string	
end = struct 
	include CommonExtended.Make(Datatype_Constructor)
	let str_of = Datatype_Constructor.str_of
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
		make_ohtype
		val str_of : t -> string
	end = struct 
		make_type
		make_compare
		make_equal
		make_hash list_hash 
		let str_of = function
			| IRI iri -> IRI.str_of iri
			| Xsd_minInclusive -> "xsd:minInclusive"
			| Xsd_maxInclusive -> "xsd:maxInclusive"
			| Xsd_minExclusive -> "xsd:minExclusive"
			| Xsd_maxExclusive -> "xsd:maxExclusive"
			| Xsd_length -> "xsd:length"
			| Xsd_minLength -> "xsd:minLength"
			| Xsd_maxLength -> "xsd:maxLength"
			| Xsd_pattern -> "xsd:pattern"
			| Rdf_langRange -> "rdf:langRange"
    let init_size () = 13
end

and ConstrainingFacet : sig	
	include CommonExtended.S with type t = ConstrainingFacet_Constructor.t
	val str_of : t -> string
end = struct 
	include CommonExtended.Make(ConstrainingFacet_Constructor)
	let str_of = ConstrainingFacet_Constructor.str_of
end

(**================= Object Properties ==================**)

and ObjectProperty_Constructor : sig
		save
			| IRI of IRI
			| TopObjectProperty
			| BottomObjectProperty
		make_ohtype
		val str_of : t -> string
	end = struct 
    make_type
		make_compare
		make_equal
		make_hash list_hash
		let str_of = function
			| IRI iri -> IRI.str_of iri
			| TopObjectProperty -> "owl:topObjectProperty"
			| BottomObjectProperty -> "owl:bottomObjectProperty"
    let init_size () = 13
end
	 
and ObjectProperty : sig
  include CommonConsed.S with type key = ObjectProperty_Constructor.t
  val str_of : t -> string
end = struct	
	include CommonConsed.Make (ObjectProperty_Constructor)
	let str_of op = ObjectProperty_Constructor.str_of op.data
end

(**=================== Data Properties ==================**)

and DataProperty_Constructor : sig
		save
			| IRI of IRI
			| TopDataProperty
			| BottomDataProperty
		make_ohtype
		val str_of : t -> string
	end = struct 
		make_type
		make_compare
		make_equal
		make_hash list_hash
		let str_of = function
			| IRI iri -> IRI.str_of iri
			| TopDataProperty -> "owl:topDataProperty"
			| BottomDataProperty -> "owl:bottomDataProperty"
    let init_size () = 13
end

and DataProperty : sig 
	include CommonExtended.S with type t = DataProperty_Constructor.t
	val str_of : t -> string
end = struct	
	include CommonExtended.Make(DataProperty_Constructor)
	let str_of = DataProperty_Constructor.str_of
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
		make_ohtype
		val str_of : t -> string
	end = struct 
		make_type
		make_compare
		make_equal
		make_hash list_hash 
		let str_of = function
			| IRI iri -> IRI.str_of iri
			| Rdfs_label -> "rdfs:label"
			| Rdfs_comment -> "rdfs:comment"
			| Rdfs_seeAlso -> "rdfs:seeAlso"
			| Rdfs_isDefinedBy -> "rdfs:isDefinedBy"
			| Owl_deprecated -> "owl:deprecated"
			| Owl_versionInfo -> "owl:versionInfo"
			| Owl_priorVersion -> "owl:priorVersion"
			| Owl_backwardCompatibleWith -> "owl:backwardCompatibleWith"
			| Owl_incompatibleWith -> "owl:incompatibleWith"
    let init_size () = 13
end 
	
and AnnotationProperty : sig	
	include CommonExtended.S with type t = AnnotationProperty_Constructor.t
	val str_of : t -> string
end = struct
	include CommonExtended.Make(AnnotationProperty_Constructor)
	let str_of = AnnotationProperty_Constructor.str_of
end

(**====================== Classes =======================**)

and Class_Constructor : sig
		save
			| IRI of IRI
			| Thing
			| Nothing
		make_ohtype
		val str_of : t -> string
	end = struct 
		make_type
		make_compare
		make_equal
		make_hash list_hash
		let str_of = function
			| IRI iri -> IRI.str_of iri
			| Thing -> "owl:Thing"
			| Nothing -> "owl:Nothing"
    let init_size () = 127
end

and Class : sig 
	include CommonExtended.S with type t = Class_Constructor.t
	val str_of : t -> string
end = struct
	include CommonExtended.Make(Class_Constructor)
	let str_of = Class_Constructor.str_of
end

(**==================== Individuals =====================**)

and Individual_Constructor : sig
		save
			| NamedIndividual of IRI
			| AnonymousIndividual of NodeID
		make_ohtype
		val str_of : t -> string
	end = struct 
		make_type
		make_compare
		make_equal
		make_hash list_hash
		let str_of = function
			| NamedIndividual iri -> IRI.str_of iri
			| AnonymousIndividual id -> NodeID.str_of id
    let init_size () = 13
end

and Individual : sig
	include CommonExtended.S with type t = Individual_Constructor.t
	val str_of : t -> string
end = struct
	include CommonExtended.Make(Individual_Constructor)
	let str_of = Individual_Constructor.str_of
end

(**======================= Literals =====================**)

and Literal_Constructor : sig
		save
			| TypedLiteral of O_string * Datatype
			| StringLiteralNoLanguage of O_string
			| StringLiteralWithLanguage of O_string * O_string
		make_ohtype
	end = struct 
		make_type
		make_compare
		make_equal
		make_hash list_hash 
    let init_size () = 13
end

and Literal : CommonConsed.S with type key = Literal_Constructor.t
= CommonConsed.Make (Literal_Constructor)

(**============ Object Property Expressions =============**)

and ObjectPropertyExpression_Constructor : sig
		save
			| ObjectProperty of ObjectProperty
			| ObjectInverseOf of ObjectProperty
		make_ohtype
	end = struct
		make_type
		make_compare
		make_equal
		make_hash list_hash 
    let init_size () = 13
end

and ObjectPropertyExpression : CommonConsed.S with type key 
= ObjectPropertyExpression_Constructor.t
= CommonConsed.Make (ObjectPropertyExpression_Constructor)

(**============= Data Property Expressions ==============**)

and DataPropertyExpression_Constructor : sig
		save
			| DataProperty of DataProperty
		make_ohtype
	end = struct 
		make_type
		make_compare
		make_equal
		make_hash list_hash
    let init_size () = 13
end

and DataPropertyExpression : CommonConsed.S with type key = DataPropertyExpression_Constructor.t
= CommonConsed.Make (DataPropertyExpression_Constructor)

(**==================== Data Ranges =====================**)

and DataRange_Constructor : sig
		save
			| Datatype of Datatype
			| DataIntersectionOf of DataRange O_list
			| DataUnionOf of DataRange O_list
			| DataComplementOf of DataRange
			| DataOneOf of Literal O_list
			| DatatypeRestriction of Datatype * ((ConstrainingFacet * Literal) O_list)
		make_ohtype 
	end = struct 
	  make_type
		make_equal
		make_compare
		make_hash list_hash
end

and DataRange : CommonConsed.S with type key = DataRange_Constructor.t 
= CommonConsed.Make (DataRange_Constructor)

(**=================== Class Expressions ================**)

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
    make_ohtype 
	end = struct 
	  make_type
		make_compare
		make_equal
		make_hash list_hash
    let init_size () = 127
end

and ClassExpression : CommonConsed.S with type key = ClassExpression_Constructor.t 
= CommonConsed.Make (ClassExpression_Constructor)

(**================= Class Expression Axioms ===============**)

and ClassAxiom_Constructor : sig
		save
			| SubClassOf of ClassExpression * ClassExpression
			| EquivalentClasses of ClassExpression O_list
			| DisjointClasses of ClassExpression O_list
			| DisjointUnion of Class * ClassExpression O_list
		make_ohtype
	end = struct
	  make_type
		make_compare
		make_equal
		make_hash list_hash
end

and ClassAxiom : CommonConsed.S with type key = ClassAxiom_Constructor.t
= CommonConsed.Make (ClassAxiom_Constructor)

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
		make_ohtype
	end = struct
	  make_type
		make_compare
		make_equal
		make_hash list_hash   
end

and ObjectPropertyAxiom : CommonConsed.S with type key = ObjectPropertyAxiom_Constructor.t
= CommonConsed.Make (ObjectPropertyAxiom_Constructor)

(**================= Data Property Axioms ================**)

and DataPropertyAxiom_Constructor : sig
		save
			| SubDataPropertyOf of DataPropertyExpression * DataPropertyExpression
			| EquivalentDataProperties of DataPropertyExpression O_list
			| DisjointDataProperties of DataPropertyExpression O_list
			| DataPropertyDomain of DataPropertyExpression * ClassExpression
			| DataPropertyRange of DataPropertyExpression * DataRange
			| FunctionalDataProperty of DataPropertyExpression
		make_ohtype
	end = struct
	  make_type
		make_compare
		make_equal
		make_hash list_hash
end 

and DataPropertyAxiom : CommonConsed.S with type key = DataPropertyAxiom_Constructor.t
= CommonConsed.Make (DataPropertyAxiom_Constructor)

(**================= Datatype Definitions =================**)

and DatatypeDefinition_Constructor : sig
    save
			| DatatypeDefinition of Datatype * DataRange
    make_ohtype
	end = struct
	  make_type
		make_compare
		make_equal
		make_hash list_hash
end

and DatatypeDefinition : CommonConsed.S with type key = DatatypeDefinition_Constructor.t
= CommonConsed.Make (DatatypeDefinition_Constructor)

(**========================= Keys =========================**)

and Key_Constructor : sig
		save
			| HasKey of ClassExpression * ObjectPropertyExpression O_list * DataPropertyExpression O_list
		make_ohtype
	end = struct
	  make_type
		make_compare
		make_equal
		make_hash list_hash
end

and Key : CommonConsed.S with type key = Key_Constructor.t
= CommonConsed.Make (Key_Constructor)

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
		make_ohtype
	end = struct
	  make_type
		make_compare
		make_equal
		make_hash list_hash
end

and Assertion : CommonConsed.S with type key = Assertion_Constructor.t
= CommonConsed.Make (Assertion_Constructor)

(**================= Annotation Subjects ==================**)

and AnnotationSubject_Constructor : sig
		save
			| IRI of IRI
			| AnonymousIndividual of NodeID
		make_ohtype
	end = struct
	  make_type
		make_compare
		make_equal
		make_hash list_hash
end

and AnnotationSubject : CommonConsed.S with type key = AnnotationSubject_Constructor.t
= CommonConsed.Make (AnnotationSubject_Constructor)

(**================== Annotation Values ===================**)

and AnnotationValue_Constructor : sig
		save
			| AnonymousIndividual of NodeID
			| IRI of IRI
			| Literal of Literal
		make_ohtype
	end = struct
	  make_type
		make_compare
		make_equal
		make_hash list_hash
end

and AnnotationValue : CommonConsed.S with type key = AnnotationValue_Constructor.t
= CommonConsed.Make (AnnotationValue_Constructor)

(**===================== Annotations ======================**)

and Annotation_Constructor : sig
		save
			| Annotation of Annotation O_list * AnnotationProperty * AnnotationValue
		make_ohtype
	end = struct
	  make_type
		make_compare
		make_equal
		make_hash list_hash
end

and Annotation : CommonConsed.S with type key = Annotation_Constructor.t
= CommonConsed.Make (Annotation_Constructor)

(**================== Annotation Axioms ===================**)

and AnnotationAxiom_Constructor : sig
		save
			| AnnotationAssertion of Annotation O_list * AnnotationProperty * AnnotationSubject * AnnotationValue
			| SubAnnotationPropertyOf of Annotation O_list * AnnotationProperty * AnnotationProperty
			| AnnotationPropertyDomain of Annotation O_list * AnnotationProperty * IRI
			| AnnotationPropertyRange of Annotation O_list * AnnotationProperty * IRI
		make_ohtype
	end = struct
	  make_type
		make_compare
		make_equal
		make_hash list_hash
end

and AnnotationAxiom : CommonConsed.S with type key = AnnotationAxiom_Constructor.t
= CommonConsed.Make (AnnotationAxiom_Constructor)
