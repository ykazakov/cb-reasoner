(* A module defining the constructors for concepts, roles, and axioms of   *)
(* ontologies                                                              *)

open CommonTypes
open Consed.T

(* auxiliary function for computing hash multipliers *)
let hm i = max_int - i

(**================= Common Interfaces ==================**)

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
    type elt
    include OrderedHashedType with type t = elt consed        
    module Cons : Consed.S with type key = elt
    val cons : Cons.t -> elt -> t
    module Set : Cset.S with type elt = t
    module Map : Cmap.S with type key = t and module Set = Set
    module HSet : Chashsetlp.S with type key = t
    module HMap : Chashtbllp.S with type key = t
  end
  
  module Make(H: OrderedHashedType): (S with type elt = H.t) = struct
    type elt = H.t        
    type t = H.t consed
    include Consed.OHT
    module Cons = Consed.Make (H)
    let cons = Cons.cons
    module Hashtbl = Chashmap.Make (H)
    module Set = Cset.Make (H)
    module Map = Cmap.Make (H)
    module HSet = Chashsetlp.Make (H)
    module HMap = Chashtbllp.Make (H)
  end
end

(* auxiliary functions on conced values *)
module C = struct
  (** comparison functions *)
  let compare2 cmp1 cmp2 x1 y1 x2 y2 =
    let c = cmp1 x1 x2 in if c <> 0 then c else cmp2 y1 y2
  let compare3 cmp1 cmp2 cmp3 x1 y1 z1 x2 y2 z2 =
    let c = compare2 cmp1 cmp2 x1 y1 x2 y2 in
    if c <> 0 then c else cmp3 z1 z2
  let compare4 cmp1 cmp2 cmp3 cmp4 x1 y1 z1 u1 x2 y2 z2 u2 =
    let c = compare2 cmp1 cmp2 x1 y1 x2 y2 in
    if c <> 0 then c else compare2 cmp3 cmp4 z1 u1 z2 u2
  let rec compare_lst cmp lst1 lst2 =
    match lst1, lst2 with
    | hd1 :: tl1, hd2 :: tl2 ->
        let c = cmp hd1 hd2 in
        if c <> 0 then c else compare_lst cmp tl1 tl2
    | [], [] -> 0
    | [], _ -> - 1
    | _ -> 1
  let compare_op cmp o1 o2 =
    match o1, o2 with
    | None, None -> 0
    | Some y1, Some y2 -> cmp y1 y2
    | None, _ -> - 1
    | _, None -> 1
  let compare_n2o cmp1 cmp2 n1 x1 o1 n2 x2 o2 =
    compare3 (-) (cmp1) (compare_op cmp2) n1 x1 o1 n2 x2 o2
  (** equality fucntions *)
  let equal2 eq1 eq2 x1 y1 x2 y2 = (eq1 x1 x2) && (eq2 y1 y2)
  let equal3 eq1 eq2 eq3 x1 y1 z1 x2 y2 z2 =
    (eq1 x1 x2) && (eq2 y1 y2) && (eq3 z1 z2)
  let equal4 eq1 eq2 eq3 eq4 x1 y1 z1 u1 x2 y2 z2 u2 =
    (eq1 x1 x2) && (eq2 y1 y2) && (eq3 z1 z2) && (eq4 u1 u2)
  let equal_lst eq lst1 lst2 =
    try List.for_all2 (eq) lst1 lst2
    with Invalid_argument _ -> false
  let equal_op eq o1 o2 =
    match o1, o2 with
    | None, None -> true
    | Some y1, Some y2 -> eq y1 y2
    | _ -> false
  let equal_n2o cmp1 cmp2 n1 x1 o1 n2 x2 o2 =
    equal3 (=) (cmp1) (equal_op cmp2) n1 x1 o1 n2 x2 o2
  (** hash functions *)
  let hash2 h1 h2 x y = (max_int - 1) * (h1 x) + (max_int - 2) * (h2 y)
  let hash3 h1 h2 h3 x y z =
    (max_int - 1) * (h1 x) + (max_int - 2) * (h2 y) + (max_int - 3) * (h3 z)
  let hash4 h1 h2 h3 h4 x y z u =
    (max_int - 1) * (h1 x) + (max_int - 2) * (h2 y) +
    (max_int - 3) * (h3 z) + (max_int - 4) * (h4 u)
  let hash_lst h lst =
    let rec hash_lst_aux i = function
      | [] -> 0
      | hd :: tl -> (max_int - i) * (h hd) + hash_lst_aux (i + 1) tl
    in hash_lst_aux 0 lst
  let hash_op h = function
    | None -> (max_int - 3)
    | Some y -> (max_int - 4) * (h y)
  let hash_n2o h1 h2 n x o =
    hash3 (fun n -> n) h1 (hash_op h2) n x o
end

(**======================== IRIs ========================**)

module IRI = struct
  module Constructor = struct
    type t =
      | IRI of string
    let compare id1 id2 =
      match id1, id2 with
      | IRI st1, IRI st2 -> String.compare st1 st2
    let equal id1 id2 =
      match id1, id2 with
      | IRI st1, IRI st2 -> st1 = st2
    let hash = function
      | IRI st -> Hashtbl.hash st
    let str_of = function
      | IRI st -> st
  end
  include CommonConsed.Make (Constructor)
  let str_of id = Constructor.str_of id.data
end;;

(**====================== NodeIDs =======================**)

module NodeID = struct
  module Constructor = struct
    type t =
      | NodeID of string
    let compare id1 id2 =
      match id1, id2 with
      | NodeID st1, NodeID st2 -> String.compare st1 st2
    let equal id1 id2 =
      match id1, id2 with
      | NodeID st1, NodeID st2 -> st1 = st2
    let hash = function
      | NodeID st -> Hashtbl.hash st
    let str_of = function
      | NodeID st -> st
  end
  include CommonConsed.Make (Constructor)
  let str_of id = Constructor.str_of id.data
end;;

(**====================== Datatypes =====================**)

module Datatype = struct
  module Constructor = struct
    type t =
      | IRI of IRI.t
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
    let precedence = function
      | IRI _ -> 0
      | Rdfs_Literal -> 1
      | Owl_real -> 2
      | Owl_rational -> 3
      | Xsd_decimal -> 4
      | Xsd_integer -> 5
      | Xsd_nonNegativeInteger -> 6
      | Xsd_nonPositiveInteger -> 7
      | Xsd_positiveInteger -> 8
      | Xsd_negativeInteger -> 9
      | Xsd_long -> 10
      | Xsd_int -> 11
      | Xsd_short -> 12
      | Xsd_byte -> 13
      | Xsd_unsignedLong -> 14
      | Xsd_unsignedInt -> 15
      | Xsd_unsignedShort -> 16
      | Xsd_unsignedByte -> 17
      | Xsd_double -> 18
      | Xsd_float -> 19
      | Rdf_PlainLiteral -> 20
      | Xsd_string -> 21
      | Xsd_normalizedString -> 22
      | Xsd_token -> 23
      | Xsd_language -> 24
      | Xsd_Name -> 25
      | Xsd_NCName -> 26
      | Xsd_NMTOKEN -> 27
      | Xsd_boolean -> 28
      | Xsd_hexBinary -> 29
      | Xsd_base64Binary -> 30
      | Xsd_anyURI -> 31
      | Xsd_dateTime -> 32
      | Xsd_dateTimeStamp -> 33
      | Rdf_XMLLiteral -> 34
    let compare dt1 dt2 =
      let c = (precedence dt1) - (precedence dt2) in
      if c <> 0 then c else
        match dt1, dt2 with
        | IRI iri1, IRI iri2 -> IRI.compare iri1 iri2
        | Rdfs_Literal, Rdfs_Literal -> 0
        | Owl_real, Owl_real -> 0
        | Owl_rational, Owl_rational -> 0
        | Xsd_decimal, Xsd_decimal -> 0
        | Xsd_integer, Xsd_integer -> 0
        | Xsd_nonNegativeInteger, Xsd_nonNegativeInteger -> 0
        | Xsd_nonPositiveInteger, Xsd_nonPositiveInteger -> 0
        | Xsd_positiveInteger, Xsd_positiveInteger -> 0
        | Xsd_negativeInteger, Xsd_negativeInteger -> 0
        | Xsd_long, Xsd_long -> 0
        | Xsd_int, Xsd_int -> 0
        | Xsd_short, Xsd_short -> 0
        | Xsd_byte, Xsd_byte -> 0
        | Xsd_unsignedLong, Xsd_unsignedLong -> 0
        | Xsd_unsignedInt, Xsd_unsignedInt -> 0
        | Xsd_unsignedShort, Xsd_unsignedShort -> 0
        | Xsd_unsignedByte, Xsd_unsignedByte -> 0
        | Xsd_double, Xsd_double -> 0
        | Xsd_float, Xsd_float -> 0
        | Rdf_PlainLiteral, Rdf_PlainLiteral -> 0
        | Xsd_string, Xsd_string -> 0
        | Xsd_normalizedString, Xsd_normalizedString -> 0
        | Xsd_token, Xsd_token -> 0
        | Xsd_language, Xsd_language -> 0
        | Xsd_Name, Xsd_Name -> 0
        | Xsd_NCName, Xsd_NCName -> 0
        | Xsd_NMTOKEN, Xsd_NMTOKEN -> 0
        | Xsd_boolean, Xsd_boolean -> 0
        | Xsd_hexBinary, Xsd_hexBinary -> 0
        | Xsd_base64Binary, Xsd_base64Binary -> 0
        | Xsd_anyURI, Xsd_anyURI -> 0
        | Xsd_dateTime, Xsd_dateTime -> 0
        | Xsd_dateTimeStamp, Xsd_dateTimeStamp -> 0
        | Rdf_XMLLiteral, Rdf_XMLLiteral -> 0
        | _ -> invalid_arg "Datatype.compare"
    let equal dt1 dt2 =
      match dt1, dt2 with
      | IRI iri1, IRI iri2 -> IRI.equal iri1 iri2
      | Rdfs_Literal, Rdfs_Literal -> true
      | Owl_real, Owl_real -> true
      | Owl_rational, Owl_rational -> true
      | Xsd_decimal, Xsd_decimal -> true
      | Xsd_integer, Xsd_integer -> true
      | Xsd_nonNegativeInteger, Xsd_nonNegativeInteger -> true
      | Xsd_nonPositiveInteger, Xsd_nonPositiveInteger -> true
      | Xsd_positiveInteger, Xsd_positiveInteger -> true
      | Xsd_negativeInteger, Xsd_negativeInteger -> true
      | Xsd_long, Xsd_long -> true
      | Xsd_int, Xsd_int -> true
      | Xsd_short, Xsd_short -> true
      | Xsd_byte, Xsd_byte -> true
      | Xsd_unsignedLong, Xsd_unsignedLong -> true
      | Xsd_unsignedInt, Xsd_unsignedInt -> true
      | Xsd_unsignedShort, Xsd_unsignedShort -> true
      | Xsd_unsignedByte, Xsd_unsignedByte -> true
      | Xsd_double, Xsd_double -> true
      | Xsd_float, Xsd_float -> true
      | Rdf_PlainLiteral, Rdf_PlainLiteral -> true
      | Xsd_string, Xsd_string -> true
      | Xsd_normalizedString, Xsd_normalizedString -> true
      | Xsd_token, Xsd_token -> true
      | Xsd_language, Xsd_language -> true
      | Xsd_Name, Xsd_Name -> true
      | Xsd_NCName, Xsd_NCName -> true
      | Xsd_NMTOKEN, Xsd_NMTOKEN -> true
      | Xsd_boolean, Xsd_boolean -> true
      | Xsd_hexBinary, Xsd_hexBinary -> true
      | Xsd_base64Binary, Xsd_base64Binary -> true
      | Xsd_anyURI, Xsd_anyURI -> true
      | Xsd_dateTime, Xsd_dateTime -> true
      | Xsd_dateTimeStamp, Xsd_dateTimeStamp -> true
      | Rdf_XMLLiteral, Rdf_XMLLiteral -> true
      | _ -> false
    let hash = function
      | IRI iri -> (hm 1) * (IRI.hash iri)
      | Rdfs_Literal -> (hm 2)
      | Owl_real -> (hm 3)
      | Owl_rational -> (hm 4)
      | Xsd_decimal -> (hm 5)
      | Xsd_integer -> (hm 6)
      | Xsd_nonNegativeInteger -> (hm 7)
      | Xsd_nonPositiveInteger -> (hm 8)
      | Xsd_positiveInteger -> (hm 9)
      | Xsd_negativeInteger -> (hm 10)
      | Xsd_long -> (hm 11)
      | Xsd_int -> (hm 12)
      | Xsd_short -> (hm 13)
      | Xsd_byte -> (hm 14)
      | Xsd_unsignedLong -> (hm 15)
      | Xsd_unsignedInt -> (hm 16)
      | Xsd_unsignedShort -> (hm 17)
      | Xsd_unsignedByte -> (hm 18)
      | Xsd_double -> (hm 19)
      | Xsd_float -> (hm 20)
      | Rdf_PlainLiteral -> (hm 21)
      | Xsd_string -> (hm 22)
      | Xsd_normalizedString -> (hm 23)
      | Xsd_token -> (hm 24)
      | Xsd_language -> (hm 25)
      | Xsd_Name -> (hm 26)
      | Xsd_NCName -> (hm 27)
      | Xsd_NMTOKEN -> (hm 28)
      | Xsd_boolean -> (hm 29)
      | Xsd_hexBinary -> (hm 30)
      | Xsd_base64Binary -> (hm 31)
      | Xsd_anyURI -> (hm 32)
      | Xsd_dateTime -> (hm 33)
      | Xsd_dateTimeStamp -> (hm 34)
      | Rdf_XMLLiteral -> (hm 35)
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
  end
  include CommonExtended.Make(Constructor)
  let str_of = Constructor.str_of
end;;

(**================== Constraining Facets ===============**)

module ConstrainingFacet = struct
  module Constructor = struct
    type t =
      | IRI of IRI.t
      | Xsd_minInclusive
      | Xsd_maxInclusive
      | Xsd_minExclusive
      | Xsd_maxExclusive
      | Xsd_length
      | Xsd_minLength
      | Xsd_maxLength
      | Xsd_pattern
      | Rdf_langRange
    let precedence = function
      | IRI _ -> 0
      | Xsd_minInclusive -> 1
      | Xsd_maxInclusive -> 2
      | Xsd_minExclusive -> 3
      | Xsd_maxExclusive -> 4
      | Xsd_length -> 5
      | Xsd_minLength -> 6
      | Xsd_maxLength -> 7
      | Xsd_pattern -> 8
      | Rdf_langRange -> 9
    let compare cf1 cf2 =
      let c = (precedence cf1) - (precedence cf2) in
      if c <> 0 then c else
        match cf1, cf2 with
        | IRI iri1, IRI iri2 -> IRI.compare iri1 iri2
        | Xsd_minInclusive, Xsd_minInclusive -> 0
        | Xsd_maxInclusive, Xsd_maxInclusive -> 0
        | Xsd_minExclusive, Xsd_minExclusive -> 0
        | Xsd_maxExclusive, Xsd_maxExclusive -> 0
        | Xsd_length, Xsd_length -> 0
        | Xsd_minLength, Xsd_minLength -> 0
        | Xsd_maxLength, Xsd_maxLength -> 0
        | Xsd_pattern, Xsd_pattern -> 0
        | Rdf_langRange, Rdf_langRange -> 0
        | _ -> invalid_arg "ConstrainingFacet.compare"
    let equal cf1 cf2 =
      match cf1, cf2 with
      | IRI iri1, IRI iri2 -> IRI.equal iri1 iri2
      | Xsd_minInclusive, Xsd_minInclusive -> true
      | Xsd_maxInclusive, Xsd_maxInclusive -> true
      | Xsd_minExclusive, Xsd_minExclusive -> true
      | Xsd_maxExclusive, Xsd_maxExclusive -> true
      | Xsd_length, Xsd_length -> true
      | Xsd_minLength, Xsd_minLength -> true
      | Xsd_maxLength, Xsd_maxLength -> true
      | Xsd_pattern, Xsd_pattern -> true
      | Rdf_langRange, Rdf_langRange -> true
      | _ -> false
    let hash = function
      | IRI iri -> (hm 1) * (IRI.hash iri)
      | Xsd_minInclusive -> (hm 2)
      | Xsd_maxInclusive -> (hm 3)
      | Xsd_minExclusive -> (hm 4)
      | Xsd_maxExclusive -> (hm 5)
      | Xsd_length -> (hm 6)
      | Xsd_minLength -> (hm 7)
      | Xsd_maxLength -> (hm 8)
      | Xsd_pattern -> (hm 9)
      | Rdf_langRange -> (hm 10)
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
  end
  include CommonExtended.Make(Constructor)
  let str_of = Constructor.str_of
end;;

(**================= Object Properties ==================**)

module ObjectProperty = struct
  module Constructor = struct
    type t =
      | IRI of IRI.t
      | TopObjectProperty
      | BottomObjectProperty
    let precedence = function
      | IRI _ -> 0
      | TopObjectProperty -> 1
      | BottomObjectProperty -> 2
    let compare ope1 ope2 =
      let c = (precedence ope1) - (precedence ope2) in
      if c <> 0 then c else
        match ope1, ope2 with
        | IRI iri1, IRI iri2 -> IRI.compare iri1 iri2
        | TopObjectProperty, TopObjectProperty -> 0
        | BottomObjectProperty, BottomObjectProperty -> 0
        | _ -> invalid_arg "ObjectProperty.compare"
    let equal ope1 ope2 =
      match ope1, ope2 with
      | IRI iri1, IRI iri2 -> IRI.equal iri1 iri2
      | TopObjectProperty, TopObjectProperty -> true
      | BottomObjectProperty, BottomObjectProperty -> true
      | _ -> false
    let hash = function
      | IRI iri -> (hm 1) * (IRI.hash iri)
      | TopObjectProperty -> (hm 2)
      | BottomObjectProperty -> (hm 3)
    let str_of = function
      | IRI iri -> IRI.str_of iri
      | TopObjectProperty -> "owl:topObjectProperty"
      | BottomObjectProperty -> "owl:bottomObjectProperty"
  end
  include CommonConsed.Make (Constructor)
  let str_of op = Constructor.str_of op.data
end;;

(**=================== Data Properties ==================**)

module DataProperty = struct
  module Constructor = struct
    type t =
      | IRI of IRI.t
      | TopDataProperty
      | BottomDataProperty
    let precedence = function
      | IRI _ -> 0
      | TopDataProperty -> 1
      | BottomDataProperty -> 2
    let compare dp1 dp2 =
      let c = (precedence dp1) - (precedence dp2) in
      if c <> 0 then c else
        match dp1, dp2 with
        | IRI iri1, IRI iri2 -> IRI.compare iri1 iri2
        | TopDataProperty, TopDataProperty -> 0
        | BottomDataProperty, BottomDataProperty -> 0
        | _ -> invalid_arg "DataProperty.compare"
    let equal dp1 dp2 =
      match dp1, dp2 with
      | IRI iri1, IRI iri2 -> IRI.equal iri1 iri2
      | TopDataProperty, TopDataProperty -> true
      | BottomDataProperty, BottomDataProperty -> true
      | _ -> false
    let hash = function
      | IRI iri -> (hm 1) * (IRI.hash iri)
      | TopDataProperty -> (hm 2)
      | BottomDataProperty -> (hm 3)
    let str_of = function
      | IRI iri -> IRI.str_of iri
      | TopDataProperty -> "owl:topDataProperty"
      | BottomDataProperty -> "owl:bottomDataProperty"
  end
  include CommonExtended.Make(Constructor)
  let str_of = Constructor.str_of
end;;

(**================ Annotation Properties ===============**)

module AnnotationProperty = struct
  module Constructor = struct
    type t =
      | IRI of IRI.t
      | Rdfs_label
      | Rdfs_comment
      | Rdfs_seeAlso
      | Rdfs_isDefinedBy
      | Owl_deprecated
      | Owl_versionInfo
      | Owl_priorVersion
      | Owl_backwardCompatibleWith
      | Owl_incompatibleWith
    let precedence = function
      | IRI _ -> 0
      | Rdfs_label -> 1
      | Rdfs_comment -> 2
      | Rdfs_seeAlso -> 3
      | Rdfs_isDefinedBy -> 4
      | Owl_deprecated -> 5
      | Owl_versionInfo -> 6
      | Owl_priorVersion -> 7
      | Owl_backwardCompatibleWith -> 8
      | Owl_incompatibleWith -> 9
    let compare ap1 ap2 =
      let c = (precedence ap1) - (precedence ap2) in
      if c <> 0 then c else
        match ap1, ap2 with
        | IRI iri1, IRI iri2 -> IRI.compare iri1 iri2
        | Rdfs_label, Rdfs_label -> 0
        | Rdfs_comment, Rdfs_comment -> 0
        | Rdfs_seeAlso, Rdfs_seeAlso -> 0
        | Rdfs_isDefinedBy, Rdfs_isDefinedBy -> 0
        | Owl_deprecated, Owl_deprecated -> 0
        | Owl_versionInfo, Owl_versionInfo -> 0
        | Owl_priorVersion, Owl_priorVersion -> 0
        | Owl_backwardCompatibleWith, Owl_backwardCompatibleWith -> 0
        | Owl_incompatibleWith, Owl_incompatibleWith -> 0
        | _ -> invalid_arg "AnnotationProperty.compare"
    let equal ap1 ap2 =
      match ap1, ap2 with
      | IRI iri1, IRI iri2 -> IRI.equal iri1 iri2
      | Rdfs_label, Rdfs_label -> true
      | Rdfs_comment, Rdfs_comment -> true
      | Rdfs_seeAlso, Rdfs_seeAlso -> true
      | Rdfs_isDefinedBy, Rdfs_isDefinedBy -> true
      | Owl_deprecated, Owl_deprecated -> true
      | Owl_versionInfo, Owl_versionInfo -> true
      | Owl_priorVersion, Owl_priorVersion -> true
      | Owl_backwardCompatibleWith, Owl_backwardCompatibleWith -> true
      | Owl_incompatibleWith, Owl_incompatibleWith -> true
      | _ -> false
    let hash = function
      | IRI iri -> (hm 1) * (IRI.hash iri)
      | Rdfs_label -> (hm 2)
      | Rdfs_comment -> (hm 3)
      | Rdfs_seeAlso -> (hm 4)
      | Rdfs_isDefinedBy -> (hm 5)
      | Owl_deprecated -> (hm 6)
      | Owl_versionInfo -> (hm 7)
      | Owl_priorVersion -> (hm 8)
      | Owl_backwardCompatibleWith -> (hm 9)
      | Owl_incompatibleWith -> (hm 10)
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
  end
  include CommonExtended.Make(Constructor)
  let str_of = Constructor.str_of
end;;

(**====================== Classes =======================**)

module Class = struct
  module Constructor = struct
    type t =
      | IRI of IRI.t
      | Thing
      | Nothing
    let precedence = function
      | IRI _ -> 0
      | Thing -> 1
      | Nothing -> 2
    let compare c1 c2 =
      let c = (precedence c1) - (precedence c2) in
      if c <> 0 then c else
        match c1, c2 with
        | IRI iri1, IRI iri2 -> IRI.compare iri1 iri2
        | Thing, Thing -> 0
        | Nothing, Nothing -> 0
        | _ -> invalid_arg "Class.compare"
    let equal c1 c2 =
      match c1, c2 with
      | IRI iri1, IRI iri2 -> IRI.equal iri1 iri2
      | Thing, Thing -> true
      | Nothing, Nothing -> true
      | _ -> false
    let hash = function
      | IRI iri -> (hm 1) * (IRI.hash iri)
      | Thing -> (hm 2)
      | Nothing -> (hm 3)
    let str_of = function
      | IRI iri -> IRI.str_of iri
      | Thing -> "owl:Thing"
      | Nothing -> "owl:Nothing"
  end
  include CommonExtended.Make(Constructor)
  let str_of = Constructor.str_of
end;;

(**==================== Individuals =====================**)

module Individual = struct
  module Constructor = struct
    type t =
      | NamedIndividual of IRI.t
      | AnonymousIndividual of NodeID.t
    let precedence = function
      | NamedIndividual _ -> 0
      | AnonymousIndividual _ -> 1
    let compare i1 i2 =
      let c = (precedence i1) - (precedence i2) in
      if c <> 0 then c else
        match i1, i2 with
        | NamedIndividual iri1, NamedIndividual iri2 -> IRI.compare iri1 iri2
        | AnonymousIndividual id1, AnonymousIndividual id2 -> NodeID.compare id1 id2
        | _ -> invalid_arg "Individual.compare"
    let equal i1 i2 =
      match i1, i2 with
      | NamedIndividual iri1, NamedIndividual iri2 -> IRI.equal iri1 iri2
      | AnonymousIndividual id1, AnonymousIndividual id2 -> NodeID.equal id1 id2
      | _ -> false
    let hash = function
      | NamedIndividual iri -> (hm 1) * (IRI.hash iri)
      | AnonymousIndividual id -> (hm 2) * (NodeID.hash id)
    let str_of = function
      | NamedIndividual iri -> IRI.str_of iri
      | AnonymousIndividual id -> NodeID.str_of id
  end
  include CommonExtended.Make(Constructor)
  let str_of = Constructor.str_of
end;;

(**======================= Literals =====================**)

module Literal = struct
  module Constructor = struct
    type t =
      | TypedLiteral of string * Datatype.t
      | StringLiteralNoLanguage of string
      | StringLiteralWithLanguage of string * string
    let precedence = function
      | TypedLiteral _ -> 0
      | StringLiteralNoLanguage _ -> 1
      | StringLiteralWithLanguage _ -> 2
    let compare l1 l2 =
      let c = (precedence l1) - (precedence l2) in
      if c <> 0 then c else
        match l1, l2 with
        | TypedLiteral (st1, dt1), TypedLiteral (st2, dt2) ->
            C.compare2 (String.compare) (Datatype.compare) st1 dt1 st2 dt2
        | StringLiteralNoLanguage st1, StringLiteralNoLanguage st2 ->
            String.compare st1 st2
        | StringLiteralWithLanguage (st1, ln1), StringLiteralWithLanguage (st2, ln2) ->
            C.compare2 (String.compare) (String.compare) st1 ln1 st2 ln2
        | _ -> invalid_arg "Literal.compare"
    let equal l1 l2 =
      match l1, l2 with
      | TypedLiteral (st1, dt1), TypedLiteral (st2, dt2) ->
          C.equal2 (=) (Datatype.equal) st1 dt1 st2 dt2
      | StringLiteralNoLanguage st1, StringLiteralNoLanguage st2 ->
          st1 = st2
      | StringLiteralWithLanguage (st1, ln1), StringLiteralWithLanguage (st2, ln2) ->
          C.equal2 (=) (=) st1 ln1 st2 ln2
      | _ -> false
    let hash = function
      | TypedLiteral (st, dt) -> (hm 1) * (C.hash2 (Hashtbl.hash) (Datatype.hash) st dt)
      | StringLiteralNoLanguage st -> (hm 2) * (Hashtbl.hash st)
      | StringLiteralWithLanguage (st, ln) -> (hm 3) * (C.hash2 (Hashtbl.hash) (Hashtbl.hash) st ln)
  end
  include CommonConsed.Make (Constructor)
end;;

(**============ Object Property Expressions =============**)

module ObjectPropertyExpression =
struct
  module Constructor = struct
    type t =
      | ObjectProperty of ObjectProperty.t
      | InverseObjectProperty of ObjectProperty.t
    let precedence = function
      | ObjectProperty _ -> 0
      | InverseObjectProperty _ -> 1
    let compare op1 op2 =
      let c = (precedence op1) - (precedence op2) in
      if c <> 0 then c else
        match op1, op2 with
        | ObjectProperty ar1, ObjectProperty ar2 -> ObjectProperty.compare ar1 ar2
        | InverseObjectProperty ar1, InverseObjectProperty ar2 -> ObjectProperty.compare ar1 ar2
        | _ -> invalid_arg "ObjectPropertyExpression.compare"
    let equal op1 op2 =
      match op1, op2 with
      | ObjectProperty ar1, ObjectProperty ar2 -> ObjectProperty.equal ar1 ar2
      | InverseObjectProperty ar1, InverseObjectProperty ar2 -> ObjectProperty.equal ar1 ar2
      | _ -> false
    let hash = function
      | ObjectProperty ar -> (hm 1) * (ObjectProperty.hash ar)
      | InverseObjectProperty ar -> (hm 2) * (ObjectProperty.hash ar)
  end
  include CommonConsed.Make (Constructor)
end;;

(**============= Data Property Expressions ==============**)

module DataPropertyExpression = struct
  module Constructor = struct
    type t =
      | DataProperty of DataProperty.t
    let compare dpe1 dpe2 =
      match dpe1, dpe2 with
      | DataProperty dp1, DataProperty dp2 -> DataProperty.compare dp1 dp2
    let equal dpe1 dpe2 =
      match dpe1, dpe2 with
      | DataProperty dp1, DataProperty dp2 -> DataProperty.equal dp1 dp2
    let hash = function
      | DataProperty dp -> (hm 1) * (DataProperty.hash dp)
  end
  include CommonConsed.Make (Constructor)
end;;

(**==================== Data Ranges =====================**)

module DataRange = struct
  module Constructor = struct
    type t =
      | Datatype of Datatype.t
      | DataIntersectionOf of t consed list
      | DataUnionOf of t consed list
      | DataComplementOf of t consed
      | DataOneOf of Literal.t list
      | DatatypeRestriction of Datatype.t * ((ConstrainingFacet.t * Literal.t) list)
    let precedence = function
      | Datatype _ -> 0
      | DataIntersectionOf _ -> 1
      | DataUnionOf _ -> 2
      | DataComplementOf _ -> 3
      | DataOneOf _ -> 4
      | DatatypeRestriction _ -> 5
    let compare dr1 dr2 =
      let c = (precedence dr1) - (precedence dr2) in
      if c <> 0 then c else
        match dr1, dr2 with
        | Datatype dt1, Datatype dt2 -> Datatype.compare dt1 dt2
        | DataIntersectionOf dr_lst1, DataIntersectionOf dr_lst2 ->
            C.compare_lst Consed.compare dr_lst1 dr_lst2
        | DataUnionOf dr_lst1, DataUnionOf dr_lst2 ->
            C.compare_lst Consed.compare dr_lst1 dr_lst2
        | DataComplementOf dr1, DataComplementOf dr2 -> Consed.compare dr1 dr2
        | DataOneOf lt_lst1, DataOneOf lt_lst2 -> C.compare_lst Literal.compare lt_lst1 lt_lst2
        | DatatypeRestriction (dt1, rt_lst1), DatatypeRestriction (dt2, rt_lst2) ->
            C.compare2 Datatype.compare (C.compare_lst
                  (fun (cf1, l1) (cf2, l2) -> C.compare2 ConstrainingFacet.compare Literal.compare cf1 l1 cf2 l2)
              ) dt1 rt_lst1 dt2 rt_lst2
        | _ -> invalid_arg "DataRange.compare"
    let equal dr1 dr2 =
      match dr1, dr2 with
      | Datatype dt1, Datatype dt2 -> Datatype.equal dt1 dt2
      | DataIntersectionOf dr_lst1, DataIntersectionOf dr_lst2 ->
          C.equal_lst Consed.equal dr_lst1 dr_lst2
      | DataUnionOf dr_lst1, DataUnionOf dr_lst2 -> C.equal_lst Consed.equal dr_lst1 dr_lst2
      | DataComplementOf dr1, DataComplementOf dr2 -> Consed.equal dr1 dr2
      | DataOneOf lt_lst1, DataOneOf lt_lst2 -> C.equal_lst Literal.equal lt_lst1 lt_lst2
      | DatatypeRestriction (dt1, rt_lst1), DatatypeRestriction (dt2, rt_lst2) ->
          C.equal2 Datatype.equal (C.equal_lst
                (fun (cf1, l1) (cf2, l2) -> C.equal2 ConstrainingFacet.equal Literal.equal cf1 l1 cf2 l2)
            ) dt1 rt_lst1 dt2 rt_lst2
      | _ -> false
    let hash = function
      | Datatype dt -> (hm 1) * (Datatype.hash dt)
      | DataIntersectionOf dr_lst -> (hm 2) * (C.hash_lst Consed.hash dr_lst)
      | DataUnionOf dr_lst -> (hm 3) * (C.hash_lst Consed.hash dr_lst)
      | DataComplementOf dr -> (hm 4) * (Consed.hash dr)
      | DataOneOf lt_lst -> (hm 5) * (C.hash_lst Literal.hash lt_lst)
      | DatatypeRestriction (dt, rt_lst) -> (hm 6) *
          C.hash2 Datatype.hash (C.hash_lst (fun (cf, l) -> C.hash2 ConstrainingFacet.hash Literal.hash cf l)) dt rt_lst
  end
  include CommonConsed.Make (Constructor)
end;;

(**=================== Class Expressions ================**)

module ClassExpression = struct
  module Constructor = struct
    type t =
      | Class of Class.t
      | ObjectIntersectionOf of t consed * t consed
      | ObjectUnionOf of t consed * t consed
      | ObjectComplementOf of t consed
      | ObjectOneOf of Individual.t list
      | ObjectSomeValuesFrom of ObjectPropertyExpression.t * t consed
      | ObjectAllValuesFrom of ObjectPropertyExpression.t * t consed
      | ObjectHasValue of ObjectPropertyExpression.t * Individual.t
      | ObjectHasSelf of ObjectPropertyExpression.t
      | ObjectMinCardinality of int * ObjectPropertyExpression.t * t consed option
      | ObjectMaxCardinality of int * ObjectPropertyExpression.t * t consed option
      | ObjectExactCardinality of int * ObjectPropertyExpression.t * t consed option
      | DataSomeValuesFrom of DataPropertyExpression.t list * DataRange.t
      | DataAllValuesFrom of DataPropertyExpression.t list * DataRange.t
      | DataHasValue of DataPropertyExpression.t * Literal.t
      | DataMinCardinality of int * DataPropertyExpression.t * DataRange.t option
      | DataMaxCardinality of int * DataPropertyExpression.t * DataRange.t option
      | DataExactCardinality of int * DataPropertyExpression.t * DataRange.t option
    let precedence = function
      | Class _ -> 0
      | ObjectIntersectionOf _ -> 1
      | ObjectUnionOf _ -> 3
      | ObjectComplementOf _ -> 5
      | ObjectOneOf _ -> 6
      | ObjectSomeValuesFrom _ -> 7
      | ObjectAllValuesFrom _ -> 8
      | ObjectHasValue _ -> 9
      | ObjectHasSelf _ -> 10
      | ObjectMinCardinality _ -> 11
      | ObjectMaxCardinality _ -> 12
      | ObjectExactCardinality _ -> 13
      | DataSomeValuesFrom _ -> 14
      | DataAllValuesFrom _ -> 15
      | DataHasValue _ -> 16
      | DataMinCardinality _ -> 17
      | DataMaxCardinality _ -> 18
      | DataExactCardinality _ -> 19
    let compare ce1 ce2 =
      let c = (precedence ce1) - (precedence ce2) in
      if c <> 0 then c else
        match ce1, ce2 with
        | Class c1, Class c2 -> Class.compare c1 c2
        | ObjectIntersectionOf (cce1, ce1), ObjectIntersectionOf (cce2, ce2) -> C.compare2 Consed.compare Consed.compare cce1 ce1 cce2 ce2
        | ObjectUnionOf (cce1, ce1), ObjectUnionOf (cce2, ce2) -> C.compare2 Consed.compare Consed.compare cce1 ce1 cce2 ce2
        | ObjectComplementOf ce1, ObjectComplementOf ce2 -> Consed.compare ce1 ce2
        | ObjectOneOf i_lst1, ObjectOneOf i_lst2 -> C.compare_lst Individual.compare i_lst1 i_lst2
        | ObjectSomeValuesFrom (ope1, ce1), ObjectSomeValuesFrom (ope2, ce2) ->
            C.compare2 ObjectPropertyExpression.compare Consed.compare ope1 ce1 ope2 ce2
        | ObjectAllValuesFrom (ope1, ce1), ObjectAllValuesFrom (ope2, ce2) ->
            C.compare2 ObjectPropertyExpression.compare Consed.compare ope1 ce1 ope2 ce2
        | ObjectHasValue (ope1, i1), ObjectHasValue (ope2, i2) ->
            C.compare2 ObjectPropertyExpression.compare Individual.compare ope1 i1 ope2 i2
        | ObjectHasSelf ope1, ObjectHasSelf ope2 -> ObjectPropertyExpression.compare ope1 ope2
        | ObjectMinCardinality (n1, ope1, ceo1), ObjectMinCardinality (n2, ope2, ceo2) ->
            C.compare_n2o ObjectPropertyExpression.compare Consed.compare n1 ope1 ceo1 n2 ope2 ceo2
        | ObjectMaxCardinality (n1, ope1, ceo1), ObjectMaxCardinality (n2, ope2, ceo2) ->
            C.compare_n2o ObjectPropertyExpression.compare Consed.compare n1 ope1 ceo1 n2 ope2 ceo2
        | ObjectExactCardinality (n1, ope1, ceo1), ObjectExactCardinality (n2, ope2, ceo2) ->
            C.compare_n2o ObjectPropertyExpression.compare Consed.compare n1 ope1 ceo1 n2 ope2 ceo2
        | DataSomeValuesFrom (dpe_lst1, dr1), DataSomeValuesFrom (dpe_lst2, dr2) ->
            C.compare2 (C.compare_lst DataPropertyExpression.compare) DataRange.compare dpe_lst1 dr1 dpe_lst2 dr2
        | DataAllValuesFrom (dpe_lst1, dr1), DataAllValuesFrom (dpe_lst2, dr2) ->
            C.compare2 (C.compare_lst DataPropertyExpression.compare) DataRange.compare dpe_lst1 dr1 dpe_lst2 dr2
        | DataHasValue (dpe1, lt1), DataHasValue (dpe2, lt2) ->
            C.compare2 DataPropertyExpression.compare Literal.compare dpe1 lt1 dpe2 lt2
        | DataMinCardinality (n1, dpe1, dro1), DataMinCardinality (n2, dpe2, dro2) ->
            C.compare_n2o DataPropertyExpression.compare DataRange.compare n1 dpe1 dro1 n2 dpe2 dro2
        | DataMaxCardinality (n1, dpe1, dro1), DataMaxCardinality (n2, dpe2, dro2) ->
            C.compare_n2o DataPropertyExpression.compare DataRange.compare n1 dpe1 dro1 n2 dpe2 dro2
        | DataExactCardinality (n1, dpe1, dro1), DataExactCardinality (n2, dpe2, dro2) ->
            C.compare_n2o DataPropertyExpression.compare DataRange.compare n1 dpe1 dro1 n2 dpe2 dro2
        | _ -> invalid_arg "ClassExpression.compare"
    let equal c1 c2 =
      match c1, c2 with
      | Class c1, Class c2 -> Class.equal c1 c2
      | ObjectIntersectionOf (cce1, ce1), ObjectIntersectionOf (cce2, ce2) -> C.equal2 Consed.equal Consed.equal cce1 ce1 cce2 ce2
      | ObjectUnionOf (cce1, ce1), ObjectUnionOf (cce2, ce2) -> C.equal2 Consed.equal Consed.equal cce1 ce1 cce2 ce2
      | ObjectComplementOf ce1, ObjectComplementOf ce2 -> Consed.equal ce1 ce2
      | ObjectOneOf i_lst1, ObjectOneOf i_lst2 -> C.equal_lst Individual.equal i_lst1 i_lst2
      | ObjectSomeValuesFrom (ope1, ce1), ObjectSomeValuesFrom (ope2, ce2) ->
          C.equal2 ObjectPropertyExpression.equal Consed.equal ope1 ce1 ope2 ce2
      | ObjectAllValuesFrom (ope1, ce1), ObjectAllValuesFrom (ope2, ce2) ->
          C.equal2 ObjectPropertyExpression.equal Consed.equal ope1 ce1 ope2 ce2
      | ObjectHasValue (ope1, i1), ObjectHasValue (ope2, i2) ->
          C.equal2 ObjectPropertyExpression.equal Individual.equal ope1 i1 ope2 i2
      | ObjectHasSelf ope1, ObjectHasSelf ope2 -> ObjectPropertyExpression.equal ope1 ope2
      | ObjectMinCardinality (n1, ope1, ceo1), ObjectMinCardinality (n2, ope2, ceo2) ->
          C.equal_n2o ObjectPropertyExpression.equal Consed.equal n1 ope1 ceo1 n2 ope2 ceo2
      | ObjectMaxCardinality (n1, ope1, ceo1), ObjectMaxCardinality (n2, ope2, ceo2) ->
          C.equal_n2o ObjectPropertyExpression.equal Consed.equal n1 ope1 ceo1 n2 ope2 ceo2
      | ObjectExactCardinality (n1, ope1, ceo1), ObjectExactCardinality (n2, ope2, ceo2) ->
          C.equal_n2o ObjectPropertyExpression.equal Consed.equal n1 ope1 ceo1 n2 ope2 ceo2
      | DataSomeValuesFrom (dpe_lst1, dr1), DataSomeValuesFrom (dpe_lst2, dr2) ->
          C.equal2 (C.equal_lst DataPropertyExpression.equal) DataRange.equal dpe_lst1 dr1 dpe_lst2 dr2
      | DataAllValuesFrom (dpe_lst1, dr1), DataAllValuesFrom (dpe_lst2, dr2) ->
          C.equal2 (C.equal_lst DataPropertyExpression.equal) DataRange.equal dpe_lst1 dr1 dpe_lst2 dr2
      | DataHasValue (dpe1, lt1), DataHasValue (dpe2, lt2) ->
          C.equal2 DataPropertyExpression.equal Literal.equal dpe1 lt1 dpe2 lt2
      | DataMinCardinality (n1, dpe1, dro1), DataMinCardinality (n2, dpe2, dro2) ->
          C.equal_n2o DataPropertyExpression.equal DataRange.equal n1 dpe1 dro1 n2 dpe2 dro2
      | DataMaxCardinality (n1, dpe1, dro1), DataMaxCardinality (n2, dpe2, dro2) ->
          C.equal_n2o DataPropertyExpression.equal DataRange.equal n1 dpe1 dro1 n2 dpe2 dro2
      | DataExactCardinality (n1, dpe1, dro1), DataExactCardinality (n2, dpe2, dro2) ->
          C.equal_n2o DataPropertyExpression.equal DataRange.equal n1 dpe1 dro1 n2 dpe2 dro2
      | _ -> false
    let hash = function
      | Class c -> (hm 1) * (Class.hash c)
      | ObjectIntersectionOf (cce, ce) -> (hm 2) * (C.hash2 Consed.hash Consed.hash cce ce)
      | ObjectUnionOf (cce, ce) -> (hm 4) * (C.hash2 Consed.hash Consed.hash cce ce)
      | ObjectComplementOf c -> (hm 6) * (Consed.hash c)
      | ObjectOneOf i_lst -> (hm 7) * (C.hash_lst Individual.hash i_lst)
      | ObjectSomeValuesFrom (ope, ce) -> (hm 8) * (C.hash2 ObjectPropertyExpression.hash Consed.hash ope ce)
      | ObjectAllValuesFrom (ope, ce) -> (hm 9) * (C.hash2 ObjectPropertyExpression.hash Consed.hash ope ce)
      | ObjectHasValue (ope, i) -> (hm 10) * (C.hash2 ObjectPropertyExpression.hash Individual.hash ope i)
      | ObjectHasSelf ope -> (hm 11) * (ObjectPropertyExpression.hash ope)
      | ObjectMinCardinality (n, ope, ceo) -> (hm 12) * (C.hash_n2o ObjectPropertyExpression.hash Consed.hash n ope ceo)
      | ObjectMaxCardinality (n, ope, ceo) -> (hm 13) * (C.hash_n2o ObjectPropertyExpression.hash Consed.hash n ope ceo)
      | ObjectExactCardinality (n, ope, ceo) -> (hm 14) * (C.hash_n2o ObjectPropertyExpression.hash Consed.hash n ope ceo)
      | DataSomeValuesFrom (dpe_lst, dr) -> (hm 15) * (C.hash2 (C.hash_lst DataPropertyExpression.hash) DataRange.hash dpe_lst dr)
      | DataAllValuesFrom (dpe_lst, dr) -> (hm 16) * (C.hash2 (C.hash_lst DataPropertyExpression.hash) DataRange.hash dpe_lst dr)
      | DataHasValue (dpe, lt) -> (hm 17) * (C.hash2 DataPropertyExpression.hash Literal.hash dpe lt)
      | DataMinCardinality (n, dpe, dro) -> (hm 18) * (C.hash_n2o DataPropertyExpression.hash DataRange.hash n dpe dro)
      | DataMaxCardinality (n, dpe, dro) -> (hm 19) * (C.hash_n2o DataPropertyExpression.hash DataRange.hash n dpe dro)
      | DataExactCardinality (n, dpe, dro) -> (hm 20) * (C.hash_n2o DataPropertyExpression.hash DataRange.hash n dpe dro)
  end
  include CommonConsed.Make (Constructor)
end

(**================= Class Expression Axioms ===============**)

module ClassExpressionAxiom = struct
  module Constructor = struct
    type t =
      | SubClassOf of ClassExpression.t * ClassExpression.t
      | EquivalentClasses of ClassExpression.t list
      | DisjointClasses of ClassExpression.t list
      | DisjointUnion of Class.t * ClassExpression.t list
    let precedence = function
      | SubClassOf _ -> 0
      | EquivalentClasses _ -> 1
      | DisjointClasses _ -> 2
      | DisjointUnion _ -> 3
    let compare ax1 ax2 =
      let c =	(precedence ax1) - (precedence ax2) in
      if c <> 0 then c else
        match ax1, ax2 with
        | SubClassOf (sce1, ce1), SubClassOf (sce2, ce2) ->
            C.compare2 ClassExpression.compare ClassExpression.compare sce1 ce1 sce2 ce2
        | EquivalentClasses ce_lst1, EquivalentClasses ce_lst2 -> C.compare_lst ClassExpression.compare ce_lst1 ce_lst2
        | DisjointClasses ce_lst1, DisjointClasses ce_lst2 -> C.compare_lst ClassExpression.compare ce_lst1 ce_lst2
        | DisjointUnion (c1, ce_lst1), DisjointUnion (c2, ce_lst2) ->
            C.compare2 Class.compare (C.compare_lst ClassExpression.compare) c1 ce_lst1 c2 ce_lst2
        | _ -> invalid_arg "ClassExpressionAxiom.compare"
    let equal ax1 ax2 =
      match ax1, ax2 with
      | SubClassOf (sce1, ce1), SubClassOf (sce2, ce2) ->
          C.equal2 ClassExpression.equal ClassExpression.equal sce1 ce1 sce2 ce2
      | EquivalentClasses ce_lst1, EquivalentClasses ce_lst2 -> C.equal_lst ClassExpression.equal ce_lst1 ce_lst2
      | DisjointClasses ce_lst1, DisjointClasses ce_lst2 -> C.equal_lst ClassExpression.equal ce_lst1 ce_lst2
      | DisjointUnion (c1, ce_lst1), DisjointUnion (c2, ce_lst2) ->
          C.equal2 Class.equal (C.equal_lst ClassExpression.equal) c1 ce_lst1 c2 ce_lst2
      | _ -> false
    let hash = function
      | SubClassOf (sce, ce) -> (hm 1) * (C.hash2 ClassExpression.hash ClassExpression.hash sce ce)
      | EquivalentClasses ce_lst -> (hm 2) * (C.hash_lst ClassExpression.hash ce_lst)
      | DisjointClasses ce_lst -> (hm 3) * (C.hash_lst ClassExpression.hash ce_lst)
      | DisjointUnion (c, ce_lst) -> (hm 4) * C.hash2 Class.hash (C.hash_lst ClassExpression.hash) c ce_lst
  end
  include CommonConsed.Make (Constructor)
end

(**================= Object Property Axioms ================**)

module ObjectPropertyAxiom = struct
  module Constructor = struct
    type t =
      | SubObjectPropertyOf of ObjectPropertyExpression.t list * ObjectPropertyExpression.t
      | EquivalentObjectProperties of ObjectPropertyExpression.t list
      | DisjointObjectProperties of ObjectPropertyExpression.t list
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
    let precedence = function
      | SubObjectPropertyOf _ -> 0
      | EquivalentObjectProperties _ -> 1
      | DisjointObjectProperties _ -> 2
      | InverseObjectProperties _ -> 3
      | ObjectPropertyDomain _ -> 4
      | ObjectPropertyRange _ -> 5
      | FunctionalObjectProperty _ -> 6
      | InverseFunctionalObjectProperty _ -> 7
      | ReflexiveObjectProperty _ -> 8
      | IrreflexiveObjectProperty _ -> 9
      | SymmetricObjectProperty _ -> 10
      | AsymmetricObjectProperty _ -> 11
      | TransitiveObjectProperty _ -> 12
    let compare ax1 ax2 =
      let c =	(precedence ax1) - (precedence ax2) in
      if c <> 0 then c else
        match ax1, ax2 with
        | SubObjectPropertyOf (ope_ch1, ope1), SubObjectPropertyOf (ope_ch2, ope2) ->
            C.compare2 (C.compare_lst ObjectPropertyExpression.compare) ObjectPropertyExpression.compare ope_ch1 ope1 ope_ch2 ope2
        | EquivalentObjectProperties ope_lst1, EquivalentObjectProperties ope_lst2 ->
            C.compare_lst ObjectPropertyExpression.compare ope_lst1 ope_lst2
        | DisjointObjectProperties ope_lst1, DisjointObjectProperties ope_lst2 ->
            C.compare_lst ObjectPropertyExpression.compare ope_lst1 ope_lst2
        | InverseObjectProperties (sope1, ope1), InverseObjectProperties (sope2, ope2) ->
            C.compare2 ObjectPropertyExpression.compare ObjectPropertyExpression.compare sope1 ope1 sope2 ope2
        | ObjectPropertyDomain (ope1, ce1), ObjectPropertyDomain (ope2, ce2) ->
            C.compare2 ObjectPropertyExpression.compare ClassExpression.compare ope1 ce1 ope2 ce2
        | ObjectPropertyRange (ope1, ce1), ObjectPropertyRange (ope2, ce2) ->
            C.compare2 ObjectPropertyExpression.compare ClassExpression.compare ope1 ce1 ope2 ce2
        | FunctionalObjectProperty ope1, FunctionalObjectProperty ope2 -> ObjectPropertyExpression.compare ope1 ope2
        | InverseFunctionalObjectProperty ope1, InverseFunctionalObjectProperty ope2 -> ObjectPropertyExpression.compare ope1 ope2
        | ReflexiveObjectProperty ope1, ReflexiveObjectProperty ope2 -> ObjectPropertyExpression.compare ope1 ope2
        | IrreflexiveObjectProperty ope1, IrreflexiveObjectProperty ope2 -> ObjectPropertyExpression.compare ope1 ope2
        | SymmetricObjectProperty ope1, SymmetricObjectProperty ope2 -> ObjectPropertyExpression.compare ope1 ope2
        | AsymmetricObjectProperty ope1, AsymmetricObjectProperty ope2 -> ObjectPropertyExpression.compare ope1 ope2
        | TransitiveObjectProperty ope1, TransitiveObjectProperty ope2 -> ObjectPropertyExpression.compare ope1 ope2
        | _ -> invalid_arg "ObjectPropertyAxiom.compare"
    let equal ax1 ax2 =
      match ax1, ax2 with
      | SubObjectPropertyOf (ope_ch1, ope1), SubObjectPropertyOf (ope_ch2, ope2) ->
          C.equal2 (C.equal_lst ObjectPropertyExpression.equal) ObjectPropertyExpression.equal ope_ch1 ope1 ope_ch2 ope2
      | EquivalentObjectProperties ope_lst1, EquivalentObjectProperties ope_lst2 ->
          C.equal_lst ObjectPropertyExpression.equal ope_lst1 ope_lst2
      | DisjointObjectProperties ope_lst1, DisjointObjectProperties ope_lst2 ->
          C.equal_lst ObjectPropertyExpression.equal ope_lst1 ope_lst2
      | InverseObjectProperties (sope1, ope1), InverseObjectProperties (sope2, ope2) ->
          C.equal2 ObjectPropertyExpression.equal ObjectPropertyExpression.equal sope1 ope1 sope2 ope2
      | ObjectPropertyDomain (ope1, ce1), ObjectPropertyDomain (ope2, ce2) ->
          C.equal2 ObjectPropertyExpression.equal ClassExpression.equal ope1 ce1 ope2 ce2
      | ObjectPropertyRange (ope1, ce1), ObjectPropertyRange (ope2, ce2) ->
          C.equal2 ObjectPropertyExpression.equal ClassExpression.equal ope1 ce1 ope2 ce2
      | FunctionalObjectProperty ope1, FunctionalObjectProperty ope2 -> ObjectPropertyExpression.equal ope1 ope2
      | InverseFunctionalObjectProperty ope1, InverseFunctionalObjectProperty ope2 -> ObjectPropertyExpression.equal ope1 ope2
      | ReflexiveObjectProperty ope1, ReflexiveObjectProperty ope2 -> ObjectPropertyExpression.equal ope1 ope2
      | IrreflexiveObjectProperty ope1, IrreflexiveObjectProperty ope2 -> ObjectPropertyExpression.equal ope1 ope2
      | SymmetricObjectProperty ope1, SymmetricObjectProperty ope2 -> ObjectPropertyExpression.equal ope1 ope2
      | AsymmetricObjectProperty ope1, AsymmetricObjectProperty ope2 -> ObjectPropertyExpression.equal ope1 ope2
      | TransitiveObjectProperty ope1, TransitiveObjectProperty ope2 -> ObjectPropertyExpression.equal ope1 ope2
      | _ -> false
    let hash = function
      | SubObjectPropertyOf (ope_ch, ope) ->
          (hm 1) * ((hm 1) * (C.hash_lst ObjectPropertyExpression.hash ope_ch) + (hm 2) * (ObjectPropertyExpression.hash ope))
      | EquivalentObjectProperties ope_lst -> (hm 2) * (C.hash_lst ObjectPropertyExpression.hash ope_lst)
      | DisjointObjectProperties ope_lst -> (hm 3) * (C.hash_lst ObjectPropertyExpression.hash ope_lst)
      | InverseObjectProperties (sope, ope) -> (hm 4) * (C.hash2 ObjectPropertyExpression.hash ObjectPropertyExpression.hash sope ope)
      | ObjectPropertyDomain (ope, ce) -> (hm 5) * (C.hash2 ObjectPropertyExpression.hash ClassExpression.hash ope ce)
      | ObjectPropertyRange (ope, ce) -> (hm 6) * (C.hash2 ObjectPropertyExpression.hash ClassExpression.hash ope ce)
      | FunctionalObjectProperty ope -> (hm 7) * (ObjectPropertyExpression.hash ope)
      | InverseFunctionalObjectProperty ope -> (hm 8) * (ObjectPropertyExpression.hash ope)
      | ReflexiveObjectProperty ope -> (hm 9) * (ObjectPropertyExpression.hash ope)
      | IrreflexiveObjectProperty ope -> (hm 10) * (ObjectPropertyExpression.hash ope)
      | SymmetricObjectProperty ope -> (hm 11) * (ObjectPropertyExpression.hash ope)
      | AsymmetricObjectProperty ope -> (hm 12) * (ObjectPropertyExpression.hash ope)
      | TransitiveObjectProperty ope -> (hm 13) * (ObjectPropertyExpression.hash ope)
  end
  include CommonConsed.Make (Constructor)
end

(**================= Data Property Axioms ================**)

module DataPropertyAxiom = struct
  module Constructor = struct
    type t =
      | SubDataPropertyOf of DataPropertyExpression.t * DataPropertyExpression.t
      | EquivalentDataProperties of DataPropertyExpression.t list
      | DisjointDataProperties of DataPropertyExpression.t list
      | DataPropertyDomain of DataPropertyExpression.t * ClassExpression.t
      | DataPropertyRange of DataPropertyExpression.t * DataRange.t
      | FunctionalDataProperty of DataPropertyExpression.t
    let precedence = function
      | SubDataPropertyOf _ -> 0
      | EquivalentDataProperties _ -> 1
      | DisjointDataProperties _ -> 2
      | DataPropertyDomain _ -> 3
      | DataPropertyRange _ -> 4
      | FunctionalDataProperty _ -> 5
    let compare ax1 ax2 =
      let c =	(precedence ax1) - (precedence ax2) in
      if c <> 0 then c else
        match ax1, ax2 with
        | SubDataPropertyOf (sdpe1, dpe1), SubDataPropertyOf (sdpe2, spe2) ->
            C.compare2 DataPropertyExpression.compare DataPropertyExpression.compare sdpe1 dpe1 sdpe2 spe2
        | EquivalentDataProperties dpe_lst1, EquivalentDataProperties dpe_lst2 ->
            C.compare_lst DataPropertyExpression.compare dpe_lst1 dpe_lst2
        | DisjointDataProperties dpe_lst1, DisjointDataProperties dpe_lst2 ->
            C.compare_lst DataPropertyExpression.compare dpe_lst1 dpe_lst2
        | DataPropertyDomain (dpe1, ce1), DataPropertyDomain (dpe2, ce2) ->
            C.compare2 DataPropertyExpression.compare ClassExpression.compare dpe1 ce1 dpe2 ce2
        | DataPropertyRange (dpe1, dr1), DataPropertyRange (dpe2, dr2) ->
            C.compare2 DataPropertyExpression.compare DataRange.compare dpe1 dr1 dpe2 dr2
        | FunctionalDataProperty dpe1, FunctionalDataProperty dpe2 ->
            DataPropertyExpression.compare dpe1 dpe2
        | _ -> invalid_arg "DataPropertyAxiom.compare"
    let equal ax1 ax2 =
      match ax1, ax2 with
      | SubDataPropertyOf (sdpe1, dpe1), SubDataPropertyOf (sdpe2, spe2) ->
          C.equal2 DataPropertyExpression.equal DataPropertyExpression.equal sdpe1 dpe1 sdpe2 spe2
      | EquivalentDataProperties dpe_lst1, EquivalentDataProperties dpe_lst2 ->
          C.equal_lst DataPropertyExpression.equal dpe_lst1 dpe_lst2
      | DisjointDataProperties dpe_lst1, DisjointDataProperties dpe_lst2 ->
          C.equal_lst DataPropertyExpression.equal dpe_lst1 dpe_lst2
      | DataPropertyDomain (dpe1, ce1), DataPropertyDomain (dpe2, ce2) ->
          C.equal2 DataPropertyExpression.equal ClassExpression.equal dpe1 ce1 dpe2 ce2
      | DataPropertyRange (dpe1, dr1), DataPropertyRange (dpe2, dr2) ->
          C.equal2 DataPropertyExpression.equal DataRange.equal dpe1 dr1 dpe2 dr2
      | FunctionalDataProperty dpe1, FunctionalDataProperty dpe2 ->
          DataPropertyExpression.equal dpe1 dpe2
      | _ -> false
    let hash = function
      | SubDataPropertyOf (sdpe, dpe) -> C.hash2 DataPropertyExpression.hash DataPropertyExpression.hash sdpe dpe
      | EquivalentDataProperties dpe_lst -> C.hash_lst DataPropertyExpression.hash dpe_lst
      | DisjointDataProperties dpe_lst -> C.hash_lst DataPropertyExpression.hash dpe_lst
      | DataPropertyDomain (dpe, ce) -> C.hash2 DataPropertyExpression.hash ClassExpression.hash dpe ce
      | DataPropertyRange (dpe, dr) -> C.hash2 DataPropertyExpression.hash DataRange.hash dpe dr
      | FunctionalDataProperty dpe -> DataPropertyExpression.hash dpe
  end
  include CommonConsed.Make (Constructor)
end

(**================= Datatype Definitions =================**)

module DatatypeDefinition = struct
  module Constructor = struct
    type t =
      | DatatypeDefinition of Datatype.t * DataRange.t
    let compare ax1 ax2 =
      match ax1, ax2 with
      | DatatypeDefinition (dt1, dr1), DatatypeDefinition (dt2, dr2) ->
          C.compare2 Datatype.compare DataRange.compare dt1 dr1 dt2 dr2
    let equal ax1 ax2 =
      match ax1, ax2 with
      | DatatypeDefinition (dt1, dr1), DatatypeDefinition (dt2, dr2) ->
          C.equal2 Datatype.equal DataRange.equal dt1 dr1 dt2 dr2
    let hash = function
      | DatatypeDefinition (dt, dr) -> C.hash2 Datatype.hash DataRange.hash dt dr
  end
  include CommonConsed.Make (Constructor)
end

(**========================= Keys =========================**)

module Key = struct
  module Constructor = struct
    type t =
      | HasKey of ClassExpression.t * ObjectPropertyExpression.t list * DataPropertyExpression.t list
    let compare ax1 ax2 =
      match ax1, ax2 with
      | HasKey (ce1, ope_lst1, dpe_lst1), HasKey (ce2, ope_lst2, dpe_lst2) ->
          C.compare3 ClassExpression.compare (C.compare_lst ObjectPropertyExpression.compare)
            (C.compare_lst DataPropertyExpression.compare) ce1 ope_lst1 dpe_lst1 ce2 ope_lst2 dpe_lst2
    let equal ax1 ax2 =
      match ax1, ax2 with
      | HasKey (ce1, ope_lst1, dpe_lst1), HasKey (ce2, ope_lst2, dpe_lst2) ->
          C.equal3 ClassExpression.equal (C.equal_lst ObjectPropertyExpression.equal)
            (C.equal_lst DataPropertyExpression.equal) ce1 ope_lst1 dpe_lst1 ce2 ope_lst2 dpe_lst2
    let hash = function
      | HasKey (ce, ope_lst, dpe_lst) -> C.hash3 ClassExpression.hash (C.hash_lst ObjectPropertyExpression.hash)
            (C.hash_lst DataPropertyExpression.hash) ce ope_lst dpe_lst
  end
  include CommonConsed.Make (Constructor)
end

(**====================== Assertions ======================**)

module Assertion = struct
  module Constructor = struct
    type t =
      | SameIndividual of Individual.t list
      | DifferentIndividuals of Individual.t list
      | ClassAssertion of ClassExpression.t * Individual.t
      | ObjectPropertyAssertion of ObjectPropertyExpression.t * Individual.t * Individual.t
      | NegativeObjectPropertyAssertion of ObjectPropertyExpression.t * Individual.t * Individual.t
      | DataPropertyAssertion of DataPropertyExpression.t * Individual.t * Literal.t
      | NegativeDataPropertyAssertion of DataPropertyExpression.t * Individual.t * Literal.t
    let precedence = function
      | SameIndividual _ -> 0
      | DifferentIndividuals _ -> 1
      | ClassAssertion _ -> 2
      | ObjectPropertyAssertion _ -> 3
      | NegativeObjectPropertyAssertion _ -> 4
      | DataPropertyAssertion _ -> 5
      | NegativeDataPropertyAssertion _ -> 6
    let compare ax1 ax2 =
      let c =	(precedence ax1) - (precedence ax2) in
      if c <> 0 then c else
        match ax1, ax2 with
        | SameIndividual iset1, SameIndividual iset2 -> C.compare_lst Individual.compare iset1 iset2
        | DifferentIndividuals iset1, DifferentIndividuals iset2 -> C.compare_lst Individual.compare iset1 iset2
        | ClassAssertion (ce1, i1), ClassAssertion (ce2, i2) -> C.compare2 ClassExpression.compare Individual.compare ce1 i1 ce2 i2
        | ObjectPropertyAssertion (ope1, si1, ti1), ObjectPropertyAssertion (ope2, si2, ti2) ->
            C.compare3 ObjectPropertyExpression.compare Individual.compare Individual.compare ope1 si1 ti1 ope2 si2 ti2
        | NegativeObjectPropertyAssertion (ope1, si1, ti1), NegativeObjectPropertyAssertion (ope2, si2, ti2) ->
            C.compare3 ObjectPropertyExpression.compare Individual.compare Individual.compare ope1 si1 ti1 ope2 si2 ti2
        | DataPropertyAssertion (dpe1, i1, l1), DataPropertyAssertion (dpe2, i2, l2) ->
            C.compare3 DataPropertyExpression.compare Individual.compare Literal.compare dpe1 i1 l1 dpe2 i2 l2
        | NegativeDataPropertyAssertion (dpe1, i1, l1), NegativeDataPropertyAssertion (dpe2, i2, l2) ->
            C.compare3 DataPropertyExpression.compare Individual.compare Literal.compare dpe1 i1 l1 dpe2 i2 l2
        | _ -> invalid_arg "Assertion.compare"
    let equal ax1 ax2 =
      match ax1, ax2 with
      | SameIndividual iset1, SameIndividual iset2 -> C.equal_lst Individual.equal iset1 iset2
      | DifferentIndividuals iset1, DifferentIndividuals iset2 -> C.equal_lst Individual.equal iset1 iset2
      | ClassAssertion (ce1, i1), ClassAssertion (ce2, i2) -> C.equal2 ClassExpression.equal Individual.equal ce1 i1 ce2 i2
      | ObjectPropertyAssertion (ope1, si1, ti1), ObjectPropertyAssertion (ope2, si2, ti2) ->
          C.equal3 ObjectPropertyExpression.equal Individual.equal Individual.equal ope1 si1 ti1 ope2 si2 ti2
      | NegativeObjectPropertyAssertion (ope1, si1, ti1), NegativeObjectPropertyAssertion (ope2, si2, ti2) ->
          C.equal3 ObjectPropertyExpression.equal Individual.equal Individual.equal ope1 si1 ti1 ope2 si2 ti2
      | DataPropertyAssertion (dpe1, i1, l1), DataPropertyAssertion (dpe2, i2, l2) ->
          C.equal3 DataPropertyExpression.equal Individual.equal Literal.equal dpe1 i1 l1 dpe2 i2 l2
      | NegativeDataPropertyAssertion (dpe1, i1, l1), NegativeDataPropertyAssertion (dpe2, i2, l2) ->
          C.equal3 DataPropertyExpression.equal Individual.equal Literal.equal dpe1 i1 l1 dpe2 i2 l2
      | _ -> false
    let hash = function
      | SameIndividual iset -> (hm 1) * (C.hash_lst Individual.hash iset)
      | DifferentIndividuals iset -> (hm 2) * (C.hash_lst Individual.hash iset)
      | ClassAssertion (ce, i) -> (hm 3) * (C.hash2 ClassExpression.hash Individual.hash ce i)
      | ObjectPropertyAssertion (ope, si, ti) ->
          (hm 4) * (C.hash3 ObjectPropertyExpression.hash Individual.hash Individual.hash ope si ti)
      | NegativeObjectPropertyAssertion (ope, si, ti) ->
          (hm 5) * (C.hash3 ObjectPropertyExpression.hash Individual.hash Individual.hash ope si ti)
      | DataPropertyAssertion (dpe, i, l) ->
          (hm 6) * (C.hash3 DataPropertyExpression.hash Individual.hash Literal.hash dpe i l)
      | NegativeDataPropertyAssertion (dpe, i, l) ->
          (hm 7) * (C.hash3 DataPropertyExpression.hash Individual.hash Literal.hash dpe i l)
  end
  include CommonConsed.Make (Constructor)
end

(**================= Annotation Subjects ==================**)

module AnnotationSubject = struct
  module Constructor = struct
    type t =
      | IRI of IRI.t
      | AnonymousIndividual of NodeID.t
    let precedence = function
      | IRI _ -> 0
      | AnonymousIndividual _ -> 1
    let compare as1 as2 =
      let c =	(precedence as1) - (precedence as2) in
      if c <> 0 then c else
        match as1, as2 with
        | IRI iri1, IRI iri2 -> IRI.compare iri1 iri2
        | AnonymousIndividual id1, AnonymousIndividual id2 -> NodeID.compare id1 id2
        | _ -> invalid_arg "AnnotationSubject.compare"
    let equal as1 as2 =
      match as1, as2 with
      | IRI iri1, IRI iri2 -> IRI.equal iri1 iri2
      | AnonymousIndividual id1, AnonymousIndividual id2 -> NodeID.equal id1 id2
      | _ -> false
    let hash = function
      | IRI iri -> (hm 1) * (IRI.hash iri)
      | AnonymousIndividual id -> (hm 2) * (NodeID.hash id)
  end
  include CommonConsed.Make (Constructor)
end

(**================== Annotation Values ===================**)

module AnnotationValue = struct
  module Constructor = struct
    type t =
      | AnonymousIndividual of NodeID.t
      | IRI of IRI.t
      | Literal of Literal.t
    let precedence = function
      | AnonymousIndividual _ -> 0
      | IRI _ -> 1
      | Literal _ -> 2
    let compare av1 av2 =
      let c =	(precedence av1) - (precedence av2) in
      if c <> 0 then c else
        match av1, av2 with
        | AnonymousIndividual id1, AnonymousIndividual id2 -> NodeID.compare id1 id2
        | IRI iri1, IRI iri2 -> IRI.compare iri1 iri2
        | Literal lt1, Literal lt2 -> Literal.compare lt1 lt2
        | _ -> invalid_arg "AnnotationValue.compare"
    let equal av1 av2 =
      match av1, av2 with
      | AnonymousIndividual id1, AnonymousIndividual id2 -> NodeID.equal id1 id2
      | IRI iri1, IRI iri2 -> IRI.equal iri1 iri2
      | Literal lt1, Literal lt2 -> Literal.equal lt1 lt2
      | _ -> false
    let hash = function
      | AnonymousIndividual id -> (hm 1) * (NodeID.hash id)
      | IRI iri -> (hm 2) * (IRI.hash iri)
      | Literal lt -> (hm 3) * (Literal.hash lt)
  end
  include CommonConsed.Make (Constructor)
end

(**===================== Annotations ======================**)

module Annotation = struct
  module Constructor = struct
    type t =
      | Annotation of t consed list * AnnotationProperty.t * AnnotationValue.t
    let compare an1 an2 =
      match an1, an2 with
      | Annotation (an_lst1, ap1, av1), Annotation (an_lst2, ap2, av2) ->
          C.compare3 (C.compare_lst Consed.compare) (AnnotationProperty.compare) (AnnotationValue.compare) an_lst1 ap1 av1 an_lst2 ap2 av2
    let equal an1 an2 =
      match an1, an2 with
      | Annotation (an_lst1, ap1, av1), Annotation (an_lst2, ap2, av2) ->
          C.equal3 (C.equal_lst Consed.equal) (AnnotationProperty.equal) (AnnotationValue.equal) an_lst1 ap1 av1 an_lst2 ap2 av2
    let hash = function
      | Annotation (an_lst, ap, av) -> C.hash3 (C.hash_lst Consed.hash) (AnnotationProperty.hash) (AnnotationValue.hash) an_lst ap av
  end
  include CommonConsed.Make (Constructor)
end

(**================== Annotation Axioms ===================**)

module AnnotationAxiom = struct
  module Constructor = struct
    type t =
      | AnnotationAssertion of Annotation.t list * AnnotationProperty.t * AnnotationSubject.t * AnnotationValue.t
      | SubAnnotationPropertyOf of Annotation.t list * AnnotationProperty.t * AnnotationProperty.t
      | AnnotationPropertyDomain of Annotation.t list * AnnotationProperty.t * IRI.t
      | AnnotationPropertyRange of Annotation.t list * AnnotationProperty.t * IRI.t
    let precedence = function
      | AnnotationAssertion _ -> 0
      | SubAnnotationPropertyOf _ -> 1
      | AnnotationPropertyDomain _ -> 2
      | AnnotationPropertyRange _ -> 3
    let compare aa1 aa2 =
      let c =	(precedence aa1) - (precedence aa2) in
      if c <> 0 then c else
        match aa1, aa2 with
        | AnnotationAssertion (aa_lst1, ap1, asb1, av1), AnnotationAssertion (aa_lst2, ap2, asb2, av2) ->
            C.compare4 (C.compare_lst Annotation.compare) (AnnotationProperty.compare)
              (AnnotationSubject.compare) (AnnotationValue.compare)
              aa_lst1 ap1 asb1 av1 aa_lst2 ap2 asb2 av2
        | SubAnnotationPropertyOf (aa_lst1, sap1, ap1), SubAnnotationPropertyOf (aa_lst2, sap2, ap2) ->
            C.compare3 (C.compare_lst Annotation.compare) (AnnotationProperty.compare) (AnnotationProperty.compare)
              aa_lst1 sap1 ap1 aa_lst2 sap2 ap2
        | AnnotationPropertyDomain (aa_lst1, ap1, iri1), AnnotationPropertyDomain (aa_lst2, ap2, iri2) ->
            C.compare3 (C.compare_lst Annotation.compare) (AnnotationProperty.compare) (IRI.compare)
              aa_lst1 ap1 iri1 aa_lst2 ap2 iri2
        | AnnotationPropertyRange (aa_lst1, ap1, iri1), AnnotationPropertyRange (aa_lst2, ap2, iri2) ->
            C.compare3 (C.compare_lst Annotation.compare) (AnnotationProperty.compare) (IRI.compare)
              aa_lst1 ap1 iri1 aa_lst2 ap2 iri2
        | _ -> invalid_arg "AnnotationAxiom.compare"
    let equal aa1 aa2 =
      match aa1, aa2 with
      | AnnotationAssertion (aa_lst1, ap1, asb1, av1), AnnotationAssertion (aa_lst2, ap2, asb2, av2) ->
          C.equal4 (C.equal_lst Annotation.equal) (AnnotationProperty.equal)
            (AnnotationSubject.equal) (AnnotationValue.equal)
            aa_lst1 ap1 asb1 av1 aa_lst2 ap2 asb2 av2
      | SubAnnotationPropertyOf (aa_lst1, sap1, ap1), SubAnnotationPropertyOf (aa_lst2, sap2, ap2) ->
          C.equal3 (C.equal_lst Annotation.equal) (AnnotationProperty.equal) (AnnotationProperty.equal)
            aa_lst1 sap1 ap1 aa_lst2 sap2 ap2
      | AnnotationPropertyDomain (aa_lst1, ap1, iri1), AnnotationPropertyDomain (aa_lst2, ap2, iri2) ->
          C.equal3 (C.equal_lst Annotation.equal) (AnnotationProperty.equal) (IRI.equal)
            aa_lst1 ap1 iri1 aa_lst2 ap2 iri2
      | AnnotationPropertyRange (aa_lst1, ap1, iri1), AnnotationPropertyRange (aa_lst2, ap2, iri2) ->
          C.equal3 (C.equal_lst Annotation.equal) (AnnotationProperty.equal) (IRI.equal)
            aa_lst1 ap1 iri1 aa_lst2 ap2 iri2
      | _ -> false
    let hash = function
      | AnnotationAssertion (aa_lst, ap, asb, av) ->
          C.hash4 (C.hash_lst Annotation.hash) (AnnotationProperty.hash)
            (AnnotationSubject.hash) (AnnotationValue.hash) aa_lst ap asb av
      | SubAnnotationPropertyOf (aa_lst, sap, ap) ->
          C.hash3 (C.hash_lst Annotation.hash) (AnnotationProperty.hash) (AnnotationProperty.hash)
            aa_lst sap ap
      | AnnotationPropertyDomain (aa_lst, ap, iri) ->
          C.hash3 (C.hash_lst Annotation.hash) (AnnotationProperty.hash) (IRI.hash) aa_lst ap iri
      | AnnotationPropertyRange (aa_lst, ap, iri) ->
          C.hash3 (C.hash_lst Annotation.hash) (AnnotationProperty.hash) (IRI.hash) aa_lst ap iri
  end
  include CommonConsed.Make (Constructor)
end