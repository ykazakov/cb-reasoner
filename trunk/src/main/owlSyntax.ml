(* A module defining the constructors for concepts, roles, and axioms of   *)
(* ontologies                                                              *)

open CommonTypes
open Consed.T

(* auxiliary function for computing hash multipliers *)
let hm i = max_int - i

(**================= Common Interfaces ==================**)

module Common = struct
  
  module type S = sig
    type elt
    type t = elt consed
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
  
  module Make(H: OrderedHashedType): (S with type elt = H.t) = struct
    type elt = H.t
    module T = struct
      type t = elt consed           
      let compare x y = compare x.tag y.tag
      let hash x = x.tag      
      let equal = (==)
      let right x y z = x.tag lxor y.tag > x.tag lxor z.tag
    end
    include T
    module OT = struct
      type t = elt consed
      let compare x y = H.compare x.data y.data
    end
    module HT = struct
      include T
      type elt = t
      let key t = t
    end
    module C = Consed.Make (H)
    let cons = C.cons
    module Hashtbl = Chashmap.Make (H)
    module OSet = Set.Make (OT)
    module OMap = Map.Make (OT)
    module Set = Cset.Make (H)
    module Map = Cmap.Make (H)
    module HMap = Chashmap.Make (H)
    module HSet = Hashsetlp.Make (HT)    
  end
end

(* auxiliary functions on conced values *)
module C = struct
  (** comparison functions *)
  let compare x1 x2 = x1.tag - x2.tag
  let compare2 x1 y1 x2 y2 =
    let c = compare x1 x2 in
    if c <> 0 then c else compare y1 y2
  let compare3 x1 y1 z1 x2 y2 z2 =
    let c = compare x1 x2 in
    if c <> 0 then c else compare2 y1 z1 y2 z2
  let rec compare_lst lst1 lst2 =
    match lst1, lst2 with
    | hd1 :: tl1, hd2 :: tl2 ->
        let c = compare hd1 hd2 in
        if c <> 0 then c else compare_lst tl1 tl2
    | [], [] -> 0
    | [], _ -> - 1
    | _ -> 1
  let compare_n2o n1 x1 o1 n2 x2 o2 =
    let c = n1 - n2 in
    if c <> 0 then c else
      let c = compare x1 x2 in
      if c <> 0 then c else
        match o1, o2 with
        | None, None -> 0
        | Some y1, Some y2 -> compare y1 y2
        | None, _ -> - 1
        | _, None -> 1
  
  (** equality fucntions *)
  (* !!! important: use the physical equality for correct hash consing !!! *)
  let equal = (==)
  let equal2 x1 y1 x2 y2 =
    (equal x1 x2) && (equal y1 y2)
  let equal3 x1 y1 z1 x2 y2 z2 =
    (equal x1 x2) && (equal2 y1 z1 y2 z2)
  let equal_lst lst1 lst2 =
    try List.for_all2 (equal) lst1 lst2
    with Invalid_argument _ -> false
  let equal_n2o n1 x1 o1 n2 x2 o2 =
    n1 == n2 && equal x1 x2 &&
    match o1, o2 with
    | None, None -> true
    | Some y1, Some y2 -> equal y1 y2
    | _ -> false
  
  (** hash functions *)
  let hash x = x.tag
  let hash2 x y = (max_int - 1) * x.tag + (max_int - 2) * y.tag
  let hash3 x y z =
    (max_int - 1) * x.tag + (max_int - 2) * y.tag + (max_int - 3) * z.tag
  let hash_lst lst =
    let rec hash_lst_aux i = function
      | [] -> 0
      | hd :: tl -> (max_int - i) * hd.tag + hash_lst_aux (i + 1) tl
    in hash_lst_aux 0 lst
  let hash_n2o n x o =
    (max_int - 1) * n + (max_int - 2) * x.tag +
    match o with
    | None -> (max_int - 3)
    | Some y -> (max_int - 4) * y.tag
end

(**================= Object Properties ==================**)

module ObjectProperty = struct
  module Constructor = struct
    type t =
      | ObjectPropertyIRI of string
      | TopObjectProperty
      | BottomObjectProperty
    let precedence = function
      | ObjectPropertyIRI _ -> 0
      | TopObjectProperty -> 1
      | BottomObjectProperty -> 2
    let compare ope1 ope2 =
      let c = (precedence ope1) - (precedence ope2) in
      if c <> 0 then c else
        match ope1, ope2 with
        | ObjectPropertyIRI iri1, ObjectPropertyIRI iri2 -> String.compare iri1 iri2
        | TopObjectProperty, TopObjectProperty -> 0
        | BottomObjectProperty, BottomObjectProperty -> 0
        | _ -> invalid_arg "ObjectProperty.compare"
    let equal ope1 ope2 =
      match ope1, ope2 with
      | ObjectPropertyIRI iri1, ObjectPropertyIRI iri2 -> iri1 = iri2
      | TopObjectProperty, TopObjectProperty -> true
      | BottomObjectProperty, BottomObjectProperty -> true
      | _ -> false
    let hash = function
      | ObjectPropertyIRI iri -> (hm 1) * (Hashtbl.hash iri)
      | TopObjectProperty -> (hm 2)
      | BottomObjectProperty -> (hm 3)
  end
  include Common.Make (Constructor)
end;;

(**====================== Classes =======================**)

module Class = struct
  module Constructor = struct
    type t =
      | ClassIRI of string
      | Thing
      | Nothing
    let precedence = function
      | ClassIRI _ -> 0
      | Thing -> 1
      | Nothing -> 2
    let compare c1 c2 =
      let c = (precedence c1) - (precedence c2) in
      if c <> 0 then c else
        match c1, c2 with
        | ClassIRI iri1, ClassIRI iri2 -> String.compare iri1 iri2
        | Thing, Thing -> 0
        | Nothing, Nothing -> 0
        | _ -> invalid_arg "Class.compare"
    let equal c1 c2 =
      match c1, c2 with
      | ClassIRI iri1, ClassIRI iri2 -> iri1 = iri2
      | Thing, Thing -> true
      | Nothing, Nothing -> true
      | _ -> false
    let hash = function
      | ClassIRI iri -> (hm 1) * (Hashtbl.hash iri)
      | Thing -> (hm 2)
      | Nothing -> (hm 3)
  end
  include Common.Make (Constructor)
end;;

(**==================== Individuals =====================**)

module Individual = struct
  module Constructor = struct
    type t =
      | IndividualIRI of string
    let compare i1 i2 =
      match i1, i2 with
      | IndividualIRI iri1, IndividualIRI iri2 -> String.compare iri1 iri2
    let equal i1 i2 =
      match i1, i2 with
      | IndividualIRI iri1, IndividualIRI iri2 -> iri1 = iri2
    let hash = function
      | IndividualIRI iri -> (hm 1) * (Hashtbl.hash iri)
  end
  include Common.Make (Constructor)
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
        | ObjectProperty ar1, ObjectProperty ar2 -> C.compare ar1 ar2
        | InverseObjectProperty ar1, InverseObjectProperty ar2 -> C.compare ar1 ar2
        | _ -> invalid_arg "ObjectPropertyExpression.compare"
    let equal op1 op2 =
      match op1, op2 with
      | ObjectProperty ar1, ObjectProperty ar2 -> C.equal ar1 ar2
      | InverseObjectProperty ar1, InverseObjectProperty ar2 -> C.equal ar1 ar2
      | _ -> false
    let hash = function
      | ObjectProperty ar -> (hm 1) * (C.hash ar)
      | InverseObjectProperty ar -> (hm 2) * (C.hash ar)
  end
  include Common.Make (Constructor)
end;;

(**==================== Class Expressions ==================**)

module ClassExpression = struct
  module Constructor = struct
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
    let precedence = function
      | Class _ -> 0
      | ObjectIntersectionOf _ -> 1
      | ObjectUnionOf _ -> 2
      | ObjectComplementOf _ -> 3
      | ObjectOneOf _ -> 4
      | ObjectSomeValuesFrom _ -> 5
      | ObjectAllValuesFrom _ -> 6
      | ObjectHasValue _ -> 7
      | ObjectHasSelf _ -> 8
      | ObjectMinCardinality _ -> 9
      | ObjectMaxCardinality _ -> 10
      | ObjectExactCardinality _ -> 11
      | DataSomeValuesFrom -> 12
      | DataAllValuesFrom -> 13
      | DataHasValue -> 14
      | DataMinCardinality -> 15
      | DataMaxCardinality -> 16
      | DataExactCardinality -> 17
    let compare ce1 ce2 =
      let c = (precedence ce1) - (precedence ce2) in
      if c <> 0 then c else
        match ce1, ce2 with
        | Class c1, Class c2 -> C.compare c1 c2
        | ObjectIntersectionOf ce_set1, ObjectIntersectionOf ce_set2 -> Cset.compare ce_set1 ce_set2
        | ObjectUnionOf ce_set1, ObjectUnionOf ce_set2 -> Cset.compare ce_set1 ce_set2
        | ObjectComplementOf de1, ObjectComplementOf de2 -> C.compare de1 de2
        | ObjectOneOf i_set1, ObjectOneOf i_set2 -> Individual.Set.compare i_set1 i_set2
        | ObjectSomeValuesFrom (ope1, de1), ObjectSomeValuesFrom (ope2, de2) -> C.compare2 ope1 de1 ope2 de2
        | ObjectAllValuesFrom (ope1, de1), ObjectAllValuesFrom (ope2, de2) -> C.compare2 ope1 de1 ope2 de2
        | ObjectHasValue (ope1, i1), ObjectHasValue (ope2, i2) -> C.compare2 ope1 i1 ope2 i2
        | ObjectHasSelf ope1, ObjectHasSelf ope2 -> C.compare ope1 ope2
        | ObjectMinCardinality (n1, ope1, ceo1), ObjectMinCardinality (n2, ope2, ceo2) -> C.compare_n2o n1 ope1 ceo1 n2 ope2 ceo2
        | ObjectMaxCardinality (n1, ope1, ceo1), ObjectMaxCardinality (n2, ope2, ceo2) -> C.compare_n2o n1 ope1 ceo1 n2 ope2 ceo2
        | ObjectExactCardinality (n1, ope1, ceo1), ObjectExactCardinality (n2, ope2, ceo2) -> C.compare_n2o n1 ope1 ceo1 n2 ope2 ceo2
        | DataSomeValuesFrom, DataSomeValuesFrom -> 0
        | DataAllValuesFrom, DataAllValuesFrom -> 0
        | DataHasValue, DataHasValue -> 0
        | DataMinCardinality, DataMinCardinality -> 0
        | DataMaxCardinality, DataMaxCardinality -> 0
        | DataExactCardinality, DataExactCardinality -> 0
        | _ -> invalid_arg "ClassExpression.compare"
    let equal c1 c2 =
      match c1, c2 with
      | Class c1, Class c2 -> C.equal c1 c2
      | ObjectIntersectionOf c_set1, ObjectIntersectionOf c_set2 -> Cset.equal c_set1 c_set2
      | ObjectUnionOf c_set1, ObjectUnionOf c_set2 -> Cset.equal c_set1 c_set2
      | ObjectComplementOf de1, ObjectComplementOf de2 -> C.equal de1 de2
      | ObjectOneOf i_set1, ObjectOneOf i_set2 -> Individual.Set.equal i_set1 i_set2
      | ObjectSomeValuesFrom (ope1, de1), ObjectSomeValuesFrom (ope2, de2) -> C.equal2 ope1 de1 ope2 de2
      | ObjectAllValuesFrom (ope1, de1), ObjectAllValuesFrom (ope2, de2) -> C.equal2 ope1 de1 ope2 de2
      | ObjectHasValue (ope1, i1), ObjectHasValue (ope2, i2) -> C.equal2 ope1 i1 ope2 i2
      | ObjectHasSelf ope1, ObjectHasSelf ope2 -> C.equal ope1 ope2
      | ObjectMinCardinality (n1, ope1, ceo1), ObjectMinCardinality (n2, ope2, ceo2) -> C.equal_n2o n1 ope1 ceo1 n2 ope2 ceo2
      | ObjectMaxCardinality (n1, ope1, ceo1), ObjectMaxCardinality (n2, ope2, ceo2) -> C.equal_n2o n1 ope1 ceo1 n2 ope2 ceo2
      | ObjectExactCardinality (n1, ope1, ceo1), ObjectExactCardinality (n2, ope2, ceo2) -> C.equal_n2o n1 ope1 ceo1 n2 ope2 ceo2
      | DataSomeValuesFrom, DataSomeValuesFrom -> true
      | DataAllValuesFrom, DataAllValuesFrom -> true
      | DataHasValue, DataHasValue -> true
      | DataMinCardinality, DataMinCardinality -> true
      | DataMaxCardinality, DataMaxCardinality -> true
      | DataExactCardinality, DataExactCardinality -> true
      | _ -> false
    let hash = function
      | Class c -> (hm 1) * (C.hash c)
      | ObjectIntersectionOf c_set -> (hm 2) * (Cset.hash c_set)
      | ObjectUnionOf c_set -> (hm 3) * (Cset.hash c_set)
      | ObjectComplementOf c -> (hm 4) * (C.hash c)
      | ObjectOneOf i_set -> (hm 5) * (Cset.hash i_set)
      | ObjectSomeValuesFrom (ope, de) -> (hm 6) * (C.hash2 ope de)
      | ObjectAllValuesFrom (ope, de) -> (hm 7) * (C.hash2 ope de)
      | ObjectHasValue (ope, i) -> (hm 8) * (C.hash2 ope i)
      | ObjectHasSelf ope -> (hm 9) * (C.hash ope)
      | ObjectMinCardinality (n, ope, ceo) -> (hm 10) * (C.hash_n2o n ope ceo)
      | ObjectMaxCardinality (n, ope, ceo) -> (hm 11) * (C.hash_n2o n ope ceo)
      | ObjectExactCardinality (n, ope, ceo) -> (hm 12) * (C.hash_n2o n ope ceo)
      | DataSomeValuesFrom -> (hm 13)
      | DataAllValuesFrom -> (hm 14)
      | DataHasValue -> (hm 15)
      | DataMinCardinality -> (hm 16)
      | DataMaxCardinality -> (hm 17)
      | DataExactCardinality -> (hm 18)
  end
  include Common.Make (Constructor)
end

(**================= Object Property Axioms ================**)

module ObjectPropertyAxiom = struct
  module Constructor = struct
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
    let precedence = function
      | SubObjectPropertyOf _ -> 0
      | EquivalentObjectProperties _ -> 1
      | DisjointObjectProperties -> 2
      | InverseObjectProperties _ -> 3
      | ObjectPropertyDomain -> 4
      | ObjectPropertyRange -> 5
      | FunctionalObjectProperty _ -> 6
      | InverseFunctionalObjectProperty _ -> 7
      | ReflexiveObjectProperty -> 8
      | IrreflexiveObjectProperty -> 9
      | SymmetricObjectProperty -> 10
      | AsymmetricObjectProperty -> 11
      | TransitiveObjectProperty _ -> 12
    let compare ax1 ax2 =
      let c =	(precedence ax1) - (precedence ax2) in
      if c <> 0 then c else
        match ax1, ax2 with
        | SubObjectPropertyOf (ope_ch1, ope1), SubObjectPropertyOf (ope_ch2, ope2) ->
            let c = C.compare_lst ope_ch1 ope_ch2 in
            if c <> 0 then C.compare ope1 ope2 else c
        | EquivalentObjectProperties (ope_set1), EquivalentObjectProperties (ope_set2) -> Cset.compare ope_set1 ope_set2
        | DisjointObjectProperties, DisjointObjectProperties -> 0
        | InverseObjectProperties (ope11, ope12), InverseObjectProperties (ope21, ope22) -> C.compare2 ope11 ope12 ope21 ope22
        | ObjectPropertyDomain, ObjectPropertyDomain -> 0
        | ObjectPropertyRange, ObjectPropertyRange -> 0
        | FunctionalObjectProperty ope1, FunctionalObjectProperty ope2 -> C.compare ope1 ope2
        | InverseFunctionalObjectProperty ope1, InverseFunctionalObjectProperty ope2 -> C.compare ope1 ope2
        | ReflexiveObjectProperty, ReflexiveObjectProperty -> 0
        | IrreflexiveObjectProperty, IrreflexiveObjectProperty -> 0
        | SymmetricObjectProperty, SymmetricObjectProperty -> 0
        | AsymmetricObjectProperty, AsymmetricObjectProperty -> 0
        | TransitiveObjectProperty ope1, TransitiveObjectProperty ope2 -> C.compare ope1 ope2
        | _ -> invalid_arg "ObjectPropertyAxiom.compare"
    let equal ax1 ax2 =
      match ax1, ax2 with
      | SubObjectPropertyOf (ope_ch1, ope1), SubObjectPropertyOf (ope_ch2, ope2) ->
          C.equal_lst ope_ch1 ope_ch2 && C.equal ope1 ope2
      | EquivalentObjectProperties (ope_set1), EquivalentObjectProperties (ope_set2) -> Cset.equal ope_set1 ope_set2
      | DisjointObjectProperties, DisjointObjectProperties -> true
      | InverseObjectProperties (ope11, ope12), InverseObjectProperties (ope21, ope22) -> C.equal2 ope11 ope12 ope21 ope22
      | ObjectPropertyDomain, ObjectPropertyDomain -> true
      | ObjectPropertyRange, ObjectPropertyRange -> true
      | FunctionalObjectProperty ope1, FunctionalObjectProperty ope2 -> C.equal ope1 ope2
      | InverseFunctionalObjectProperty ope1, InverseFunctionalObjectProperty ope2 -> C.equal ope1 ope2
      | ReflexiveObjectProperty, ReflexiveObjectProperty -> true
      | IrreflexiveObjectProperty, IrreflexiveObjectProperty -> true
      | SymmetricObjectProperty, SymmetricObjectProperty -> true
      | AsymmetricObjectProperty, AsymmetricObjectProperty -> true
      | TransitiveObjectProperty ope1, TransitiveObjectProperty ope2 -> C.equal ope1 ope2
      | _ -> false
    let hash = function
      | SubObjectPropertyOf (ope_ch, ope) -> (hm 1) * ( (hm 1) * (C.hash_lst ope_ch) + (hm 2) * (C.hash ope) )
      | EquivalentObjectProperties (ope_set) -> (hm 2) * (Cset.hash ope_set)
      | DisjointObjectProperties -> (hm 3)
      | InverseObjectProperties (ope1, ope2) -> (hm 4) * (C.hash2 ope1 ope2)
      | ObjectPropertyDomain -> (hm 5)
      | ObjectPropertyRange -> (hm 6)
      | FunctionalObjectProperty ope -> (hm 7) * (C.hash ope)
      | InverseFunctionalObjectProperty ope -> (hm 8) * (C.hash ope)
      | ReflexiveObjectProperty -> (hm 9)
      | IrreflexiveObjectProperty -> (hm 10)
      | SymmetricObjectProperty -> (hm 11)
      | AsymmetricObjectProperty -> (hm 12)
      | TransitiveObjectProperty ope -> (hm 13) * (C.hash ope)
  end
  include Common.Make (Constructor)
end

(**================= Class Expression Axioms ===============**)

module ClassExpressionAxiom = struct
  module Constructor = struct
    type t =
      | SubClassOf of ClassExpression.t * ClassExpression.t
      | EquivalentClasses of ClassExpression.t Cset.t
      | DisjointClasses
      | DisjointUnion
    let precedence = function
      | SubClassOf _ -> 0
      | EquivalentClasses _ -> 1
      | DisjointClasses -> 2
      | DisjointUnion -> 3
    let compare ax1 ax2 =
      let c =	(precedence ax1) - (precedence ax2) in
      if c <> 0 then c else
        match ax1, ax2 with
        | SubClassOf (c11, c12), SubClassOf (c21, c22) -> C.compare2 c11 c12 c21 c22
        | EquivalentClasses (c_set1), EquivalentClasses (c_set2) -> Cset.compare c_set1 c_set2
        | DisjointClasses, DisjointClasses -> 0
        | DisjointUnion, DisjointUnion -> 0
        | _ -> invalid_arg "ClassExpressionAxiom.compare"
    let equal ax1 ax2 =
      match ax1, ax2 with
      | SubClassOf (c11, c12), SubClassOf (c21, c22) -> C.equal2 c11 c12 c21 c22
      | EquivalentClasses (c_set1), EquivalentClasses (c_set2) -> Cset.equal c_set1 c_set2
      | DisjointClasses, DisjointClasses -> true
      | DisjointUnion, DisjointUnion -> true
      | _ -> false
    let hash = function
      | SubClassOf (c1, c2) -> (hm 1) * (C.hash2 c1 c2)
      | EquivalentClasses (c_set) -> (hm 2) * (Cset.hash c_set)
      | DisjointClasses -> (hm 3)
      | DisjointUnion -> (hm 4)
  end
  include Common.Make (Constructor)
end

(**====================== Assertions ======================**)

module Assertion = struct
  module Constructor = struct
    type t =
      | SameIndividual
      | DifferentIndividuals
      | ClassAssertion of ClassExpression.t * Individual.t
      | ObjectPropertyAssertion of ObjectPropertyExpression.t * Individual.t * Individual.t
      | NegativeObjectPropertyAssertion
      | DataPropertyAssertion
      | NegativeDataPropertyAssertion
    let precedence = function
      | SameIndividual -> 0
      | DifferentIndividuals -> 1
      | ClassAssertion _ -> 2
      | ObjectPropertyAssertion _ -> 3
      | NegativeObjectPropertyAssertion -> 4
      | DataPropertyAssertion -> 5
      | NegativeDataPropertyAssertion -> 6
    let compare ax1 ax2 =
      let c =	(precedence ax1) - (precedence ax2) in
      if c <> 0 then c else
        match ax1, ax2 with
        | SameIndividual, SameIndividual -> 0
        | DifferentIndividuals, DifferentIndividuals -> 0
        | ClassAssertion (c1, i1), ClassAssertion (c2, i2) -> C.compare2 c1 i1 c2 i2
        | ObjectPropertyAssertion (ope1, i11, i12), ObjectPropertyAssertion (ope2, i21, i22) ->
            C.compare3 ope1 i11 i12 ope2 i21 i22
        | NegativeObjectPropertyAssertion, NegativeObjectPropertyAssertion -> 0
        | DataPropertyAssertion, DataPropertyAssertion -> 0
        | NegativeDataPropertyAssertion, NegativeDataPropertyAssertion -> 0
        | _ -> invalid_arg "Assertion.compare"
    let equal ax1 ax2 =
      match ax1, ax2 with
      | SameIndividual, SameIndividual -> true
      | DifferentIndividuals, DifferentIndividuals -> true
      | ClassAssertion (c1, i1), ClassAssertion (c2, i2) -> C.equal2 c1 i1 c2 i2
      | ObjectPropertyAssertion (ope1, i11, i12), ObjectPropertyAssertion (ope2, i21, i22) ->
          C.equal3 ope1 i11 i12 ope2 i21 i22
      | NegativeObjectPropertyAssertion, NegativeObjectPropertyAssertion -> true
      | DataPropertyAssertion, DataPropertyAssertion -> true
      | NegativeDataPropertyAssertion, NegativeDataPropertyAssertion -> true
      | _ -> false
    let hash = function
      | SameIndividual -> (hm 1)
      | DifferentIndividuals -> (hm 2)
      | ClassAssertion (c, i) -> (hm 3) * (C.hash2 c i)
      | ObjectPropertyAssertion (ope, i1, i2) -> (hm 4) * (C.hash3 ope i1 i2)
      | NegativeObjectPropertyAssertion -> (hm 5)
      | DataPropertyAssertion -> (hm 6)
      | NegativeDataPropertyAssertion -> (hm 7)
  end
  include Common.Make (Constructor)
end
