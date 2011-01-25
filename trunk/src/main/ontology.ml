open Owl
open Consed
open Printf

module PC = Polarity.Counter

(**======================= ontology ========================**)

module OrderedClass =
struct
	type t = Class.t
	let compare c1 c2 = String.compare (Class.str_of c1) (Class.str_of c2)
end

module ClassMap = Map.Make (OrderedClass)
module ClassSet = Set.Make (OrderedClass)

type t = {
	
	(* ========== signature ========== *)
	mutable record_ObjectProperty : int ObjectProperty.Map.t;
	mutable count_ObjectPropertyIRI : int;
	mutable count_TopObjectProperty : PC.t;
	mutable count_BottomObjectProperty: PC.t;
	
	mutable record_Class : int ClassMap.t;
	mutable count_ClassIRI : int;
	mutable count_Thing : PC.t;
	mutable count_Nothing: PC.t;
	
	mutable record_Individual : int Individual.Map.t;
	mutable count_IndividualIRI : int;
	
	(* ========== structure ========== *)
	mutable record_ComplexObjectPropertyExpression : PC.t ObjectPropertyExpression.Map.t;
	mutable count_ObjectInverseOf : PC.t;
	
	mutable record_ComplexClassExpression : PC.t ClassExpression.Map.t;
	mutable count_ObjectIntersectionOf : PC.t;
	mutable count_ObjectUnionOf : PC.t;
	mutable count_ObjectComplementOf : PC.t;
	mutable count_ObjectOneOf : PC.t;
	mutable count_ObjectSomeValuesFrom : PC.t;
	mutable count_ObjectAllValuesFrom : PC.t;
	mutable count_ObjectHasValue : PC.t;
	mutable count_ObjectHasSelf : PC.t;
	mutable count_ObjectMinCardinality : PC.t;
	mutable count_ObjectMaxCardinality : PC.t;
	mutable count_ObjectExactCardinality : PC.t;
	mutable count_DataSomeValuesFrom : PC.t;
	mutable count_DataAllValuesFrom : PC.t;
	mutable count_DataHasValue : PC.t;
	mutable count_DataMinCardinality : PC.t;
	mutable count_DataMaxCardinality : PC.t;
	mutable count_DataExactCardinality : PC.t;
	
	(* ========== axioms ========== *)
	mutable record_ObjectPropertyAxiom : ObjectPropertyAxiom.Set.t;
	mutable count_SubPropertyOf : int;
	mutable count_EquivalentProperties : int;
	mutable count_InverseProperties : int;
	mutable count_FunctionalProperty : int;
	mutable count_InverseFunctionalProperty : int;
	mutable count_TransitiveProperty : int;
	
	mutable record_ClassAxiom : ClassAxiom.Set.t;
	mutable count_SubClassOf : int;
	mutable count_EquivalentClasses : int;
	
	mutable record_Assertion : Assertion.Set.t;
	mutable count_ClassAssertion : int;
	mutable count_PropertyAssertion : int;
}

(**=================== initialization ======================**)

let create () =	{

(* ========== signature ========== *)
	record_ObjectProperty = ObjectProperty.Map.empty;
	count_ObjectPropertyIRI = 0;
	count_TopObjectProperty = PC.zero;
	count_BottomObjectProperty = PC.zero;
	
	record_Class = ClassMap.empty;
	count_ClassIRI = 0;
	count_Thing = PC.zero;
	count_Nothing = PC.zero;
	
	record_Individual = Individual.Map.empty;
	count_IndividualIRI = 0;
	
	(* ========== structure ========== *)
	record_ComplexObjectPropertyExpression = ObjectPropertyExpression.Map.empty;
	count_ObjectInverseOf = PC.zero;
	
	record_ComplexClassExpression = ClassExpression.Map.empty;
	count_ObjectIntersectionOf = PC.zero;
	count_ObjectUnionOf = PC.zero;
	count_ObjectComplementOf = PC.zero;
	count_ObjectOneOf = PC.zero;
	count_ObjectSomeValuesFrom = PC.zero;
	count_ObjectAllValuesFrom = PC.zero;
	count_ObjectHasValue = PC.zero;
	count_ObjectHasSelf = PC.zero;
	count_ObjectMinCardinality = PC.zero;
	count_ObjectMaxCardinality = PC.zero;
	count_ObjectExactCardinality = PC.zero;
	count_DataSomeValuesFrom = PC.zero;
	count_DataAllValuesFrom = PC.zero;
	count_DataHasValue = PC.zero;
	count_DataMinCardinality = PC.zero;
	count_DataMaxCardinality = PC.zero;
	count_DataExactCardinality = PC.zero;
	
	(* ========== axioms ========== *)
	record_ObjectPropertyAxiom = ObjectPropertyAxiom.Set.empty;
	count_SubPropertyOf = 0;
	count_EquivalentProperties = 0;
	count_InverseProperties = 0;
	count_FunctionalProperty = 0;
	count_InverseFunctionalProperty = 0;
	count_TransitiveProperty = 0;
	
	record_ClassAxiom = ClassAxiom.Set.empty;
	count_SubClassOf = 0;
	count_EquivalentClasses = 0;
	
	record_Assertion = Assertion.Set.empty;
	count_ClassAssertion = 0;
	count_PropertyAssertion = 0;
}

(**====================== iterators ========================**)

let iter_record_ObjectProperty f ont =
	ObjectProperty.Map.iter f ont.record_ObjectProperty
let iter_record_Class f ont =
	ClassMap.iter f ont.record_Class
let iter_record_Individual f ont =
	Individual.Map.iter f ont.record_Individual

let iter_record_ComplexObjectPropertyExpression f ont =
	ObjectPropertyExpression.Map.iter f ont.record_ComplexObjectPropertyExpression
let iter_record_ComplexClassExpression f ont =
	ClassExpression.Map.iter f ont.record_ComplexClassExpression

let iter_record_ObjectPropertyAxiom f ont =
	ObjectPropertyAxiom.Set.iter f ont.record_ObjectPropertyAxiom
let iter_record_ClassAxiom f ont =
	ClassAxiom.Set.iter f ont.record_ClassAxiom
let iter_record_Assertion f ont =
	Assertion.Set.iter f ont.record_Assertion

(**============== statistical information ================**)

let has_positive_Nothing ont =
	PC.get_pos ont.count_Nothing > 0
let has_positive_ComplementOf ont =
	PC.get_pos ont.count_ObjectComplementOf > 0
let has_negative_Thing ont =
	PC.get_neg ont.count_Thing > 0

let total_ObjectPropertyIRI ont = ont.count_ObjectPropertyIRI
let total_ClassIRI ont = ont.count_ClassIRI
let total_IndividualIRI ont = ont.count_IndividualIRI

let count_TopObjectProperty ont = ont.count_TopObjectProperty
let count_BottomObjectProperty ont = ont.count_BottomObjectProperty
let count_Thing ont = ont.count_Thing
let count_Nothing ont = ont.count_Nothing

let count_ObjectInverseOf ont = ont.count_ObjectInverseOf
let total_ObjectInverseOf ont = PC.get_total ont.count_ObjectInverseOf

let count_ObjectIntersectionOf ont = ont.count_ObjectIntersectionOf
let count_ObjectUnionOf ont = ont.count_ObjectUnionOf
let count_ObjectComplementOf ont = ont.count_ObjectComplementOf
let count_ObjectOneOf ont = ont.count_ObjectOneOf
let count_ObjectSomeValuesFrom ont = ont.count_ObjectSomeValuesFrom
let count_ObjectAllValuesFrom ont = ont.count_ObjectAllValuesFrom
let count_ObjectHasValue ont = ont.count_ObjectHasValue
let count_ObjectHasSelf ont = ont.count_ObjectHasSelf
let count_ObjectMinCardinality ont = ont.count_ObjectMinCardinality
let count_ObjectMaxCardinality ont = ont.count_ObjectMaxCardinality
let count_ObjectExactCardinality ont = ont.count_ObjectExactCardinality
let count_DataSomeValuesFrom ont = ont.count_DataSomeValuesFrom
let count_DataAllValuesFrom ont = ont.count_DataAllValuesFrom
let count_DataHasValue ont = ont.count_DataHasValue
let count_DataMinCardinality ont = ont.count_DataMinCardinality
let count_DataMaxCardinality ont = ont.count_DataMaxCardinality
let count_DataExactCardinality ont = ont.count_DataExactCardinality

let total_ObjectIntersectionOf ont = PC.get_total ont.count_ObjectIntersectionOf
let total_ObjectUnionOf ont = PC.get_total ont.count_ObjectUnionOf
let total_ObjectComplementOf ont = PC.get_total ont.count_ObjectComplementOf
let total_ObjectOneOf ont = PC.get_total ont.count_ObjectOneOf
let total_ObjectSomeValuesFrom ont = PC.get_total ont.count_ObjectSomeValuesFrom
let total_ObjectAllValuesFrom ont = PC.get_total ont.count_ObjectAllValuesFrom
let total_ObjectHasValue ont = PC.get_total ont.count_ObjectHasValue
let total_ObjectHasSelf ont = PC.get_total ont.count_ObjectHasSelf
let total_ObjectMinCardinality ont = PC.get_total ont.count_ObjectMinCardinality
let total_ObjectMaxCardinality ont = PC.get_total ont.count_ObjectMaxCardinality
let total_ObjectExactCardinality ont = PC.get_total ont.count_ObjectExactCardinality
let total_DataSomeValuesFrom ont = PC.get_total ont.count_DataSomeValuesFrom
let total_DataAllValuesFrom ont = PC.get_total ont.count_DataAllValuesFrom
let total_DataHasValue ont = PC.get_total ont.count_DataHasValue
let total_DataMinCardinality ont = PC.get_total ont.count_DataMinCardinality
let total_DataMaxCardinality ont = PC.get_total ont.count_DataMaxCardinality
let total_DataExactCardinality ont = PC.get_total ont.count_DataExactCardinality

let total_SubPropertyOf ont = ont.count_SubPropertyOf
let total_EquivalentProperties ont = ont.count_EquivalentProperties
let total_InverseProperties ont = ont.count_InverseProperties
let total_FunctionalProperty ont = ont.count_FunctionalProperty
let total_InverseFunctionalProperty ont = ont.count_InverseFunctionalProperty
let total_TransitiveProperty ont = ont.count_TransitiveProperty
let total_ObjectPropertyAxiom ont =
	ont.count_SubPropertyOf +
	ont.count_EquivalentProperties +
	ont.count_InverseProperties +
	ont.count_FunctionalProperty +
	ont.count_InverseFunctionalProperty +
	ont.count_TransitiveProperty
let total_SubClassOf ont = ont.count_SubClassOf
let total_EquivalentClasses ont = ont.count_EquivalentClasses
let total_ClassAxiom ont =
	ont.count_SubClassOf +
	ont.count_EquivalentClasses
let total_ClassAssertion ont = ont.count_ClassAssertion
let total_PropertyAssertion ont = ont.count_PropertyAssertion
let total_Assertion ont =
	ont.count_ClassAssertion +
	ont.count_PropertyAssertion

(**==== insertion, computation of polarities and stats =====**)

let add_ObjectProperty_pc ont op pc =
	let module M = ObjectProperty.Map in
	let module C = ObjectProperty_Constructor in
	match op.data with
	| C.IRI _ -> (* adding only iris in the record *)
			ont.record_ObjectProperty <- M.add op
				(let count = try M.find op ont.record_ObjectProperty
						with Not_found ->
								ont.count_ObjectPropertyIRI <-
								succ ont.count_ObjectPropertyIRI;
								0
					in count + PC.get_total pc
				) ont.record_ObjectProperty;
	| C.TopObjectProperty -> ont.count_TopObjectProperty <-
			PC.sum ont.count_TopObjectProperty pc
	| C.BottomObjectProperty -> ont.count_BottomObjectProperty <-
			PC.sum ont.count_BottomObjectProperty pc
;;

let add_Class_pc ont c pc =
	let module M = ClassMap in
	let module C = Class_Constructor in
	match c with
	| C.IRI iri -> (* adding only iris in the record *)
			ont.record_Class <- M.add c
				(let count = try M.find c ont.record_Class
						with Not_found ->
								ont.count_ClassIRI <-
								succ ont.count_ClassIRI;
								0
					in count + PC.get_total pc
				) ont.record_Class
	| C.Thing -> ont.count_Thing <-
			PC.sum ont.count_Thing pc
	| C.Nothing -> ont.count_Nothing <-
			PC.sum ont.count_Nothing pc
;;

let add_Individual_pc ont i pc =
	let module M = Individual.Map in
	let module C = Individual_Constructor in
	(* adding only iris in the record *)
	ont.record_Individual <- M.add i
		(let count = try M.find i ont.record_Individual
				with Not_found ->
						ont.count_IndividualIRI <-
						succ ont.count_IndividualIRI;
						0
			in succ count
		) ont.record_Individual
;;

let rec add_ObjectPropertyExpression_pc ont ope pc =
	let module M = ObjectPropertyExpression.Map in
	let module C = ObjectPropertyExpression_Constructor in
	begin match ope.data with
		| C.ObjectProperty _ -> ()
		| _ -> ont.record_ComplexObjectPropertyExpression <-
				M.process ope (function
						| Some pc_old -> Some (PC.sum pc_old pc)
						| None -> Some pc
					) ont.record_ComplexObjectPropertyExpression;
	end;
	propagate_ObjectPropertyExpression_pc ont ope pc
and propagate_ObjectPropertyExpression_pc ont ope pc =
	let module M = ObjectPropertyExpression.Map in
	let module C = ObjectPropertyExpression_Constructor in
	match ope.data with
	| C.ObjectProperty op -> add_ObjectProperty_pc ont op pc
	| C.ObjectInverseOf op -> add_ObjectProperty_pc ont op pc;
			ont.count_ObjectInverseOf <-
			PC.sum ont.count_ObjectInverseOf pc
;;

let rec add_ClassExpression_pc ont ce pc =
	let module M = ClassExpression.Map in
	let module C = ClassExpression_Constructor in
	begin match ce.data with
		| C.Class _ -> ()
		| _ -> ont.record_ComplexClassExpression <-
				M.process ce (function
						| Some pc_old -> Some (PC.sum pc_old pc)
						| None -> Some pc
					) ont.record_ComplexClassExpression;
	end;
	propagate_ClassExpression_pc ont ce pc
and propagate_ClassExpression_pc ont ce pc =
	let module C = ClassExpression_Constructor in
	match ce.data with
	| C.Class c -> add_Class_pc ont c pc
	| C.ObjectIntersectionOf (ce, cce) ->
			add_ClassExpression_pc ont ce pc;
			add_ClassExpression_pc ont cce pc;
			ont.count_ObjectIntersectionOf <-
			PC.sum ont.count_ObjectIntersectionOf pc
	| C.ObjectIntersectionOfrec (ce, cce) ->
			add_ClassExpression_pc ont ce pc;
			add_ClassExpression_pc ont cce pc;
	| C.ObjectUnionOf (ce, cce) ->
			add_ClassExpression_pc ont ce pc;
			add_ClassExpression_pc ont cce pc;
			ont.count_ObjectUnionOf <-
			PC.sum ont.count_ObjectUnionOf pc
	| C.ObjectUnionOfrec (ce, cce) ->
			add_ClassExpression_pc ont ce pc;
			add_ClassExpression_pc ont cce pc;
	| C.ObjectComplementOf de ->
			add_ClassExpression_pc ont de (PC.invert pc);
			ont.count_ObjectComplementOf <-
			PC.sum ont.count_ObjectComplementOf pc
	| C.ObjectOneOf i_lst ->
			List.iter (fun i -> add_Individual_pc ont i pc) i_lst;
			ont.count_ObjectOneOf <-
			PC.sum ont.count_ObjectOneOf pc
	| C.ObjectSomeValuesFrom (ope, de) ->
			add_ObjectPropertyExpression_pc ont ope pc;
			add_ClassExpression_pc ont de pc;
			ont.count_ObjectSomeValuesFrom <-
			PC.sum ont.count_ObjectSomeValuesFrom pc
	| C.ObjectAllValuesFrom (ope, de) ->
			add_ObjectPropertyExpression_pc ont ope (PC.invert pc);
			add_ClassExpression_pc ont de pc;
			ont.count_ObjectAllValuesFrom <-
			PC.sum ont.count_ObjectAllValuesFrom pc
	| C.ObjectHasValue (ope, i) ->
			add_ObjectPropertyExpression_pc ont ope pc;
			add_Individual_pc ont i pc;
			ont.count_ObjectHasValue <-
			PC.sum ont.count_ObjectHasValue pc
	| C.ObjectHasSelf ope ->
			add_ObjectPropertyExpression_pc ont ope pc;
			ont.count_ObjectHasSelf <-
			PC.sum ont.count_ObjectHasSelf pc
	| C.ObjectMinCardinality (n, ope, ceo) ->
			add_ObjectPropertyExpression_pc ont ope pc;
			begin match ceo with
				| None -> ()
				| Some ce -> add_ClassExpression_pc ont ce pc;
			end;
			ont.count_ObjectMinCardinality <-
			PC.sum ont.count_ObjectMinCardinality pc
	| C.ObjectMaxCardinality (n, ope, ceo) ->
			add_ObjectPropertyExpression_pc ont ope (PC.invert pc);
			begin match ceo with
				| None -> ()
				| Some ce -> add_ClassExpression_pc ont ce (PC.invert pc);
			end;
			ont.count_ObjectMinCardinality <-
			PC.sum ont.count_ObjectMinCardinality pc
	| C.ObjectExactCardinality (n, ope, ceo) ->
			add_ObjectPropertyExpression_pc ont ope (PC.symm pc);
			begin match ceo with
				| None -> ()
				| Some ce -> add_ClassExpression_pc ont ce (PC.symm pc);
			end;
			ont.count_ObjectMinCardinality <-
			PC.sum ont.count_ObjectMinCardinality pc
	| _ -> ()
;;

let add_ObjectPropertyAxiom ont opa =
	let module S = ObjectPropertyAxiom.Set in
	let module C = ObjectPropertyAxiom_Constructor in
	if not (S.mem opa ont.record_ObjectPropertyAxiom) then
		begin
			ont.record_ObjectPropertyAxiom <- S.add opa
				ont.record_ObjectPropertyAxiom;
			match opa.data with
			| C.SubObjectPropertyOf (ope_ch, ope) ->
					List.iter (fun ope -> add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Negative)) ope_ch;
					add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Positive);
					ont.count_SubPropertyOf <- succ ont.count_SubPropertyOf
			| C.EquivalentObjectProperties (ope_lst) ->
					List.iter (fun ope -> add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Both)) ope_lst;
					ont.count_EquivalentProperties <- succ ont.count_EquivalentProperties
			| C.DisjointObjectProperties _ -> ()
			| C.InverseObjectProperties (ope1, ope2) ->
					add_ObjectPropertyExpression_pc ont ope1 (PC.to_elt Polarity.Both);
					add_ObjectPropertyExpression_pc ont ope2 (PC.to_elt Polarity.Both);
					ont.count_InverseProperties <- succ ont.count_InverseProperties
			| C.ObjectPropertyDomain _ -> ()
			| C.ObjectPropertyRange _ -> ()
			| C.FunctionalObjectProperty ope ->
					add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Negative);
					ont.count_FunctionalProperty <- succ ont.count_FunctionalProperty
			| C.InverseFunctionalObjectProperty ope ->
					add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Negative);
					ont.count_InverseFunctionalProperty <- succ ont.count_InverseFunctionalProperty
			| C.ReflexiveObjectProperty _ -> ()
			| C.IrreflexiveObjectProperty _ -> ()
			| C.SymmetricObjectProperty _ -> ()
			| C.AsymmetricObjectProperty _ -> ()
			| C.TransitiveObjectProperty ope ->
					add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Both);
					ont.count_TransitiveProperty <- succ ont.count_TransitiveProperty
		end
;;

let add_ClassAxiom ont cea =
	let module S = ClassAxiom.Set in
	let module C = ClassAxiom_Constructor in
	if not (S.mem cea ont.record_ClassAxiom) then
		begin
			ont.record_ClassAxiom <- S.add cea
				ont.record_ClassAxiom;
			match cea.data with
			| C.SubClassOf (ce1, ce2) ->
					add_ClassExpression_pc ont ce1 (PC.to_elt Polarity.Negative);
					add_ClassExpression_pc ont ce2 (PC.to_elt Polarity.Positive);
					ont.count_SubClassOf <- succ ont.count_SubClassOf
			| C.EquivalentClasses (c_lst) ->
					List.iter (fun ce -> add_ClassExpression_pc ont ce (PC.to_elt Polarity.Both)) c_lst;
					ont.count_EquivalentClasses <- succ ont.count_EquivalentClasses
			| C.DisjointClasses _ -> ()
			| C.DisjointUnion _ -> ()
		end
;;

let add_Assertion ont a =
	let module S = Assertion.Set in
	let module C = Assertion_Constructor in
	if not (S.mem a ont.record_Assertion) then
		begin
			ont.record_Assertion <- S.add a
				ont.record_Assertion;
			match a.data with
			| C.SameIndividual _ -> ()
			| C.DifferentIndividuals _ -> ()
			| C.ClassAssertion (ce, i) ->
					add_ClassExpression_pc ont ce (PC.to_elt Polarity.Positive);
					add_Individual_pc ont i Polarity.Negative;
			| C.ObjectPropertyAssertion (ope, i1, i2) ->
					add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Positive);
					add_Individual_pc ont i1 (PC.to_elt Polarity.Negative);
					add_Individual_pc ont i2 (PC.to_elt Polarity.Negative);
			| C.NegativeObjectPropertyAssertion _ -> ()
			| C.DataPropertyAssertion _ -> ()
			| C.NegativeDataPropertyAssertion _ -> ()
		end
;;

(** =============== printing various information ================== **)

let print_info ont ch_out =
	let width = 30 in
	let print item count =
		if count > 0 then
			fprintf ch_out "%-*s%6n\n" width item count
	in
	let print_pc item pc =
		if PC.get_total pc > 0 then
			fprintf ch_out "%-*s%6n%8n\n"
				width item (PC.get_pos pc) (PC.get_neg pc)
	in
	fprintf ch_out "%-*s\n" width "Ontology information:";
	fprintf ch_out "%s\n" (String.make width '=');
	fprintf ch_out "%-*s\n" width "Signature (element counts):";
	fprintf ch_out "%s\n" (String.make width '-');
	print "object properties:" (total_ObjectPropertyIRI ont);
	print "classes:" (total_ClassIRI ont);
	print "individuals" (total_IndividualIRI ont);
	fprintf ch_out "\n";
	fprintf ch_out "%-*s%6s%8s\n" width
		"Structure (expression counts):" "(pos)" "(neg)";
	fprintf ch_out "%s\n" (String.make width '-');
	print_pc "\"owl:Thing\":" (count_Thing ont);
	print_pc "\"owl:Nothing\":" (count_Nothing ont);
	print_pc "ObjectInverseOf:" (count_ObjectInverseOf ont);
	print_pc "ObjectIntersectionOf:" (count_ObjectIntersectionOf ont);
	print_pc "ObjectUnionOf:" (count_ObjectUnionOf ont);
	print_pc "ObjectComplementOf:" (count_ObjectComplementOf ont);
	print_pc "ObjectOneOf:" (count_ObjectOneOf ont);
	print_pc "ObjectSomeValuesFrom:" (count_ObjectSomeValuesFrom ont);
	print_pc "ObjectAllValuesFrom:" (count_ObjectAllValuesFrom ont);
	print_pc "ObjectHasValue:" (count_ObjectHasValue ont);
	print_pc "ObjectHasSelf:" (count_ObjectHasSelf ont);
	print_pc "ObjectMinCardinality:" (count_ObjectMinCardinality ont);
	print_pc "ObjectMaxCardinality:" (count_ObjectMaxCardinality ont);
	print_pc "ObjectExactCardinality:" (count_ObjectExactCardinality ont);
	print_pc "DataSomeValuesFrom:" (count_DataSomeValuesFrom ont);
	print_pc "DataAllValuesFrom:" (count_DataAllValuesFrom ont);
	print_pc "DataHasValue:" (count_DataHasValue ont);
	print_pc "DataMinCardinality:" (count_DataMinCardinality ont);
	print_pc "DataMaxCardinality:" (count_DataMaxCardinality ont);
	print_pc "DataExactCardinality:" (count_DataExactCardinality ont);
	fprintf ch_out "\n";
	fprintf ch_out "%-*s\n" width "Theory (axiom counts):";
	fprintf ch_out "%s\n" (String.make width '-');
	print "SubPropertyOf:" (total_SubPropertyOf ont);
	print "EquivalentProperties:" (total_EquivalentProperties ont);
	print "InverseProperties:" (total_InverseProperties ont);
	print "FunctionalProperty:" (total_FunctionalProperty ont);
	print "InverseFunctionalProperty:" (total_InverseFunctionalProperty ont);
	print "TransitiveProperty:" (total_TransitiveProperty ont);
	print "SubClassOf:" (total_SubClassOf ont);
	print "EquivalentClasses:" (total_EquivalentClasses ont);
	print "ClassAssertion:" (total_ClassAssertion ont);
	print "PropertyAssertion:" (total_PropertyAssertion ont);
	fprintf ch_out "\n";
;;