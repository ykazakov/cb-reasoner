open Owl
open Consed
open Printf

module PC = Polarity.Counter

(**======================= ontology ========================**)

(* functor for lexicographic comparison *)

module type OrderedMapSet = sig
	type t
	module Map : Map.S with type key = t
	module Set : Set.S with type elt = t
end

module MakeOrdered (M: sig type t val str_of : t -> string end)
: OrderedMapSet with type t = M.t = struct
	module T = struct
		type t = M.t
		let compare t1 t2 = String.compare (M.str_of t1) (M.str_of t2)
	end
	type t = T.t
	module Map = Map.Make (T)
	module Set = Set.Make (T)
end

module OrderedClass = MakeOrdered (Class)
module OrderedDatatype = MakeOrdered (Datatype)
module OrderedConstrainingFacet = MakeOrdered (ConstrainingFacet)
module OrderedObjectProperty = MakeOrdered (ObjectProperty)
module OrderedDataProperty = MakeOrdered (DataProperty)
module OrderedIndividual = MakeOrdered (Individual)
module OrderedLiteral = MakeOrdered (Literal)

(* currently supported constructors *)
let supported_Class_Constructor =
	let module C = Class_Constructor.Cases in
	let record = C.record_create None in
	C.record_set record C.Thing (Some Polarity.Both);
	C.record_set record C.Nothing (Some Polarity.Both);
	record
let supported_Datatype_Constructor =
	let module C = Datatype_Constructor.Cases in
	let record = C.record_create None in
	record
let supported_ConstrainingFacet_Constructor =
	let module C = ConstrainingFacet_Constructor.Cases in
	let record = C.record_create None in
	record
let supported_ObjectProperty_Constructor =
	let module C = ObjectProperty_Constructor.Cases in
	let record = C.record_create None in
	record
let supported_DataProperty_Constructor =
	let module C = DataProperty_Constructor.Cases in
	let record = C.record_create None in
	record
let supported_Individual_Constructor =
	let module C = Individual_Constructor.Cases in
	let record = C.record_create None in
	record
let supported_ClassExpression_Constructor =
	let module C = ClassExpression_Constructor.Cases in
	let record = C.record_create None in
	C.record_set record C.ObjectIntersectionOf (Some Polarity.Both);
	C.record_set record C.ObjectUnionOf (Some Polarity.Negative);
	C.record_set record C.ObjectComplementOf (Some Polarity.Positive);
	C.record_set record C.ObjectSomeValuesFrom (Some Polarity.Both);
	C.record_set record C.ObjectAllValuesFrom (Some Polarity.Positive);
	record	
let supported_ObjectPropertyExpression_Constructor =
	let module C = ObjectPropertyExpression_Constructor.Cases in
	let record = C.record_create None in
	C.record_set record C.ObjectInverseOf (Some Polarity.Both);
	record
(*|let supported_Declaration_Constructor =          *)
(*|  let module C = Declaration_Constructor.Cases in*)
(*|  let record = C.record_create false in          *)
(*|  C.record_set record C.Class true;              *)
(*|	C.record_set record C.ObjectProperty true;     *)
(*|  record	                                       *)
let supported_ClassAxiom_Constructor =
	let module C = ClassAxiom_Constructor.Cases in
	let record = C.record_create false in
	C.record_set record C.SubClassOf true;
	C.record_set record C.EquivalentClasses true;
	record
let supported_ObjectPropertyAxiom_Constructor =
	let module C = ObjectPropertyAxiom_Constructor.Cases in
	let record = C.record_create false in
	C.record_set record C.SubObjectPropertyOf true;
	C.record_set record C.EquivalentObjectProperties true;
	C.record_set record C.InverseObjectProperties true;
	C.record_set record C.FunctionalObjectProperty true;
	C.record_set record C.InverseFunctionalObjectProperty true;
	C.record_set record C.TransitiveObjectProperty true;
	record

type t = {
	(* ========== signature ========== *)
	mutable record_Class : PC.t OrderedClass.Map.t;
	mutable count_Class : int;
	mutable total_cases_Class : PC.t Class_Constructor.Cases.record;
	
	mutable record_Datatype : PC.t OrderedDatatype.Map.t;
	mutable count_Datatype : int;
	mutable total_cases_Datatype : PC.t Datatype_Constructor.Cases.record;
	
	mutable record_ConstrainingFacet : PC.t OrderedConstrainingFacet.Map.t;
	mutable count_ConstrainingFacet : int;
	mutable total_cases_ConstrainingFacet : PC.t ConstrainingFacet_Constructor.Cases.record;
	
	mutable record_ObjectProperty : PC.t OrderedObjectProperty.Map.t;
	mutable count_ObjectProperty : int;
	mutable total_cases_ObjectProperty : PC.t ObjectProperty_Constructor.Cases.record;
	
	mutable record_DataProperty : PC.t OrderedDataProperty.Map.t;
	mutable count_DataProperty : int;
	mutable total_cases_DataProperty : PC.t DataProperty_Constructor.Cases.record;
	
	mutable record_Individual : PC.t OrderedIndividual.Map.t;
	mutable count_Individual : int;
	mutable total_cases_Individual : PC.t Individual_Constructor.Cases.record;
	
	mutable record_Literal : PC.t Literal.Map.t;
	mutable count_Literal : int;
	mutable total_cases_Literal : PC.t Literal_Constructor.Cases.record;
	
	(* ========== structure ========== *)
	
	mutable record_ObjectPropertyExpression : PC.t ObjectPropertyExpression.Map.t;
	mutable count_ObjectPropertyExpression : int;
	mutable total_cases_ObjectPropertyExpression : PC.t ObjectPropertyExpression_Constructor.Cases.record;
	
	mutable record_DataRange : PC.t DataRange.Map.t;
	mutable count_DataRange : int;
	mutable total_cases_DataRange : PC.t DataRange_Constructor.Cases.record;
	
	mutable record_ClassExpression : PC.t ClassExpression.Map.t;
	mutable count_ClassExpression : int;
	mutable total_cases_ClassExpression : PC.t ClassExpression_Constructor.Cases.record;
	
	(* ========== axioms ========== *)
	mutable record_Declaration : int Declaration.Map.t;
  mutable count_Declaration : int;
  mutable total_cases_Declaration : int Declaration_Constructor.Cases.record;
	
	mutable record_ClassAxiom : int ClassAxiom.Map.t;
	mutable count_ClassAxiom : int;
	mutable total_cases_ClassAxiom : int ClassAxiom_Constructor.Cases.record;
	
	mutable record_ObjectPropertyAxiom : int ObjectPropertyAxiom.Map.t;
	mutable count_ObjectPropertyAxiom : int;
	mutable total_cases_ObjectPropertyAxiom : int ObjectPropertyAxiom_Constructor.Cases.record;
	
	mutable record_DataPropertyAxiom : int DataPropertyAxiom.Map.t;
	mutable count_DataPropertyAxiom : int;
	mutable total_cases_DataPropertyAxiom : int DataPropertyAxiom_Constructor.Cases.record;
	
	mutable record_DatatypeDefinition : int DatatypeDefinition.Map.t;
	mutable count_DatatypeDefinition : int;
	mutable total_cases_DatatypeDefinition : int DatatypeDefinition_Constructor.Cases.record;
	
	mutable record_Key : int Key.Map.t;
	mutable count_Key : int;
	mutable total_cases_Key : int Key_Constructor.Cases.record;
	
	mutable record_Assertion : int Assertion.Map.t;
	mutable count_Assertion : int;
	mutable total_cases_Assertion : int Assertion_Constructor.Cases.record;
}

(**=================== initialization ======================**)

let create () =	{

(* ========== signature ========== *)
	record_Class = OrderedClass.Map.empty;
	count_Class = 0;
	total_cases_Class = Class_Constructor.Cases.record_create PC.zero;
	
	record_Datatype = OrderedDatatype.Map.empty;
	count_Datatype = 0;
	total_cases_Datatype = Datatype_Constructor.Cases.record_create PC.zero;
	
	record_ConstrainingFacet = OrderedConstrainingFacet.Map.empty;
	count_ConstrainingFacet = 0;
	total_cases_ConstrainingFacet = ConstrainingFacet_Constructor.Cases.record_create PC.zero;
	
	record_ObjectProperty = OrderedObjectProperty.Map.empty;
	count_ObjectProperty = 0;
	total_cases_ObjectProperty = ObjectProperty_Constructor.Cases.record_create PC.zero;
	
	record_DataProperty = OrderedDataProperty.Map.empty;
	count_DataProperty = 0;
	total_cases_DataProperty = DataProperty_Constructor.Cases.record_create PC.zero;
	
	record_Individual = OrderedIndividual.Map.empty;
	count_Individual = 0;
	total_cases_Individual = Individual_Constructor.Cases.record_create PC.zero;
	
	record_Literal = Literal.Map.empty;
	count_Literal = 0;
	total_cases_Literal = Literal_Constructor.Cases.record_create PC.zero;
	
	(* ========== structure ========== *)
	record_ObjectPropertyExpression = ObjectPropertyExpression.Map.empty;
	count_ObjectPropertyExpression = 0;
	total_cases_ObjectPropertyExpression = ObjectPropertyExpression_Constructor.Cases.record_create PC.zero;
	
	record_DataRange = DataRange.Map.empty;
	count_DataRange = 0;
	total_cases_DataRange = DataRange_Constructor.Cases.record_create PC.zero;
	
	record_ClassExpression = ClassExpression.Map.empty;
	count_ClassExpression = 0;
	total_cases_ClassExpression = ClassExpression_Constructor.Cases.record_create PC.zero;
	
	(* ========== axioms ========== *)
	record_Declaration = Declaration.Map.empty;
  count_Declaration = 0;
  total_cases_Declaration = Declaration_Constructor.Cases.record_create 0;
	
	record_ClassAxiom = ClassAxiom.Map.empty;
	count_ClassAxiom = 0;
	total_cases_ClassAxiom = ClassAxiom_Constructor.Cases.record_create 0;
	
	record_ObjectPropertyAxiom = ObjectPropertyAxiom.Map.empty;
	count_ObjectPropertyAxiom = 0;
	total_cases_ObjectPropertyAxiom = ObjectPropertyAxiom_Constructor.Cases.record_create 0;
	
	record_DataPropertyAxiom = DataPropertyAxiom.Map.empty;
	count_DataPropertyAxiom = 0;
	total_cases_DataPropertyAxiom = DataPropertyAxiom_Constructor.Cases.record_create 0;
	
	record_DatatypeDefinition = DatatypeDefinition.Map.empty;
	count_DatatypeDefinition = 0;
	total_cases_DatatypeDefinition = DatatypeDefinition_Constructor.Cases.record_create 0;
	
	record_Key = Key.Map.empty;
	count_Key = 0;
	total_cases_Key = Key_Constructor.Cases.record_create 0;
	
	record_Assertion = Assertion.Map.empty;
	count_Assertion = 0;
	total_cases_Assertion = Assertion_Constructor.Cases.record_create 0;
}

(**====================== iterators ========================**)

let iter_record_Class f ont =
	OrderedClass.Map.iter f ont.record_Class
let iter_record_Datatype f ont =
	OrderedDatatype.Map.iter f ont.record_Datatype
let iter_record_ConstrainingFacet f ont =
	OrderedConstrainingFacet.Map.iter f ont.record_ConstrainingFacet
let iter_record_ObjectProperty f ont =
	OrderedObjectProperty.Map.iter f ont.record_ObjectProperty
let iter_record_DataProperty f ont =
	OrderedDataProperty.Map.iter f ont.record_DataProperty
let iter_record_Individual f ont =
	OrderedIndividual.Map.iter f ont.record_Individual

let iter_record_ObjectPropertyExpression f ont =
	ObjectPropertyExpression.Map.iter f ont.record_ObjectPropertyExpression
let iter_record_ClassExpression f ont =
	ClassExpression.Map.iter f ont.record_ClassExpression

let iter_record_Declaration f ont =
    Declaration.Map.iter f ont.record_Declaration
let iter_record_ObjectPropertyAxiom f ont =
	ObjectPropertyAxiom.Map.iter f ont.record_ObjectPropertyAxiom
let iter_record_ClassAxiom f ont =
	ClassAxiom.Map.iter f ont.record_ClassAxiom
let iter_record_Assertion f ont =
	Assertion.Map.iter f ont.record_Assertion

(**============== statistical information ================**)

let count_Class ont = ont.count_Class
let count_Datatype ont = ont.count_Datatype
let count_ConstrainingFacet ont = ont.count_ConstrainingFacet
let count_ObjectProperty ont = ont.count_ObjectProperty
let count_DataProperty ont = ont.count_DataProperty
let count_Individual ont = ont.count_Individual

let count_ObjectPropertyExpression ont = ont.count_ObjectPropertyExpression
let count_ClassExpression ont = ont.count_ClassExpression

let count_ObjectPropertyAxiom ont = ont.count_ObjectPropertyAxiom
let count_ClassAxiom ont = ont.count_ClassAxiom
let count_Assertion ont = ont.count_Assertion

let has_positive_Nothing ont =
	let module C = Class_Constructor.Cases in
	PC.get_pos (C.record_get ont.total_cases_Class C.Nothing) > 0
let has_negative_Thing ont =
	let module C = Class_Constructor.Cases in
	PC.get_neg (C.record_get ont.total_cases_Class C.Thing) > 0
let has_positive_ObjectComplementOf ont =
	let module C = ClassExpression_Constructor.Cases in
	PC.get_pos (C.record_get ont.total_cases_ClassExpression C.ObjectComplementOf) > 0

(**==== insertion, computation of polarities and stats =====**)

let change_Class_pc ont c pc =
	let module M = OrderedClass.Map in
	let module C = Class_Constructor in
	let count =
		try M.find c ont.record_Class
		with Not_found -> PC.zero
	in
	let new_count = PC.sum count pc in
	if PC.is_zero new_count then begin
		ont.record_Class <- M.remove c ont.record_Class;
		if not (PC.is_zero count) then
			ont.count_Class <- pred ont.count_Class;
	end else begin
		ont.record_Class <- M.add c new_count ont.record_Class;
		if (PC.is_zero count) then
			ont.count_Class <- succ ont.count_Class;
	end;
	(* updating cases counter *)
	let case = C.case_of c in
	C.Cases.record_set ont.total_cases_Class case (
			PC.sum pc (C.Cases.record_get ont.total_cases_Class case)
		)
;;

let change_ObjectProperty_pc ont op pc =
	let module M = OrderedObjectProperty.Map in
	let module C = ObjectProperty_Constructor in
	let count =
		try M.find op ont.record_ObjectProperty
		with Not_found ->	PC.zero
	in
	let new_count = PC.sum count pc in
	if PC.is_zero new_count then begin
		ont.record_ObjectProperty <- M.remove op ont.record_ObjectProperty;
		if not (PC.is_zero count) then
			ont.count_ObjectProperty <- pred ont.count_ObjectProperty;
	end else begin
		ont.record_ObjectProperty <- M.add op new_count ont.record_ObjectProperty;
		if (PC.is_zero count) then
			ont.count_ObjectProperty <- succ ont.count_ObjectProperty;
	end;
	(* updating cases counter *)
	let case = C.case_of op.data in
	C.Cases.record_set ont.total_cases_ObjectProperty case (
			PC.sum pc (C.Cases.record_get ont.total_cases_ObjectProperty case)
		)
;;

let change_Individual_pc ont i pc =
	let module M = OrderedIndividual.Map in
	let module C = Individual_Constructor in
	let count =
		try M.find i ont.record_Individual
		with Not_found ->
				PC.zero
	in
	let new_count =	PC.sum count pc in
	if PC.is_zero new_count then begin
		ont.record_Individual <- M.remove i ont.record_Individual;
		if not (PC.is_zero count) then
			ont.count_Individual <- succ ont.count_Individual;
	end else begin
		ont.record_Individual <- M.add i new_count ont.record_Individual;
		if (PC.is_zero count) then
			ont.count_Individual <- succ ont.count_Individual;
	end;
	(* updating cases counter *)
	let case = C.case_of i in
	C.Cases.record_set ont.total_cases_Individual case (
			PC.sum pc (C.Cases.record_get ont.total_cases_Individual case)
		)
;;

let rec change_ObjectPropertyExpression_pc ont ope pc =
	let module M = ObjectPropertyExpression.Map in
	let module C = ObjectPropertyExpression_Constructor in
	ont.record_ObjectPropertyExpression <- M.process ope (function
			| Some pc_old ->
					let pc_new = PC.sum pc_old pc in
					if PC.is_zero pc_new then None
					else Some pc_new
			| None -> Some pc
		) ont.record_ObjectPropertyExpression;
	let case = C.case_of ope.data in
	C.Cases.record_set ont.total_cases_ObjectPropertyExpression case (
			PC.sum pc (C.Cases.record_get ont.total_cases_ObjectPropertyExpression case)
		);
	propagate_ObjectPropertyExpression_pc ont ope pc
and propagate_ObjectPropertyExpression_pc ont ope pc =
	let module M = ObjectPropertyExpression.Map in
	let module C = ObjectPropertyExpression_Constructor in
	match ope.data with
	| C.ObjectProperty op -> change_ObjectProperty_pc ont op pc
	| C.ObjectInverseOf op -> change_ObjectProperty_pc ont op pc;
;;

let rec change_ClassExpression_pc ont ce pc =
	let module M = ClassExpression.Map in
	let module C = ClassExpression_Constructor in
	ont.record_ClassExpression <- M.process ce (function
			| Some pc_old ->
					let pc_new = PC.sum pc_old pc in
					if PC.is_zero pc_new then None
					else Some pc_new
			| None -> Some pc
		) ont.record_ClassExpression;
	let case = C.case_of ce.data in
	C.Cases.record_set ont.total_cases_ClassExpression case (
			PC.sum pc (C.Cases.record_get ont.total_cases_ClassExpression case)
		);
	propagate_ClassExpression_pc ont ce pc
and propagate_ClassExpression_pc ont ce pc =
	let module C = ClassExpression_Constructor in
	match ce.data with
	| C.Class c -> change_Class_pc ont c pc
	| C.ObjectIntersectionOf (ce, cce) ->
			change_ClassExpression_pc ont ce pc;
			change_ClassExpression_pc ont cce pc;
	| C.ObjectIntersectionOfrec (ce, cce) ->
			change_ClassExpression_pc ont ce pc;
			change_ClassExpression_pc ont cce pc;
	| C.ObjectUnionOf (ce, cce) ->
			change_ClassExpression_pc ont ce pc;
			change_ClassExpression_pc ont cce pc;
	| C.ObjectUnionOfrec (ce, cce) ->
			change_ClassExpression_pc ont ce pc;
			change_ClassExpression_pc ont cce pc;
	| C.ObjectComplementOf de ->
			change_ClassExpression_pc ont de (PC.invert pc);
	| C.ObjectOneOf i_lst ->
			List.iter (fun i -> change_Individual_pc ont i pc) i_lst;
	| C.ObjectSomeValuesFrom (ope, de) ->
			change_ObjectPropertyExpression_pc ont ope pc;
			change_ClassExpression_pc ont de pc;
	| C.ObjectAllValuesFrom (ope, de) ->
			change_ObjectPropertyExpression_pc ont ope (PC.invert pc);
			change_ClassExpression_pc ont de pc;
	| C.ObjectHasValue (ope, i) ->
			change_ObjectPropertyExpression_pc ont ope pc;
			change_Individual_pc ont i pc;
	| C.ObjectHasSelf ope ->
			change_ObjectPropertyExpression_pc ont ope pc;
	| C.ObjectMinCardinality (n, ope, ceo) ->
			change_ObjectPropertyExpression_pc ont ope pc;
			begin match ceo with
				| None -> ()
				| Some ce -> change_ClassExpression_pc ont ce pc;
			end;
	| C.ObjectMaxCardinality (n, ope, ceo) ->
			change_ObjectPropertyExpression_pc ont ope (PC.invert pc);
			begin match ceo with
				| None -> ()
				| Some ce -> change_ClassExpression_pc ont ce (PC.invert pc);
			end;
	| C.ObjectExactCardinality (n, ope, ceo) ->
			change_ObjectPropertyExpression_pc ont ope (PC.symm pc);
			begin match ceo with
				| None -> ()
				| Some ce -> change_ClassExpression_pc ont ce (PC.symm pc);
			end;
	| _ -> (* TODO: other constructors *) ()
;;

let rec change_Declaration_cnt ont ax cnt =
    let module M = Declaration.Map in
    let module C = Declaration_Constructor in
    ont.record_Declaration <- M.process ax (function
            | Some cnt_old ->
                    let cnt_new = cnt_old + cnt in
                    if cnt_new = 0 then None
                    else Some cnt
            | None -> Some cnt
        ) ont.record_Declaration;
    let case = C.case_of ax.data in
    C.Cases.record_set ont.total_cases_Declaration case (
            cnt + (C.Cases.record_get ont.total_cases_Declaration case)
        );
    propagate_Declaration_cnt ont ax cnt
and propagate_Declaration_cnt ont ax cnt =
	  (* TODO: distinguish declarations from logical elements in the counters *)
    let module C = Declaration_Constructor in
    match ax.data with
    | C.Class c -> change_Class_pc ont c (PC.to_elt ~mult: cnt Polarity.Both)
    | C.Datatype _ -> ()
    | C.ObjectProperty op -> change_ObjectProperty_pc ont op (PC.to_elt ~mult: cnt Polarity.Both)
    | C.DataProperty _ -> ()
    | C.AnnotationProperty _ -> ()
    | C.NamedIndividual ind -> change_Individual_pc ont ind (PC.to_elt ~mult: cnt Polarity.Both)
;; 
let remove_Declaration ont ax =
    change_Declaration_cnt ont ax (- 1)
let add_Declaration ont ax =
    change_Declaration_cnt ont ax 1

let rec change_ObjectPropertyAxiom_cnt ont ax cnt =
	let module M = ObjectPropertyAxiom.Map in
	let module C = ObjectPropertyAxiom_Constructor in
	ont.record_ObjectPropertyAxiom <- M.process ax (function
			| Some cnt_old ->
					let cnt_new = cnt_old + cnt in
					if cnt_new = 0 then None
					else Some cnt
			| None -> Some cnt
		) ont.record_ObjectPropertyAxiom;
	let case = C.case_of ax.data in
	C.Cases.record_set ont.total_cases_ObjectPropertyAxiom case (
			cnt + (C.Cases.record_get ont.total_cases_ObjectPropertyAxiom case)
		);
	propagate_ObjectPropertyAxiom_cnt ont ax cnt
and propagate_ObjectPropertyAxiom_cnt ont ax cnt =
	let module C = ObjectPropertyAxiom_Constructor in
	match ax.data with
	| C.SubObjectPropertyOf (ope_ch, ope) ->
			List.iter (fun ope -> change_ObjectPropertyExpression_pc ont ope (PC.to_elt ~mult: cnt Polarity.Negative)) ope_ch;
			change_ObjectPropertyExpression_pc ont ope (PC.to_elt ~mult: cnt Polarity.Positive);
	| C.EquivalentObjectProperties (ope_lst) ->
			List.iter (fun ope -> change_ObjectPropertyExpression_pc ont ope (PC.to_elt ~mult: cnt Polarity.Both)) ope_lst;
	| C.DisjointObjectProperties _ -> ()
	| C.InverseObjectProperties (ope1, ope2) ->
			change_ObjectPropertyExpression_pc ont ope1 (PC.to_elt ~mult: cnt Polarity.Both);
			change_ObjectPropertyExpression_pc ont ope2 (PC.to_elt ~mult: cnt Polarity.Both);
	| C.ObjectPropertyDomain _ -> ()
	| C.ObjectPropertyRange _ -> ()
	| C.FunctionalObjectProperty ope ->
			change_ObjectPropertyExpression_pc ont ope (PC.to_elt ~mult: cnt Polarity.Negative);
	| C.InverseFunctionalObjectProperty ope ->
			change_ObjectPropertyExpression_pc ont ope (PC.to_elt ~mult: cnt Polarity.Negative);
	| C.ReflexiveObjectProperty _ -> ()
	| C.IrreflexiveObjectProperty _ -> ()
	| C.SymmetricObjectProperty _ -> ()
	| C.AsymmetricObjectProperty _ -> ()
	| C.TransitiveObjectProperty ope ->
			change_ObjectPropertyExpression_pc ont ope (PC.to_elt ~mult: cnt Polarity.Both);
;;
let remove_ObjectPropertyAxiom ont ax =
	change_ObjectPropertyAxiom_cnt ont ax (- 1)
let add_ObjectPropertyAxiom ont ax =
	change_ObjectPropertyAxiom_cnt ont ax 1

let rec change_ClassAxiom_cnt ont ax cnt =
	let module M = ClassAxiom.Map in
	let module C = ClassAxiom_Constructor in
	ont.record_ClassAxiom <- M.process ax (function
			| Some cnt_old ->
					let cnt_new = cnt_old + cnt in
					if cnt_new = 0 then None
					else Some cnt
			| None -> Some cnt
		) ont.record_ClassAxiom;
	let case = C.case_of ax.data in
	C.Cases.record_set ont.total_cases_ClassAxiom case (
			cnt + (C.Cases.record_get ont.total_cases_ClassAxiom case)
		);
	propagate_ClassAxiom_cnt ont ax cnt
and propagate_ClassAxiom_cnt ont ax cnt =
	let module C = ClassAxiom_Constructor in
	match ax.data with
	| C.SubClassOf (ce1, ce2) ->
			change_ClassExpression_pc ont ce1 (PC.to_elt ~mult: cnt Polarity.Negative);
			change_ClassExpression_pc ont ce2 (PC.to_elt ~mult: cnt Polarity.Positive);
	| C.EquivalentClasses (c_lst) ->
			List.iter (fun ce -> change_ClassExpression_pc ont ce (PC.to_elt ~mult: cnt Polarity.Both)) c_lst;
	| C.DisjointClasses _ -> ()
	| C.DisjointUnion _ -> ()
;;
let remove_ClassAxiom ont ax =
	change_ClassAxiom_cnt ont ax (- 1)
let add_ClassAxiom ont ax =
	change_ClassAxiom_cnt ont ax 1

let rec change_Assertion_cnt ont ax cnt =
	let module M = Assertion.Map in
	let module C = Assertion_Constructor in
	ont.record_Assertion <- M.process ax (function
			| Some cnt_old ->
					let cnt_new = cnt_old + cnt in
					if cnt_new = 0 then None
					else Some cnt
			| None -> Some cnt
		) ont.record_Assertion;
	let case = C.case_of ax.data in
	C.Cases.record_set ont.total_cases_Assertion case (
			cnt + (C.Cases.record_get ont.total_cases_Assertion case)
		);
	propagate_Assertion_cnt ont ax cnt
and propagate_Assertion_cnt ont ax cnt =
	let module C = Assertion_Constructor in
	match ax.data with
	| C.SameIndividual _ -> ()
	| C.DifferentIndividuals _ -> ()
	| C.ClassAssertion (ce, i) ->
			change_ClassExpression_pc ont ce (PC.to_elt ~mult: cnt Polarity.Positive);
			change_Individual_pc ont i (PC.to_elt Polarity.Negative);
	| C.ObjectPropertyAssertion (ope, i1, i2) ->
			change_ObjectPropertyExpression_pc ont ope (PC.to_elt ~mult: cnt Polarity.Positive);
			change_Individual_pc ont i1 (PC.to_elt ~mult: cnt Polarity.Negative);
			change_Individual_pc ont i2 (PC.to_elt ~mult: cnt Polarity.Negative);
	| C.NegativeObjectPropertyAssertion _ -> ()
	| C.DataPropertyAssertion _ -> ()
	| C.NegativeDataPropertyAssertion _ -> ()
;;
let remove_Assertion ont ax =
	change_Assertion_cnt ont ax (- 1)
let add_Assertion ont ax =
	change_Assertion_cnt ont ax 1

(** =============== printing various information ================== **)

type info_type =
	| All            (* about all OWL objects *)
	| Used           (* about those used in the ontology *)
	| Unsupported    (* about unsupported *)

let print_info ?(info_type = Used) ont ch_out =
	(* first column width *)
	let width = 36 in
	(* printing title *)
	let print_title = ref (fun () -> ()) in
	let title_printed = ref false in
	(* printing header *)
	let print_header = ref (fun () -> ()) in
	let header_printed = ref false in
	(* print pending titles or headers *)
	let check_print () =
		if (not !title_printed) then
			(!print_title (); title_printed := true);
		if (not !header_printed) then
			(!print_header (); header_printed := true);
	in
	(* printing simple counters *)
	let print item count =
		check_print ();
		fprintf ch_out "%-*s%6n\n" width item count
	in
	let print = match info_type with
		| All -> print
		| Used | Unsupported ->
				fun item count ->
						if count > 0 then print item count
	in
	(* for supported constructors *)
	let print_sp = match info_type with
		| All | Used -> print
		| Unsupported -> fun item count -> ()
	in
	(* printing polarity counters *)
	let string_of_counter = match info_type with
		| All -> string_of_int
		| Used | Unsupported ->
				fun n -> if n = 0 then "" else string_of_int n
	in
	let print_pc item pos neg =
		check_print ();
		fprintf ch_out "%-*s%6s%8s\n"
			width item
			(string_of_counter pos)
			(string_of_counter neg)
	in
	let print_pc = match info_type with
		| All -> print_pc
		| Used | Unsupported ->
				fun item pos neg ->
						if pos + neg > 0 then print_pc item pos neg
	in
	let print_pc ?(pol = Polarity.Both) item pc =
		print_pc item
			(if Polarity.is_positive pol then PC.get_pos pc else 0)
			(if Polarity.is_negative pol then PC.get_neg pc else 0)
	in
	(* for supported constructors *)
	let print_pc_sp ?(pol = Polarity.Both) = match info_type with
		| All | Used -> print_pc ~pol: Polarity.Both
		| Unsupported ->
				if pol = Polarity.Both then fun _ _ -> ()
				else print_pc ~pol: (Polarity.invert pol)
	in
	title_printed := false;
	print_title := (fun () ->
				begin match info_type with
					| All | Used ->
							fprintf ch_out "%s\n" (String.make (width + 14) '~');
							fprintf ch_out "%-*s\n" width "Ontology information:";
							fprintf ch_out "%s\n" (String.make (width + 14) '~');
					| Unsupported ->
							fprintf ch_out "%s\n" (String.make (width + 14) '~');
							fprintf ch_out "%-*s\n" width "Warning: Reasoning can be incomplete!";
							fprintf ch_out "%s\n" (String.make (width + 14) '~');
				end);
	header_printed := false;
	print_header := (fun () ->
				begin match info_type with
					| All | Used ->
							fprintf ch_out "%-*s\n" width "Signature (element counts):";
							fprintf ch_out "%s\n" (String.make (width + 14) '-');
					| Unsupported ->
							fprintf ch_out "%-*s\n" width "Unsupported signature elements:";
							fprintf ch_out "%s\n" (String.make (width + 14) '-');
				end);
	print_sp "classes:" ont.count_Class;
	print "datatypes:" ont.count_Datatype;
	print "constraining facets:" ont.count_ConstrainingFacet;
	print_sp "object properties:" ont.count_ObjectProperty;
	print "data properties:" ont.count_DataProperty;
	print "individuals:" ont.count_Individual;
	
	header_printed := false;
	print_header := (fun () ->
				begin match info_type with
					| All | Used ->
							fprintf ch_out "%s\n" (String.make (width + 14) '-');
							fprintf ch_out "%-*s%6s%8s\n" width
								"Structure (expression counts):" "(pos)" "(neg)";
							fprintf ch_out "%s\n" (String.make (width + 14) '-');
					| Unsupported ->
							fprintf ch_out "%s\n" (String.make (width + 14) '-');
							fprintf ch_out "%-*s%6s%8s\n" width
								"Unsupported constructors:" "(pos)" "(neg)";
							fprintf ch_out "%s\n" (String.make (width + 14) '-');
				end);
	let module C = Class_Constructor.Cases in
	C.iter (function
			| C.IRI -> ()
			| _ as case ->
					(match C.record_get supported_Class_Constructor case with
						| Some pol -> fun id pc -> print_pc_sp ~pol: pol id pc
						| None -> fun id pc -> print_pc id pc
					) (C.str_of case ^ ":") (C.record_get ont.total_cases_Class case);
		);
	let module C = Datatype_Constructor.Cases in
	C.iter (function
			| C.IRI -> ()
			| _ as case ->
					(match C.record_get supported_Datatype_Constructor case with
						| Some pol -> fun id pc -> print_pc_sp ~pol: pol id pc
						| None -> fun id pc -> print_pc id pc
					) (C.str_of case ^ ":") (C.record_get ont.total_cases_Datatype case);
		);
	let module C = ConstrainingFacet_Constructor.Cases in
	C.iter (function
			| C.IRI -> ()
			| _ as case ->
					(match C.record_get supported_ConstrainingFacet_Constructor case with
						| Some pol -> fun id pc -> print_pc_sp ~pol: pol id pc
						| None -> fun id pc -> print_pc id pc
					) (C.str_of case ^ ":") (C.record_get ont.total_cases_ConstrainingFacet case);
		);
	let module C = ObjectProperty_Constructor.Cases in
	C.iter (function
			| C.IRI -> ()
			| _ as case ->
					(match C.record_get supported_ObjectProperty_Constructor case with
						| Some pol -> fun id pc -> print_pc_sp ~pol: pol id pc
						| None -> fun id pc -> print_pc id pc
					) (C.str_of case ^ ":") (C.record_get ont.total_cases_ObjectProperty case);
		);
	let module C = DataProperty_Constructor.Cases in
	C.iter (function
			| C.IRI -> ()
			| _ as case ->
					(match C.record_get supported_DataProperty_Constructor case with
						| Some pol -> fun id pc -> print_pc_sp ~pol: pol id pc
						| None -> fun id pc -> print_pc id pc
					) (C.str_of case ^ ":") (C.record_get ont.total_cases_DataProperty case);
		);
	let module C = Individual_Constructor.Cases in
	C.iter (function
			| C.NamedIndividual -> ()
			| C.AnonymousIndividual -> ()
			(*|			| _ as case ->                                                             *)
			(*|					(match C.record_get supported_Individual_Constructor case with         *)
			(*|						| Some pol -> fun id pc -> print_pc_sp ~pol: pol id pc               *)
			(*|						| None -> fun id pc -> print_pc id pc                                *)
			(*|					) (C.str_of case ^ ":") (C.record_get ont.total_cases_Individual case);*)
		);
	let module C = ObjectPropertyExpression_Constructor.Cases in
	C.iter (function
			| C.ObjectProperty -> ()
			| _ as case ->
					(match C.record_get supported_ObjectPropertyExpression_Constructor case with
						| Some pol -> fun id pc -> print_pc_sp ~pol: pol id pc
						| None -> fun id pc -> print_pc id pc
					) (C.str_of case ^ ":") (C.record_get ont.total_cases_ObjectPropertyExpression case);
		);
	let module C = ClassExpression_Constructor.Cases in
	C.iter (function
			| C.Class -> ()
			| C.ObjectIntersectionOfrec -> ()
			| _ as case ->
					(match C.record_get supported_ClassExpression_Constructor case with
						| Some pol -> fun id pc -> print_pc_sp ~pol: pol id pc
						| None -> fun id pc -> print_pc id pc
					) (C.str_of case ^ ":") (C.record_get ont.total_cases_ClassExpression case);
		);
	header_printed := false;
	print_header := (fun () ->
				begin match info_type with
					| All | Used ->
							fprintf ch_out "%s\n" (String.make (width + 14) '-');
							fprintf ch_out "%-*s\n" width "Theory (axiom counts):";
							fprintf ch_out "%s\n" (String.make (width + 14) '-');
					| Unsupported ->
							fprintf ch_out "%s\n" (String.make (width + 14) '-');
							fprintf ch_out "%-*s\n" width "Unsupported axioms:";
							fprintf ch_out "%s\n" (String.make (width + 14) '-');
				end);
(*|	let module C = Declaration_Constructor.Cases in                                   *)
(*|    C.iter (fun case ->                                                             *)
(*|            (match C.record_get supported_Declaration_Constructor case with         *)
(*|              | true -> print_sp                                                    *)
(*|              | false -> print                                                      *)
(*|            ) (C.str_of case ^ ":") (C.record_get ont.total_cases_Declaration case);*)
(*|        );			                                                                    *)
	let module C = ObjectPropertyAxiom_Constructor.Cases in
	C.iter (fun case ->
					(match C.record_get supported_ObjectPropertyAxiom_Constructor case with
						| true -> print_sp
						| false -> print
					) (C.str_of case ^ ":") (C.record_get ont.total_cases_ObjectPropertyAxiom case);
		);
	let module C = ClassAxiom_Constructor.Cases in
	C.iter (fun case ->
					(match C.record_get supported_ClassAxiom_Constructor case with
						| true -> print_sp
						| false -> print
					) (C.str_of case ^ ":") (C.record_get ont.total_cases_ClassAxiom case);
		);
	let module C = Assertion_Constructor.Cases in
	let supported_Assertion_Constructor = C.record_create false in
	C.iter (fun case ->
					(match C.record_get supported_Assertion_Constructor case with
						| true -> print_sp
						| false -> print
					) (C.str_of case ^ ":") (C.record_get ont.total_cases_Assertion case);
		);
	
	if !title_printed then
		fprintf ch_out "%s\n" (String.make (width + 14) '~');
;;