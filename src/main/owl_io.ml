(**=========== input-output for ontologies in functional style syntax ============**)

open Consed
open Owl
module O = Ontology
module F = Format
module PT = Progress_tracker

(**=========== loading from the input channel ===========**)

let load_Ontology_from_channel ?(message = "Loading ontology...") pt_lst in_chan =
	let lexbuf = Lexing.from_channel in_chan in
	PT.start pt_lst message (in_channel_length in_chan);
	let ont =
		try Owl_fs_parser.entry (Owl_fs_lexer.entry pt_lst) lexbuf;
		with Parsing.Parse_error ->
				let err_lexeme = Lexing.lexeme lexbuf in
				let err_pos = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol in
				Printf.fprintf stderr "\nLine %n, characters %n-%n:\nSyntax error: unexpected \"%s\"\n"
					lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
					(err_pos - String.length err_lexeme + 1)
					err_pos
					err_lexeme;
				raise Parsing.Parse_error
	in
	PT.finish pt_lst;
	ont

(* auxiliary printing functions *)
module P = struct
	(* [fprint_list f prt sep lst] prints elements of the list [lst] using   *)
	(* the printer [prnt] according to format [f]; the elements are          *)
	(* separated with a formatting string [sep]                              *)
	let rec fprint_list f prt sep = function
		| [] -> ()
		| e :: tl ->
				prt f e;
				List.iter (fun e ->
								F.fprintf f sep;
								prt f e;
					) tl
	(* [fprint_cset f prt sep set] prints elements of the set [set] using    *)
	(* the printer [prnt] according to format [f]; the elements are          *)
	(* separated with a formatting string [sep]                              *)
	let fprint_cset f prt sep set =
		let flag_sep = ref false in
		List.iter (fun e ->
						if !flag_sep then (
							F.fprintf f sep;
						);
						flag_sep := true;
						prt f e;
			) set
	(* [fprint_option f prt prx o] optionally prints element o with prefix   *)
	(* [prx]                                                                 *)
	let fprint_option f prt prx o =
		match o with
		| None -> ()
		| Some e -> F.fprintf f prx; prt f e
end;;

(**======================== IRIs ========================**)

let str_of_IRI iri = IRI.str_of iri
let fprint_IRI f iri = F.pp_print_string f (str_of_IRI iri)

(**====================== NodeIDs =======================**)

let str_of_NodeID id = NodeID.str_of id
let fprint_NodeID f id = F.pp_print_string f (str_of_NodeID id)

(**====================== Datatypes =====================**)

let str_of_Datatype dt = Datatype.str_of dt
let fprint_Datatype f dt = F.pp_print_string f (str_of_Datatype dt)

(**================== Constraining Facets ===============**)

let str_of_ConstrainingFacet dt = ConstrainingFacet.str_of dt
let fprint_ConstrainingFacet f dt = F.pp_print_string f (str_of_ConstrainingFacet dt)

(**================= Object Properties ==================**)

let str_of_ObjectProperty dt = ObjectProperty.str_of dt
let fprint_ObjectProperty f dt = F.pp_print_string f (str_of_ObjectProperty dt)

(**=================== Data Properties ==================**)

let str_of_DataProperty dt = DataProperty.str_of dt
let fprint_DataProperty f dt = F.pp_print_string f (str_of_DataProperty dt)

(**================ Annotation Properties ===============**)

let str_of_AnnotationProperty dt = AnnotationProperty.str_of dt
let fprint_AnnotationProperty f dt = F.pp_print_string f (str_of_AnnotationProperty dt)

(**====================== Classes =======================**)

let str_of_Class dt = Class.str_of dt
let fprint_Class f dt = F.pp_print_string f (str_of_Class dt)

(**==================== Individuals =====================**)

let str_of_Individual dt = Individual.str_of dt
let fprint_Individual f dt = F.pp_print_string f (str_of_Individual dt)

(**======================= Literals =====================**)

let fprint_Literal f lt =
	let module C = Literal_Constructor in
	begin match lt.data with
		| C.TypedLiteral (lf, dt) ->
				F.fprintf f "@[<hv 2>\"%s\"^^" lf;
				fprint_Datatype f dt;
				F.fprintf f "@]";
		| C.StringLiteralNoLanguage st ->
				F.fprintf f "@[<hv 2>\"%s\"@]" st;
		| C.StringLiteralWithLanguage (st, lg) ->
				F.fprintf f "@[<hv 2>\"%s\"@%s@]" st lg;
	end
;;

let str_of_Literal lt =
	fprint_Literal F.str_formatter lt;
	F.flush_str_formatter ()
;;

(**============ Object Property Expressions =============**)

let fprint_ObjectPropertyExpression f ope =
	let module C = ObjectPropertyExpression_Constructor in
	begin match ope.data with
		| C.ObjectProperty op ->
				fprint_ObjectProperty f op;
		| C.ObjectInverseOf op ->
				F.fprintf f "@[<hv 2>ObjectInverseOf(@,";
				fprint_ObjectProperty f op;
				F.fprintf f "@;<0 -2>)@]";
	end;
;;

let fprint_subObjectPropertyExpression f sope =
	begin match sope with
		| [ope] -> fprint_ObjectPropertyExpression f ope
		| _ ->
				F.fprintf f "@[<hv 2>ObjectPropertyChain(@,";
				P.fprint_list f fprint_ObjectPropertyExpression "@ " sope;
				F.fprintf f "@;<0 -2>)@]";
	end
;;

let str_of_ObjectPropertyExpression ope =
	fprint_ObjectPropertyExpression F.str_formatter ope;
	F.flush_str_formatter ()
;;

(**============= Data Property Expressions ==============**)

let fprint_DataPropertyExpression f dpe =
	let module C = DataPropertyExpression_Constructor in
	begin match dpe.data with
		| C.DataProperty dp -> fprint_DataProperty f dp
	end
;;

let str_of_DataPropertyExpression dpe =
	fprint_DataPropertyExpression F.str_formatter dpe;
	F.flush_str_formatter ()
;;

(**==================== Data Ranges =====================**)

let rec fprint_DataRange f dr =
	let module C = DataRange_Constructor in
	begin match dr.data with
		| C.Datatype dt -> fprint_Datatype f dt
		| C.DataIntersectionOf dr_set ->
				F.fprintf f "@[<hv 2>DataIntersectionOf(@,";
				P.fprint_cset f fprint_DataRange "@ " dr_set;
				F.fprintf f "@;<0 -2>)@]";
		| C.DataUnionOf dr_set ->
				F.fprintf f "@[<hv 2>DataUnionOf(@,";
				P.fprint_cset f fprint_DataRange "@ " dr_set;
				F.fprintf f "@;<0 -2>)@]";
		| C.DataComplementOf dr ->
				F.fprintf f "@[<hv 2>DataComplementOf(@,";
				fprint_DataRange f dr;
				F.fprintf f "@;<0 -2>)@]";
		| C.DataOneOf lt_set ->
				F.fprintf f "@[<hv 2>DataOneOf(@,";
				P.fprint_cset f fprint_Literal "@ " lt_set;
				F.fprintf f "@;<0 -2>)@]";
		| C.DatatypeRestriction (dt, cf_lt_lst) ->
				F.fprintf f "@[<hv 2>DataOneOf(@,";
				P.fprint_list f
					(fun f (cf, lt) ->
								fprint_ConstrainingFacet f cf;
								F.fprintf f "@ ";
								fprint_Literal f lt;
					) "@ " cf_lt_lst;
				F.fprintf f "@;<0 -2>)@]";
	end
;;

let str_of_DataRange dr =
	fprint_DataRange F.str_formatter dr;
	F.flush_str_formatter ()
;;

(**==================== Class Expressions ==================**)

let rec fprint_ClassExpression f ce =
	let module C = ClassExpression_Constructor in
	begin match ce.data with
		| C.Class c ->
				fprint_Class f c;
		| C.ObjectIntersectionOf (cce, ce) ->
				F.fprintf f "@[<hv 2>ObjectIntersectionOf(@,";
				fprint_ClassExpression f cce;
				F.fprintf f "@ ";
				fprint_ClassExpression f ce;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectIntersectionOfrec (cce, ce) ->
				fprint_ClassExpression f cce;
				F.fprintf f "@ ";
				fprint_ClassExpression f ce;
		| C.ObjectUnionOf (cce, ce) ->
				F.fprintf f "@[<hv 2>ObjectUnionOf(@,";
				fprint_ClassExpression f cce;
				F.fprintf f "@ ";
				fprint_ClassExpression f ce;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectUnionOfrec (cce, ce) ->
				fprint_ClassExpression f cce;
				F.fprintf f "@ ";
				fprint_ClassExpression f ce;
		| C.ObjectComplementOf ce ->
				F.fprintf f "@[<hv 2>ObjectComplementOf(@,";
				fprint_ClassExpression f ce;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectOneOf i_set ->
				F.fprintf f "@[<hv 2>ObjectOneOf(@,";
				P.fprint_cset f fprint_Individual "@ " i_set;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectSomeValuesFrom (ope, ce) ->
				F.fprintf f "@[<hv 2>ObjectSomeValuesFrom(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@ ";
				fprint_ClassExpression f ce;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectAllValuesFrom (ope, ce) ->
				F.fprintf f "@[<hv 2>ObjectAllValuesFrom(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@ ";
				fprint_ClassExpression f ce;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectHasValue (ope, i) ->
				F.fprintf f "@[<hv 2>ObjectHasValue(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@ ";
				fprint_Individual f i;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectHasSelf ope ->
				F.fprintf f "@[<hv 2>ObjectHasSelf(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectMinCardinality (n, ope, ceo) ->
				F.fprintf f "@[<hv 2>ObjectMinCardinality(@,";
				F.fprintf f "@[%n@]" n;
				F.fprintf f "@ ";
				fprint_ObjectPropertyExpression f ope;
				P.fprint_option f fprint_ClassExpression "@ " ceo;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectMaxCardinality (n, ope, ceo) ->
				F.fprintf f "@[<hv 2>ObjectMaxCardinality(@,";
				F.fprintf f "@[%n@]" n;
				F.fprintf f "@ ";
				fprint_ObjectPropertyExpression f ope;
				P.fprint_option f fprint_ClassExpression "@ " ceo;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectExactCardinality (n, ope, ceo) ->
				F.fprintf f "@[<hv 2>ObjectExactCardinality(@,";
				F.fprintf f "@[%n@]" n;
				F.fprintf f "@ ";
				fprint_ObjectPropertyExpression f ope;
				P.fprint_option f fprint_ClassExpression "@ " ceo;
				F.fprintf f "@;<0 -2>)@]";
		| C.DataSomeValuesFrom (dpe_lst, dr) ->
				F.fprintf f "@[<hv 2>DataSomeValuesFrom(@,";
				P.fprint_list f fprint_DataPropertyExpression "@ " dpe_lst;
				F.fprintf f "@ ";
				fprint_DataRange f dr;
				F.fprintf f "@;<0 -2>)@]";
		| C.DataAllValuesFrom (dpe_lst, dr) ->
				F.fprintf f "@[<hv 2>DataAllValuesFrom(@,";
				P.fprint_list f fprint_DataPropertyExpression "@ " dpe_lst;
				F.fprintf f "@ ";
				fprint_DataRange f dr;
				F.fprintf f "@;<0 -2>)@]";
		| C.DataHasValue (dpe, lt) ->
				F.fprintf f "@[<hv 2>DataHasValue(@,";
				fprint_DataPropertyExpression f dpe;
				F.fprintf f "@ ";
				fprint_Literal f lt;
				F.fprintf f "@;<0 -2>)@]";
		| C.DataMinCardinality (n, dpe, dro) ->
				F.fprintf f "@[<hv 2>DataMinCardinality(@,";
				F.fprintf f "@[%n@]" n;
				F.fprintf f "@ ";
				fprint_DataPropertyExpression f dpe;
				P.fprint_option f fprint_DataRange "@ " dro;
				F.fprintf f "@;<0 -2>)@]";
		| C.DataMaxCardinality (n, dpe, dro) ->
				F.fprintf f "@[<hv 2>DataMaxCardinality(@,";
				F.fprintf f "@[%n@]" n;
				F.fprintf f "@ ";
				fprint_DataPropertyExpression f dpe;
				P.fprint_option f fprint_DataRange "@ " dro;
				F.fprintf f "@;<0 -2>)@]";
		| C.DataExactCardinality (n, dpe, dro) ->
				F.fprintf f "@[<hv 2>DataExactCardinality(@,";
				F.fprintf f "@[%n@]" n;
				F.fprintf f "@ ";
				fprint_DataPropertyExpression f dpe;
				P.fprint_option f fprint_DataRange "@ " dro;
				F.fprintf f "@;<0 -2>)@]";
	end
;;

let str_of_ClassExpression ce =
	fprint_ClassExpression F.str_formatter ce;
	F.flush_str_formatter ()
;;

(**================= Class Expression Axioms ===============**)

let fprint_ClassAxiom f ax =
	let module C = ClassAxiom_Constructor in
	begin match ax.data with
		| C.SubClassOf (ce1, ce2) ->
				F.fprintf f "@[<hv 2>SubClassOf(@,";
				fprint_ClassExpression f ce1;
				F.fprintf f "@ ";
				fprint_ClassExpression f ce2;
				F.fprintf f "@;<0 -2>)@]";
		| C.EquivalentClasses ce_set ->
				F.fprintf f "@[<hv 2>EquivalentClasses(@,";
				P.fprint_cset f fprint_ClassExpression "@ " ce_set;
				F.fprintf f "@;<0 -2>)@]";
		| C.DisjointClasses ce_set ->
				F.fprintf f "@[<hv 2>DisjointClasses(@,";
				P.fprint_cset f fprint_ClassExpression "@ " ce_set;
				F.fprintf f "@;<0 -2>)@]";
		| C.DisjointUnion (c, ce_set) ->
				F.fprintf f "@[<hv 2>DisjointUnion(@,";
				fprint_Class f c;
				F.fprintf f "@ ";
				P.fprint_cset f fprint_ClassExpression "@ " ce_set;
				F.fprintf f "@;<0 -2>)@]";
	end;
;;

let str_of_ClassAxiom ax =
	fprint_ClassAxiom F.str_formatter ax;
	F.flush_str_formatter ()
;;

(**================= Object Property Axioms ================**)

let fprint_ObjectPropertyAxiom f ax =
	let module C = ObjectPropertyAxiom_Constructor in
	begin match ax.data with
		| C.SubObjectPropertyOf (op_ch, op) ->
				F.fprintf f "@[<hv 2>SubObjectPropertyOf(@,";
				fprint_subObjectPropertyExpression f op_ch;
				F.fprintf f "@ ";
				fprint_ObjectPropertyExpression f op;
				F.fprintf f "@;<0 -2>)@]";
		| C.EquivalentObjectProperties ope_set ->
				F.fprintf f "@[<hv 2>EquivalentObjectProperties(@,";
				P.fprint_cset f fprint_ObjectPropertyExpression "@ " ope_set;
				F.fprintf f "@;<0 -2>)@]";
		| C.DisjointObjectProperties ope_set ->
				F.fprintf f "@[<hv 2>DisjointObjectProperties(@,";
				P.fprint_cset f fprint_ObjectPropertyExpression "@ " ope_set;
				F.fprintf f "@;<0 -2>)@]";
		| C.InverseObjectProperties (ope1, ope2) ->
				F.fprintf f "@[<hv 2>InverseObjectProperties(@,";
				fprint_ObjectPropertyExpression f ope1;
				F.fprintf f "@ ";
				fprint_ObjectPropertyExpression f ope2;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectPropertyDomain (ope, ce) ->
				F.fprintf f "@[<hv 2>ObjectPropertyDomain(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@ ";
				fprint_ClassExpression f ce;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectPropertyRange (ope, ce) ->
				F.fprintf f "@[<hv 2>ObjectPropertyRange(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@ ";
				fprint_ClassExpression f ce;
				F.fprintf f "@;<0 -2>)@]";
		| C.FunctionalObjectProperty ope ->
				F.fprintf f "@[<hv 2>FunctionalObjectProperty(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@;<0 -2>)@]";
		| C.InverseFunctionalObjectProperty ope ->
				F.fprintf f "@[<hv 2>InverseFunctionalObjectProperty(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@;<0 -2>)@]";
		| C.ReflexiveObjectProperty ope ->
				F.fprintf f "@[<hv 2>ReflexiveObjectProperty(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@;<0 -2>)@]";
		| C.IrreflexiveObjectProperty ope ->
				F.fprintf f "@[<hv 2>IrreflexiveObjectProperty(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@;<0 -2>)@]";
		| C.SymmetricObjectProperty ope ->
				F.fprintf f "@[<hv 2>SymmetricObjectProperty(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@;<0 -2>)@]";
		| C.AsymmetricObjectProperty ope ->
				F.fprintf f "@[<hv 2>AsymmetricObjectProperty(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@;<0 -2>)@]";
		| C.TransitiveObjectProperty ope ->
				F.fprintf f "@[<hv 2>TransitiveObjectProperty(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@;<0 -2>)@]";
	end
;;

let str_of_ObjectPropertyAxiom ax =
	fprint_ObjectPropertyAxiom F.str_formatter ax;
	F.flush_str_formatter ()
;;

(**================= Data Property Axioms ================**)

let fprint_DataPropertyAxiom f ax =
	let module C = DataPropertyAxiom_Constructor in
	begin match ax.data with
		| C.SubDataPropertyOf (sdpe, dpe) ->
				F.fprintf f "@[<hv 2>SubDataPropertyOf(@,";
				fprint_DataPropertyExpression f sdpe;
				F.fprintf f "@ ";
				fprint_DataPropertyExpression f dpe;
				F.fprintf f "@;<0 -2>)@]";
		| C.EquivalentDataProperties dpe_set ->
				F.fprintf f "@[<hv 2>EquivalentDataProperties(@,";
				P.fprint_cset f fprint_DataPropertyExpression "@ " dpe_set;
				F.fprintf f "@;<0 -2>)@]";
		| C.DisjointDataProperties dpe_set ->
				F.fprintf f "@[<hv 2>DisjointDataProperties(@,";
				P.fprint_cset f fprint_DataPropertyExpression "@ " dpe_set;
				F.fprintf f "@;<0 -2>)@]";
		| C.DataPropertyDomain (dpe, ce) ->
				F.fprintf f "@[<hv 2>DataPropertyDomain(@,";
				fprint_DataPropertyExpression f dpe;
				F.fprintf f "@ ";
				fprint_ClassExpression f ce;
				F.fprintf f "@;<0 -2>)@]";
		| C.DataPropertyRange (dpe, dr) ->
				F.fprintf f "@[<hv 2>DataPropertyDomain(@,";
				fprint_DataPropertyExpression f dpe;
				F.fprintf f "@ ";
				fprint_DataRange f dr;
				F.fprintf f "@;<0 -2>)@]";
		| C.FunctionalDataProperty dpe ->
				F.fprintf f "@[<hv 2>FunctionalDataProperty(@,";
				fprint_DataPropertyExpression f dpe;
				F.fprintf f "@;<0 -2>)@]";
	end
;;

let str_of_DataPropertyAxiom ax =
	fprint_DataPropertyAxiom F.str_formatter ax;
	F.flush_str_formatter ()
;;

(**====================== Assertions ======================**)

let fprint_Assertion f ax =
	let module C = Assertion_Constructor in
	begin match ax.data with
		| C.SameIndividual i_set ->
				F.fprintf f "@[<hv 2>SameIndividual(@,";
				P.fprint_cset f fprint_Individual "@ " i_set;
				F.fprintf f "@;<0 -2>)@]";
		| C.DifferentIndividuals i_set ->
				F.fprintf f "@[<hv 2>DifferentIndividuals(@,";
				P.fprint_cset f fprint_Individual "@ " i_set;
				F.fprintf f "@;<0 -2>)@]";
		| C.ClassAssertion (c, i) ->
				F.fprintf f "@[<hv 2>ClassAssertion(@,";
				fprint_ClassExpression f c;
				F.fprintf f "@ ";
				fprint_Individual f i;
				F.fprintf f "@;<0 -2>)@]";
		| C.ObjectPropertyAssertion (ope, i1, i2) ->
				F.fprintf f "@[<hv 2>ObjectPropertyAssertion(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@ ";
				fprint_Individual f i1;
				F.fprintf f "@ ";
				fprint_Individual f i2;
				F.fprintf f "@;<0 -2>)@]";
		| C.NegativeObjectPropertyAssertion (ope, i1, i2) ->
				F.fprintf f "@[<hv 2>NegativeObjectPropertyAssertion(@,";
				fprint_ObjectPropertyExpression f ope;
				F.fprintf f "@ ";
				fprint_Individual f i1;
				F.fprintf f "@ ";
				fprint_Individual f i2;
				F.fprintf f "@;<0 -2>)@]";
		| C.DataPropertyAssertion (dpe, i, l) ->
				F.fprintf f "@[<hv 2>DataPropertyAssertion(@,";
				fprint_DataPropertyExpression f dpe;
				F.fprintf f "@ ";
				fprint_Individual f i;
				F.fprintf f "@ ";
				fprint_Literal f l;
				F.fprintf f "@;<0 -2>)@]";
		| C.NegativeDataPropertyAssertion (dpe, i, l) ->
				F.fprintf f "@[<hv 2>NegativeDataPropertyAssertion(@,";
				fprint_DataPropertyExpression f dpe;
				F.fprintf f "@ ";
				fprint_Individual f i;
				F.fprintf f "@ ";
				fprint_Literal f l;
				F.fprintf f "@;<0 -2>)@]";
	end;
;;

let str_of_Assertion ax =
	fprint_Assertion F.str_formatter ax;
	F.flush_str_formatter ()
;;

(**======================== Ontology =======================**)

let fprint_ontology ?(message = "Printing ontology...") pt f ont =
	pt.PT.start message (
			O.total_ObjectPropertyAxiom ont +
			O.total_ClassAxiom ont +
			O.total_Assertion ont);
	F.fprintf f "@[<v 2>Ontology(";
	O.iter_record_ObjectPropertyAxiom
		(fun ax ->
					F.fprintf f "@;";
					fprint_ObjectPropertyAxiom f ax;
					pt.PT.step ();
		) ont;
	O.iter_record_ClassAxiom
		(fun ax ->
					F.fprintf f "@;";
					fprint_ClassAxiom f ax;
					pt.PT.step ();
		) ont;
	O.iter_record_Assertion
		(fun ax ->
					F.fprintf f "@;";
					fprint_Assertion f ax;
					pt.PT.step ();
		) ont;
	F.fprintf f "@;<0 -2>)@]%!";
	pt.PT.finish ();
;;

let print_ontology_ch ?(message = "Printing ontology...") pt ont out =
	let f = F.formatter_of_out_channel out in
	fprint_ontology ~message: message pt f ont;
;;

let save_ontology ?(message = "Saving ontology...") pt ont file_name =
	let file = open_out file_name in
	print_ontology_ch ~message: message pt ont file;
	close_out file;
;;
