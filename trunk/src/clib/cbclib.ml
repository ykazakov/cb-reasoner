(** C interface functions for cb *)

open Owl
module PM = Progress_monitor
module CE = ClassExpression_Constructor
module C = Class_Constructor

(** Exception handling *)

let cb_exception_to_string e = Printexc.to_string e
let cb_backtrace_status () = Printexc.backtrace_status ()
let cb_get_backtrace () = Printexc.get_backtrace ()

(** Initialization *)

let _ =
	Printexc.record_backtrace true;
	let old_controls = Gc.get () in
	let new_controls = { old_controls with
		Gc.minor_heap_size = 4 * 1024 * 1024 * 8 / Sys.word_size; (* 4MB *)
		Gc.major_heap_increment = 8 * 1024 * 1024 * 8 / Sys.word_size; (* 8MB *)
		Gc.max_overhead =   1000;
    Gc.space_overhead = 100;
	} in
	Gc.set new_controls

(** External progress monitor *)

type pm
external pm_start : pm -> string -> unit = "pm_start"
external pm_report: pm -> int -> int -> unit = "pm_report"
external pm_finish : pm -> unit = "pm_finish"

(** Internal progress monitor *)

let cb_pm_stderr_new () =
	Progress_monitor.of_out_channel stdout

let cb_pm_stderr_start pm message =
	pm.Progress_monitor.start message

let cb_pm_stderr_report pm state max =
	pm.Progress_monitor.report state max

let cb_pm_stderr_finish pm =
	pm.Progress_monitor.finish ()

(** Ontologies *)

type ontology = {
	axioms : Ontology.t;
	mutable saturation : Saturation.t option;
	mutable taxonomy : Class_taxonomy.t option;
}

let cb_ontology_add_class_axiom ont cax =
	Ontology.add_ClassAxiom ont.axioms cax

let cb_ontology_add_object_property_axiom ont opax =
	Ontology.add_ObjectPropertyAxiom ont.axioms opax

let cb_ontology_print_info ont =
	Ontology.print_info ont.axioms stdout;
	flush stdout

let cb_ontology_classify ont =
	let pm_stderr = Progress_monitor.of_out_channel stderr in
	let pt_stderr = Progress_tracker.of_progress_monitor pm_stderr in
	let saturation = Saturation.compute [pt_stderr] ont.axioms in
	ont.saturation <- Some saturation;
	let taxonomy =
		Class_taxonomy.compute [pt_stderr] saturation ont.axioms
	in
	ont.taxonomy <- Some taxonomy

(* operations on class representatives *)
let cb_ontology_get_equiv_repr ont ce =
	match ont.taxonomy with
	| None -> []
	| Some tax ->
			let record = Class_taxonomy.get_record tax ce
			in record.Class_taxonomy.equiv

let cb_get_direct_subs_repr ont ce =
	match ont.taxonomy with
	| None -> []
	| Some tax ->
			let record = Class_taxonomy.get_record tax ce
			in record.Class_taxonomy.subs

let cb_get_direct_sups_repr ont ce =
	match ont.taxonomy with
	| None -> []
	| Some tax ->
			let record = Class_taxonomy.get_record tax ce
			in record.Class_taxonomy.sups

let cb_ontology_get_unsat ont =
	cb_ontology_get_equiv_repr ont
	(ClassExpression.cons (CE.Class C.Nothing))	

let cb_ontology_classify_pm ont pm =
	let pt =
		Progress_tracker.of_progress_monitor {
				PM.start = pm_start pm;
				PM.report = pm_report pm;
				PM.finish = (fun () -> pm_finish pm);
			} in
	let pt_stderr = Progress_tracker.of_progress_monitor
			(Progress_monitor.of_out_channel stderr)
	in
	let saturation = Saturation.compute [pt; pt_stderr] ont.axioms in
	let taxonomy = Class_taxonomy.compute [pt; pt_stderr] saturation ont.axioms in
	ont.saturation <- Some saturation;
	ont.taxonomy <- Some taxonomy

let cb_ontology_new () = {
	axioms = Ontology.create ();
	saturation = None;
	taxonomy = None;
}

(** Class expressions *)

let cb_class_expression_print ce =
	let f = Format.formatter_of_out_channel stderr in
	Owl_io.fprint_ClassExpression f ce;
	Format.fprintf f "\n%!"

let cb_class_of_iri = function
	| "owl:Thing" -> Class_Constructor.Thing
	| "owl:Nothing" -> Class_Constructor.Nothing
	| iri -> Class_Constructor.IRI (
				IRI.cons (IRI_Constructor.IRI iri)
			)

let cb_thing_get () =
	ClassExpression.cons
		(ClassExpression_Constructor.Class
			Class_Constructor.Thing)

let cb_nothing_get () =
	ClassExpression.cons
		(ClassExpression_Constructor.Class
			Class_Constructor.Nothing)

let cb_class_get iri =
	ClassExpression.cons
		(ClassExpression_Constructor.Class
			(cb_class_of_iri iri))

let cb_object_intersection_of_get cea ceb =
	ClassExpression.cons
		(ClassExpression_Constructor.ObjectIntersectionOf (cea, ceb))

let cb_object_intersection_of_rec_get cea ceb =
	ClassExpression.cons
		(ClassExpression_Constructor.ObjectIntersectionOfrec (cea, ceb))

let cb_object_some_values_from_get ope ce =
	ClassExpression.cons
		(ClassExpression_Constructor.ObjectSomeValuesFrom (ope, ce))

(** Object property expressions *)

let cb_object_property_expression_print ope =
	let f = Format.formatter_of_out_channel stderr in
	Owl_io.fprint_ObjectPropertyExpression f ope;
	Format.fprintf f "\n%!"

let cb_top_object_property =
	ObjectProperty.cons
		(ObjectProperty_Constructor.TopObjectProperty)

let cb_bottom_object_property =
	ObjectProperty.cons
		(ObjectProperty_Constructor.BottomObjectProperty)

let cb_object_property_of_iri = function
	| "owl:TopObjectProperty" -> cb_top_object_property
	| "owl:BottomObjectProperty" -> cb_bottom_object_property
	| iri -> ObjectProperty.cons
				(ObjectProperty_Constructor.IRI
					(IRI.cons (IRI_Constructor.IRI iri))
				)

let cb_top_object_property_get () =
	ObjectPropertyExpression.cons
		(ObjectPropertyExpression_Constructor.ObjectProperty
			cb_top_object_property)

let cb_bottom_object_property_get () =
	ObjectPropertyExpression.cons
		(ObjectPropertyExpression_Constructor.ObjectProperty
			cb_bottom_object_property)

let cb_object_property_get iri =
	ObjectPropertyExpression.cons
		(ObjectPropertyExpression_Constructor.ObjectProperty
			(cb_object_property_of_iri iri))

let cb_object_inverse_of_get iri =
	ObjectPropertyExpression.cons
		(ObjectPropertyExpression_Constructor.ObjectInverseOf
			(cb_object_property_of_iri iri))

(** Class axioms *)

let cb_class_axiom_print ax =
	let f = Format.formatter_of_out_channel stderr in
	Owl_io.fprint_ClassAxiom f ax;
	Format.fprintf f "\n%!"

let cb_sub_class_of_axiom_get cea ceb =
	ClassAxiom.cons
		(ClassAxiom_Constructor.SubClassOf (cea, ceb))

let cb_equivalent_classes_axiom_get ce_lst =
	ClassAxiom.cons
		(ClassAxiom_Constructor.EquivalentClasses ce_lst)

(** Object property axioms *)

let cb_object_property_axiom_print ax =
	let f = Format.formatter_of_out_channel stderr in
	Owl_io.fprint_ObjectPropertyAxiom f ax;
	Format.fprintf f "\n%!"

let cb_sub_object_property_of_axiom_get ope_lst opeb =
	ObjectPropertyAxiom.cons
		(ObjectPropertyAxiom_Constructor.SubObjectPropertyOf (ope_lst, opeb))

let cb_inverse_object_properties_axiom_get opea opeb =
	ObjectPropertyAxiom.cons
		(ObjectPropertyAxiom_Constructor.InverseObjectProperties (opea, opeb))

let cb_functional_object_property_axiom_get ope =
	ObjectPropertyAxiom.cons
		(ObjectPropertyAxiom_Constructor.FunctionalObjectProperty ope)

let cb_inverse_functional_object_property_axiom_get ope =
	ObjectPropertyAxiom.cons
		(ObjectPropertyAxiom_Constructor.InverseFunctionalObjectProperty ope)

let cb_transitive_object_property_axiom_get ope =
	ObjectPropertyAxiom.cons
		(ObjectPropertyAxiom_Constructor.TransitiveObjectProperty ope)

(* Export the functions to C *)

let _ =
	(** Exception handling *)
	Callback.register "cb_exception_to_string" cb_exception_to_string;
	Callback.register "cb_backtrace_status" cb_backtrace_status;
	Callback.register "cb_get_backtrace" cb_get_backtrace;
	(** Progress monitor *)
	Callback.register "cb_pm_stderr_new" cb_pm_stderr_new;
	Callback.register "cb_pm_stderr_start" cb_pm_stderr_start;
	Callback.register "cb_pm_stderr_report" cb_pm_stderr_report;
	Callback.register "cb_pm_stderr_finish" cb_pm_stderr_finish;
	(** Ontologies *)
	Callback.register "cb_ontology_add_class_axiom" cb_ontology_add_class_axiom;
	Callback.register "cb_ontology_add_object_property_axiom"
		cb_ontology_add_object_property_axiom;
	Callback.register "cb_ontology_new" cb_ontology_new;
	Callback.register "cb_ontology_print_info" cb_ontology_print_info;
	Callback.register "cb_ontology_classify" cb_ontology_classify;
	Callback.register "cb_ontology_classify_pm" cb_ontology_classify_pm;
	(** Class expressions *)
	Callback.register "cb_class_expression_print" cb_class_expression_print;
	Callback.register "cb_thing_get" cb_thing_get;
	Callback.register "cb_nothing_get" cb_nothing_get;
	Callback.register "cb_class_get" cb_class_get;
	Callback.register "cb_object_intersection_of_get" cb_object_intersection_of_get;
	Callback.register "cb_object_intersection_of_rec_get" cb_object_intersection_of_rec_get;
	Callback.register "cb_object_some_values_from_get" cb_object_some_values_from_get;
	(** Object property expressions *)
	Callback.register "cb_object_property_expression_print" cb_object_property_expression_print;
	Callback.register "cb_top_object_property_get" cb_top_object_property_get;
	Callback.register "cb_bottom_object_property_get" cb_bottom_object_property_get;
	Callback.register "cb_object_property_get" cb_object_property_get;
	Callback.register "cb_object_inverse_of_get" cb_object_inverse_of_get;
	(** Class axioms *)
	Callback.register "cb_class_axiom_print" cb_class_axiom_print;
	Callback.register "cb_sub_class_of_axiom_get" cb_sub_class_of_axiom_get;
	Callback.register "cb_equivalent_classes_axiom_get" cb_equivalent_classes_axiom_get;
	(** Object property axioms *)
	Callback.register "cb_object_property_axiom_print" cb_object_property_axiom_print;
	Callback.register "cb_sub_object_property_of_axiom_get" cb_sub_object_property_of_axiom_get;
	Callback.register "cb_inverse_object_properties_axiom_get" cb_inverse_object_properties_axiom_get;
	Callback.register "cb_functional_object_property_axiom_get" cb_functional_object_property_axiom_get;
	Callback.register "cb_inverse_functional_object_property_axiom_get" cb_inverse_functional_object_property_axiom_get;
	Callback.register "cb_transitive_object_property_axiom_get" cb_transitive_object_property_axiom_get