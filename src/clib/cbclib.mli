(** C interface functions for cb *)

open Owl

(** Exception handling *)

val cb_exception_to_string : exn -> string
val cb_backtrace_status : unit -> bool
val cb_get_backtrace : unit -> string

(** Internal progress monitor *)

val cb_pm_stderr_new : unit -> Progress_monitor.t
val cb_pm_stderr_start : Progress_monitor.t -> string -> unit
val cb_pm_stderr_report : Progress_monitor.t -> int -> int -> unit    
val cb_pm_stderr_finish : Progress_monitor.t -> unit

(** Ontologies *)

type ontology

val cb_ontology_add_class_axiom : ontology -> ClassAxiom.t -> unit
(** [cb_ontology_add_class_axiom ont cax] adds to the 
    ontology [ont] the class axiom [cax] *)

val cb_ontology_add_object_property_axiom : ontology ->
    ObjectPropertyAxiom.t -> unit
(** [cb_ontology_add_object_property_axiom ont opeax] adds to the 
    ontology [ont] the object property axiom [opeax] *)

val cb_ontology_print_info : ontology -> unit
(** [cb_ontology_print_info ont] prints statistics for ontology [ont] *)

val cb_ontology_classify : ontology -> unit
(** [cb_ontology_classify ont] classifies the ontology [ont] *)

val cb_ontology_new : unit -> ontology
(** [cb_ontology_new ()] retrns a new empty ontology *)

(** Class expressions *)

val cb_class_expression_print : ClassExpression.t -> unit
(** [cb_class_expression_print ce] prints the class expression [ce]
    in functional-style owl syntax *)

val cb_thing_get : unit -> ClassExpression.t
(** [cb_thing_get ()] returns the class associated with [owl:Thing] *)

val cb_nothing_get : unit -> ClassExpression.t
(** [cb_nothing_get ()] returns the class associated with [owl:Nothing] *)

val cb_class_get : string -> ClassExpression.t
(** [cb_class_get iri] returns class express expression associated
    with [iri] *)

val cb_object_intersection_of_get : ClassExpression.t -> 
	ClassExpression.t -> ClassExpression.t
(** [cb_object_intersection_of_get cea ceb] returns class expression 
    which is the object intersection of class expressions [cea] and 
		[ceb] *)

val cb_object_intersection_of_rec_get : ClassExpression.t -> 
    ClassExpression.t -> ClassExpression.t
(** [cb_object_intersection_of_get cea ceb] returns class expression 
    which is the intermediate object intersection of class expressions 
		[cea] and [ceb] *)

val cb_object_some_values_from_get : ObjectPropertyExpression.t ->
	ClassExpression.t -> ClassExpression.t
(** [cb_object_some_values_from_get ope ce] returns class expression 
    which is the restriction for the object property expression [ope] 
		and class expression [ce] *)			

(** Object property expressions *)

val cb_object_property_expression_print : ObjectPropertyExpression.t -> unit
(** [cb_object_property_expression_print ope] prints the object property 
    expression [ope] in functional-style owl syntax *)

val cb_top_object_property_get : unit -> ObjectPropertyExpression.t
(** [cb_top_object_property_get ()] returns the object property expression 
    associated with [owl:TopObjectProperty] *)

val cb_bottom_object_property_get : unit -> ObjectPropertyExpression.t
(** [cb_bottom_object_property_get ()] returns the object property expression 
    associated with [owl:BottomObjectProperty] *)
		
val cb_object_property_get : string -> ObjectPropertyExpression.t
(** [cb_object_property_get iri] returns the object property expression 
    associated with [iri] *)
		
val cb_object_inverse_of_get : string -> ObjectPropertyExpression.t
(** [cb_object_inverse_of_get iri] returns the object property expression 
    associated with the invers of [iri] *)
																		
(** Class axioms *)		

val cb_class_axiom_print : ClassAxiom.t -> unit
(** [cb_class_expression_axiom_print ax] prints the class axiom [ax] in 
    functional-style owl syntax *)
		
val cb_sub_class_of_axiom_get : ClassExpression.t -> 
	ClassExpression.t -> ClassAxiom.t
(** [cb_object_intersection_of_get cea ceb] returns class axiom stating 
    that [cea] is a sub-class of [ceb] *)		

val cb_equivalent_classes_axiom_get : ClassExpression.t list -> 
	ClassAxiom.t
(** [cb_equivalent_classes_axiom_get ce_lst] returns class axiom stating 
    that all class expressions in the list [ce_lst] are equivalent *)	
						
(** Object property axioms *)      

val cb_object_property_axiom_print : ObjectPropertyAxiom.t -> unit
(** [cb_object_property_axiom_print ax] prints the object property 
    axiom [ax] in functional-style owl syntax *)
        
val cb_sub_object_property_of_axiom_get : 
    ObjectPropertyExpression.t list -> ObjectPropertyExpression.t -> 
			ObjectPropertyAxiom.t
(** [cb_sub_object_property_of_axiom_get ope_lst opeb] returns object 
    property axiom stating that the chain of roles in the list [ope_lst] 
		is a sub-object-property of [opeb] *)     		