open OwlSyntax
open Consed.T
module F = Format

(**================= Object Properties ==================**)

let print_ObjectProperty f op =
  let module C = ObjectProperty.Constructor in
  F.fprintf f "@[%s@]"
    begin match op.data with
      | C.ObjectPropertyIRI iri -> iri
      | C.TopObjectProperty -> "owl:topObjectProperty"
      | C.BottomObjectProperty -> "owl:bottomObjectProperty"
    end
;;

(**====================== Classes =======================**)

let print_Class f c =
  let module C = Class.Constructor in
  F.fprintf f "@[%s@]"
    begin match c.data with
      | C.ClassIRI iri -> iri
      | C.Thing -> "owl:Thing"
      | C.Nothing -> "owl:Nothing"
    end
;;

(**==================== Individuals =====================**)

let print_Individual f i =
  let module C = Individual.Constructor in
  F.fprintf f "@[%s@]"
    begin match i.data with
      | C.IndividualIRI iri -> iri
    end
;;

(**============ Object Property Expressions =============**)

let print_ObjectPropertyExpression f ope =
  let module C = ObjectPropertyExpression.Constructor in
  F.pp_open_box f 0;
  begin match ope.data with
    | C.ObjectProperty op ->
        F.pp_print_string f "ObjectProperty(";
        F.pp_print_cut f ();
        print_ObjectProperty f op;
        F.pp_print_string f ")";
    | C.InverseObjectProperty op ->
        F.pp_print_string f "InverseObjectProperty(";
        F.pp_print_cut f ();
        print_ObjectProperty f op;
        F.pp_print_string f ")";
  end;
  F.pp_close_box f ();
;;

(* auxiliary function for printing a list of values [print_List f prt sep  *)
(* lst] prints elements of the list [lst] using the printer [prnt]         *)
(* according to format [f]; the elements are separated with a formatting   *)
(* string [sep]                                                            *)
let rec print_list f prt sep = function
  | [] -> ()
  | ce :: tl ->
      prt f ce;
      List.iter (fun ce ->
              F.fprintf f sep;              
              F.pp_print_break f 1 0;
              prt f ce;
        ) tl;
;;

let print_set f prt sep set =
  let flag_sep = ref false in
  Cset.iter (fun ce ->
          if !flag_sep then (
            F.fprintf f sep;
            F.pp_print_break f 1 0;
          );
          flag_sep := true;
          prt f ce;
    ) set;
;;

(**==================== Class Expressions ==================**)

let rec print_ClassExpression f ce =
  let module C = ClassExpression.Constructor in
  F.pp_open_box f 0;
  begin match ce.data with
    | C.Class c ->
        print_Class f c;
    | C.ObjectIntersectionOf ce_set ->
        F.pp_print_string f "ObjectIntersectionOf(";
        F.pp_print_cut f (); 
        print_set f print_ClassExpression "@;<1 0>" ce_set;
        F.pp_print_string f ")";
    | C.ObjectUnionOf ce_set ->
        F.pp_print_string f "ObjectUnionOf(";
        F.pp_print_cut f ();
        print_set f print_ClassExpression "@;<1 0>" ce_set;
        F.pp_print_string f ")";
    | C.ObjectComplementOf ce ->
        F.pp_print_string f "ObjectComplementOf(";
        F.pp_print_cut f ();
        print_ClassExpression f ce;
        F.pp_print_string f ")";
    | C.ObjectOneOf i_set ->
        F.pp_print_string f "ObjectOneOf(";
        F.pp_print_cut f ();
        print_set f print_Individual "@;<1 0>" i_set;
        F.pp_print_string f ")";
    | C.ObjectSomeValuesFrom (ope, ce) ->
        F.pp_print_string f "ObjectSomeValuesFrom(";
        F.pp_print_cut f ();
        print_ObjectPropertyExpression f ope;
        F.pp_print_break f 1 0;
        print_ClassExpression f ce;
        F.pp_print_string f ")";
    | C.ObjectAllValuesFrom (ope, ce) ->
        F.pp_print_string f "ObjectAllValuesFrom(";
        F.pp_print_cut f ();
        print_ObjectPropertyExpression f ope;
        F.pp_print_break f 1 0;
        print_ClassExpression f ce;
        F.pp_print_string f ")";
    | C.ObjectHasValue (ope, i) ->
        F.pp_print_string f "ObjectHasValue(";
        F.pp_print_cut f ();
        print_ObjectPropertyExpression f ope;
        F.pp_print_break f 1 0;
        print_Individual f i;
        F.pp_print_string f ")";
    | C.ObjectHasSelf ope ->
        F.pp_print_string f "ObjectHasSelf(";
        F.pp_print_cut f ();
        print_ObjectPropertyExpression f ope;
        F.pp_print_string f ")";
    | C.ObjectMinCardinality (n, ope, ceo) ->
        F.pp_print_string f "ObjectMinCardinality(";
        F.pp_print_cut f ();
        F.fprintf f "@[%n@]" n;
        F.pp_print_break f 1 0;
        print_ObjectPropertyExpression f ope;
        print_ClassExpression_option f ceo;
        F.pp_print_string f ")";
    | C.ObjectMaxCardinality (n, ope, ceo) ->
        F.pp_print_string f "ObjectMaxCardinality(";
        F.pp_print_cut f ();
        F.fprintf f "@[%n@]" n;
        F.pp_print_break f 1 0;
        print_ObjectPropertyExpression f ope;
        print_ClassExpression_option f ceo;
        F.pp_print_string f ")";
    | C.ObjectExactCardinality (n, ope, ceo) ->
        F.pp_print_string f "ObjectExactCardinality(";
        F.pp_print_cut f ();
        F.fprintf f "@[%n@]" n;
        F.pp_print_break f 1 0;
        print_ObjectPropertyExpression f ope;
        print_ClassExpression_option f ceo;
        F.pp_print_string f ")";
    | C.DataSomeValuesFrom -> ()
    | C.DataAllValuesFrom -> ()
    | C.DataHasValue -> ()
    | C.DataMinCardinality -> ()
    | C.DataMaxCardinality -> ()
    | C.DataExactCardinality -> ()
  end;
  F.pp_close_box f ();
and print_ClassExpression_option f ceo =
  match ceo with
  | None -> ()
  | Some ce ->
      F.pp_print_break f 1 0;
      print_ClassExpression f ce;
;;

(**================= Object Property Axioms ================**)

let print_ObjectPropertyAxiom f (ax, _) =
  let module C = ObjectPropertyAxiom.Constructor in
  F.pp_open_box f 0;
  begin match ax with
    | C.SubObjectPropertyOf (op_ch, op) ->
        F.pp_print_string f "SubObjectPropertyOf(";
        F.pp_print_cut f ();
        F.pp_open_box f 0;
        F.pp_print_string f "ObjectPropertyChain(";
        F.pp_print_cut f ();
        print_list f print_ObjectPropertyExpression "@;<1 0>" op_ch;
        F.pp_print_string f ")";
        F.pp_close_box f ();
        F.pp_print_break f 1 0;
        print_ObjectPropertyExpression f op;
        F.pp_print_string f ")";
    | C.EquivalentObjectProperties (ope_set) ->
        F.pp_print_string f "EquivalentObjectProperties(";
        F.pp_print_cut f ();
        print_set f print_ObjectPropertyExpression "@;<1 0>" ope_set;
        F.pp_print_string f ")";
    | C.DisjointObjectProperties -> ()
    | C.InverseObjectProperties (ope1, ope2) ->
        F.pp_print_string f "InverseObjectProperties(";
        F.pp_print_cut f ();
        print_ObjectPropertyExpression f ope1;
        F.pp_print_break f 1 0;
        print_ObjectPropertyExpression f ope2;
        F.pp_print_string f ")";
    | C.ObjectPropertyDomain -> ()
    | C.ObjectPropertyRange -> ()
    | C.FunctionalObjectProperty ope ->
        F.pp_print_string f "FunctionalObjectProperty(";
        F.pp_print_cut f ();
        print_ObjectPropertyExpression f ope;
        F.pp_print_string f ")";
    | C.InverseFunctionalObjectProperty ope ->
        F.pp_print_string f "InverseFunctionalObjectProperty(";
        F.pp_print_cut f ();
        print_ObjectPropertyExpression f ope;
        F.pp_print_string f ")";
    | C.ReflexiveObjectProperty -> ()
    | C.IrreflexiveObjectProperty -> ()
    | C.SymmetricObjectProperty -> ()
    | C.AsymmetricObjectProperty -> ()
    | C.TransitiveObjectProperty ope ->
        F.pp_print_string f "TransitiveObjectProperty(";
        F.pp_print_cut f ();
        print_ObjectPropertyExpression f ope;
        F.pp_print_string f ")";
  end;
  F.pp_close_box f ();
;;

(**================= Class Expression Axioms ===============**)

let print_ClassExpressionAxiom f (ax, _) =
  let module C = ClassExpressionAxiom.Constructor in
  F.pp_open_box f 0;
  begin match ax with
    | C.SubClassOf (ce1, ce2) ->
        F.pp_print_string f "SubClassOf(";
        F.pp_print_cut f ();
        print_ClassExpression f ce1;
        F.pp_print_break f 1 0;
        print_ClassExpression f ce2;
        F.pp_print_string f ")";
    | C.EquivalentClasses (ce_set) ->
        F.pp_print_string f "EquivalentClasses(";
        F.pp_print_cut f ();
        print_set f print_ClassExpression "@;<1 0>" ce_set;
        F.pp_print_string f ")";
    | C.DisjointClasses -> ()
    | C.DisjointUnion -> ()
  end;
  F.pp_close_box f ();
;;

(**====================== Assertions ======================**)

let print_Assertion f (ax, _) =
  let module C = Assertion.Constructor in
  F.pp_open_box f 0;
  begin match ax with
    | C.SameIndividual -> ()
    | C.DifferentIndividuals -> ()
    | C.ClassAssertion (c, i) ->
        F.pp_print_string f "ClassAssertion(";
        F.pp_print_cut f ();
        print_ClassExpression f c;
        F.pp_print_break f 1 0;
        print_Individual f i;
        F.pp_print_string f ")";
    | C.ObjectPropertyAssertion (ope, i1, i2) ->
        F.pp_print_string f "PropertyAssertion(";
        F.pp_print_cut f ();
        print_ObjectPropertyExpression f ope;
        F.pp_print_break f 1 0;
        print_Individual f i1;
        F.pp_print_break f 1 0;
        print_Individual f i2;
        F.pp_print_string f ")";
    | C.NegativeObjectPropertyAssertion -> ()
    | C.DataPropertyAssertion -> ()
    | C.NegativeDataPropertyAssertion -> ()
  end;
  F.pp_close_box f ();
;;

(**====================== Axioms ===========================**)

(*|let print_Axiom f ax =                            *)
(*|  F.pp_open_box f 0;                              *)
(*|  match ax with                                   *)
(*|  | ObjectPropertyAxiom ax ->                     *)
(*|      F.pp_print_string f "ObjectPropertyAxiom("; *)
(*|      print_ObjectPropertyAxiom f ax;             *)
(*|      F.pp_print_cut f ();                     *)
(*|      F.pp_print_string f ")";                    *)
(*|  | ClassExpressionAxiom ax ->                    *)
(*|      F.pp_print_string f "ClassExpressionAxiom(";*)
(*|      print_ClassExpressionAxiom f ax;            *)
(*|      F.pp_print_cut f ();                     *)
(*|      F.pp_print_string f ")";                    *)
(*|  | Assertion ax ->                               *)
(*|      F.pp_print_string f "ClassExpressionAxiom(";*)
(*|      print_Assertion f ax;                       *)
(*|      F.pp_print_cut f ();                     *)
(*|      F.pp_print_string f ")";                    *)
(*|      F.pp_close_box f ();                        *)
(*|  | Unsupported -> ()                             *)
(*|;;                                                *)
