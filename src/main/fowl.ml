(**=========== input-output for ontologies in krss format ============**)

open OwlSyntax
open Consed.T
module O = Ontology
module PB = ProgressBar

(**============ loading ontology from the input channel ==============**)

let load_ontology input =
  let lexbuf = Lexing.from_channel input in
  let ont =
    try
      Fowl_parser.owl_ontologyDocument Fowl_lexer.token lexbuf    
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
  (*|  Ontology.compute_polaritites ont;*)
  (*|  Gc.compact ();  (* <- slow but useful in the long run *)*)
  ont
;;

(**==================== printing the ontology  =======================**)

let str_of_object_property op =
  let module C = ObjectProperty.Constructor in
  match op.data with
  | C.ObjectPropertyIRI iri -> iri
  | C.TopObjectProperty -> "owl:topObjectProperty"
  | C.BottomObjectProperty -> "owl:bottomObjectProperty"
;;

let str_of_class c =
  let module C = Class.Constructor in
  match c.data with
  | C.ClassIRI iri -> iri
  | C.Thing -> "owl:Thing"
  | C.Nothing -> "owl:Nothing"
;;

let str_of_individual i =
  let module C = Individual.Constructor in
  match i.data with
  | C.IndividualIRI iri -> iri
;;

let str_of_object_property_expression ope =
  let module C = ObjectPropertyExpression.Constructor in
  match ope.data with
  | C.ObjectProperty op -> str_of_object_property op
  | C.InverseObjectProperty op -> "ObjectInverseOf(" ^ str_of_object_property op ^ ")"
;;

let rec str_of_class_expression c =
  let module C = ClassExpression.Constructor in
  match c.data with
  | C.Class ac -> str_of_class ac
  | C.ObjectIntersectionOf c_set -> "ObjectIntersectionOf(" ^ Cset.fold (fun c s -> s ^ " " ^ str_of_class_expression c) c_set "" ^ ")"
  | C.ObjectUnionOf c_set -> "ObjectUnionOf(" ^ Cset.fold (fun c s -> s ^ " " ^ str_of_class_expression c) c_set "" ^ ")"
  | C.ObjectComplementOf c1 -> "ObjectComplementOf(" ^ str_of_class_expression c1 ^ ")"
  | C.ObjectOneOf i_set -> "ObjectOneOf(" ^ Cset.fold (fun i s -> s ^ " " ^ str_of_individual i) i_set "" ^ ")"
  | C.ObjectSomeValuesFrom (r, c) -> "ObjectSomeValuesFrom(" ^ str_of_object_property_expression r ^ " " ^ str_of_class_expression c ^ ")"
  | C.ObjectAllValuesFrom (r, c) -> "ObjectAllValuesFrom(" ^ str_of_object_property_expression r ^ " " ^ str_of_class_expression c ^ ")"
  | C.ObjectHasValue (r, i) -> "ObjectHasValue(" ^ str_of_object_property_expression r ^ " " ^ str_of_individual i ^ ")"
  | C.ObjectHasSelf r -> "ObjectHasSelf(" ^ str_of_object_property_expression r ^ ")"
  | C.ObjectMinCardinality (n, r, co) -> "ObjectMinCardinality(" ^ str_of_object_property_expression r ^
      begin match co with
        | None -> ")"
        | Some c -> " " ^ str_of_class_expression c ^ ")"
      end
  | C.ObjectMaxCardinality (n, r, co) -> "ObjectMaxCardinality(" ^ str_of_object_property_expression r ^
      begin match co with
        | None -> ")"
        | Some c -> " " ^ str_of_class_expression c ^ ")"
      end
  | C.ObjectExactCardinality (n, r, co) -> "ObjectExactCardinality(" ^ str_of_object_property_expression r ^
      begin match co with
        | None -> ")"
        | Some c -> " " ^ str_of_class_expression c ^ ")"
      end
  | C.DataSomeValuesFrom -> "Unsupported"
  | C.DataAllValuesFrom -> "Unsupported"
  | C.DataHasValue -> "Unsupported"
  | C.DataMinCardinality -> "Unsupported"
  | C.DataMaxCardinality -> "Unsupported"
  | C.DataExactCardinality -> "Unsupported"
;;

let str_of_object_property_expression_axiom ax =
  let module C = ObjectPropertyAxiom.Constructor in
  match ax.data with
  | C.SubObjectPropertyOf ([r1], r2) -> "SubObjectPropertyOf(" ^ str_of_object_property_expression r1 ^ " " ^ str_of_object_property_expression r2 ^ ")"
  | C.SubObjectPropertyOf (r_lst, r) -> "SubObjectPropertyOf(" ^ List.fold_left (fun s r -> s ^ str_of_object_property_expression r ^ " ") "" r_lst ^
      str_of_object_property_expression r ^ ")"
  | C.EquivalentObjectProperties r_set -> "EquivalentObjectProperties(" ^ Cset.fold (fun r s -> s ^ " " ^ str_of_object_property_expression r) r_set "" ^ ")"
  | C.DisjointObjectProperties -> "Unsupported"
  | C.InverseObjectProperties (r1, r2) -> "InverseObjectProperties(" ^ str_of_object_property_expression r1 ^ " " ^ str_of_object_property_expression r2 ^ ")"
  | C.ObjectPropertyDomain -> "Unsupported"
  | C.ObjectPropertyRange -> "Unsupported"
  | C.FunctionalObjectProperty r -> "FunctionalObjectProperty(" ^ str_of_object_property_expression r ^ ")"
  | C.InverseFunctionalObjectProperty r -> "InverseFunctionalObjectProperty(" ^ str_of_object_property_expression r ^ ")"
  | C.ReflexiveObjectProperty -> "Unsupported"
  | C.IrreflexiveObjectProperty -> "Unsupported"
  | C.SymmetricObjectProperty -> "Unsupported"
  | C.AsymmetricObjectProperty -> "Unsupported"
  | C.TransitiveObjectProperty r -> "TransitiveObjectProperty(" ^ str_of_object_property_expression r ^ ")"
;;

let str_of_class_expression_axiom ax =
  let module C = ClassExpressionAxiom.Constructor in
  match ax.data with
  | C.SubClassOf (c1, c2) -> "SubClassOf(" ^ str_of_class_expression c1 ^ " " ^ str_of_class_expression c2 ^ ")"
  | C.EquivalentClasses c_set -> "EquivalentClasses(" ^ Cset.fold (fun c s -> s ^ " " ^ str_of_class_expression c) c_set "" ^ ")"
  | C.DisjointClasses -> "Unsupported"
  | C.DisjointUnion -> "Unsupported"
;;

let str_of_assertion ax =
  (* this is not a part of krss specification *)
  let module C = Assertion.Constructor in
  match ax.data with
  | C.SameIndividual -> "Unsupported"
  | C.DifferentIndividuals -> "Unsupported"
  | C.ClassAssertion (c, i) -> "ClassAssertion(" ^ (str_of_individual i) ^ " " ^ str_of_class_expression c ^ ")"
  | C.ObjectPropertyAssertion (r, i1, i2) ->
      "ObjectPropertyAssertion(" ^ (str_of_individual i1) ^ " " ^ (str_of_individual i2) ^ " " ^ str_of_object_property_expression r ^ ")"
  | C.NegativeObjectPropertyAssertion -> "Unsupported"
  | C.DataPropertyAssertion -> "Unsupported"
  | C.NegativeDataPropertyAssertion -> "Unsupported"
;;

(**============== printing the ontology into a file ==================**)

let print_ontology_ch ont out =
  Printf.fprintf out "Ontology(\n";
  O.iter_record_ObjectPropertyAxiom (fun ax ->
          Printf.fprintf out "%s\n" (str_of_object_property_expression_axiom ax)
    ) ont;
    
  O.iter_record_ClassExpressionAxiom (fun ax ->
          Printf.fprintf out "%s\n" (str_of_class_expression_axiom ax)
    ) ont;
    
  O.iter_record_Assertion (fun ax ->
          Printf.fprintf out "%s\n" (str_of_assertion ax)
    ) ont;
  Printf.fprintf out ")";  
;;

let save_ontology ont file_name =
  let file = open_out file_name in
  print_ontology_ch ont file;
  close_out file;
;;

let print_ontology ont out =
  print_ontology_ch ont out
;;
