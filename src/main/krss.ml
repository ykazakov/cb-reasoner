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
    (*|      Krss_parser.ontology Krss_lexer.token lexbuf*)
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
  | C.TopObjectProperty -> "TOP_ROLE"
  | C.BottomObjectProperty -> "BOTTOM_ROLE"
;;

let str_of_class c =
  let module C = Class.Constructor in
  match c.data with
  | C.ClassIRI iri -> iri
  | C.Thing -> "TOP"
  | C.Nothing -> "BOTTOM"
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
  | C.InverseObjectProperty op -> "(inv " ^ str_of_object_property op ^ ")"
;;

let rec str_of_class_expression c =
  let module C = ClassExpression.Constructor in
  match c.data with
  | C.Class ac -> str_of_class ac
  | C.ObjectIntersectionOf c_set -> "(and" ^ Cset.fold (fun c s -> s ^ " " ^ str_of_class_expression c) c_set "" ^ ")"
  | C.ObjectUnionOf c_set -> "(or" ^ Cset.fold (fun c s -> s ^ " " ^ str_of_class_expression c) c_set "" ^ ")"
  | C.ObjectComplementOf c1 -> "(complement " ^ str_of_class_expression c1 ^ ")"
  | C.ObjectOneOf _ -> "Unsupported"
  | C.ObjectSomeValuesFrom (r, c) -> "(some " ^ str_of_object_property_expression r ^ " " ^ str_of_class_expression c ^ ")"
  | C.ObjectAllValuesFrom (r, c) -> "(all " ^ str_of_object_property_expression r ^ " " ^ str_of_class_expression c ^ ")"
  | C.ObjectHasValue _ -> "Unsupported"
  | C.ObjectHasSelf _ -> "Unsupported"
  | C.ObjectMinCardinality (n, r, co) -> "(atleast " ^ str_of_object_property_expression r ^
      begin match co with
        | None -> ")"
        | Some c -> " " ^ str_of_class_expression c ^ ")"
      end
  | C.ObjectMaxCardinality (n, r, co) -> "(atmost " ^ str_of_object_property_expression r ^
      begin match co with
        | None -> ")"
        | Some c -> " " ^ str_of_class_expression c ^ ")"
      end
  | C.ObjectExactCardinality _ -> "Unsupported"
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
  | C.SubObjectPropertyOf ([r1], r2) -> "(implies-role " ^ str_of_object_property_expression r1 ^ " " ^ str_of_object_property_expression r2 ^ ")"
  | C.SubObjectPropertyOf (r_lst, r) -> "(composition " ^ List.fold_left (fun s r -> s ^ str_of_object_property_expression r ^ " ") "" r_lst ^
      str_of_object_property_expression r ^ ")"
  | C.EquivalentObjectProperties r_set -> "(equivalent-role " ^ Cset.fold (fun r s -> s ^ " " ^ str_of_object_property_expression r) r_set "" ^ ")"
  | C.DisjointObjectProperties -> "Unsupported"
  | C.InverseObjectProperties (r1, r2) -> "(inverse " ^ str_of_object_property_expression r1 ^ " " ^ str_of_object_property_expression r2 ^ ")"
  | C.ObjectPropertyDomain -> "Unsupported"
  | C.ObjectPropertyRange -> "Unsupported"
  | C.FunctionalObjectProperty r -> "(functional " ^ str_of_object_property_expression r ^ ")"
  | C.InverseFunctionalObjectProperty r -> "(functional " ^ "(inverse " ^ str_of_object_property_expression r ^ "))"
  | C.ReflexiveObjectProperty -> "Unsupported"
  | C.IrreflexiveObjectProperty -> "Unsupported"
  | C.SymmetricObjectProperty -> "Unsupported"
  | C.AsymmetricObjectProperty -> "Unsupported"
  | C.TransitiveObjectProperty r -> "(transitive " ^ str_of_object_property_expression r ^ ")"
;;

let str_of_class_expression_axiom ax =
  let module C = ClassExpressionAxiom.Constructor in
  match ax.data with
  | C.SubClassOf (c1, c2) -> "(implies " ^ str_of_class_expression c1 ^ " " ^ str_of_class_expression c2 ^ ")"
  | C.EquivalentClasses c_set -> "(equivalent " ^ Cset.fold (fun c s -> s ^ " " ^ str_of_class_expression c) c_set "" ^ ")"
  | C.DisjointClasses -> "Unsupported"
  | C.DisjointUnion -> "Unsupported"
;;

let str_of_assertion ax =
  (* this is not a part of krss specification *)
  let module C = Assertion.Constructor in
  match ax.data with
  | C.SameIndividual -> "Unsupported"
  | C.DifferentIndividuals -> "Unsupported"
  | C.ClassAssertion (c, i) -> "(instance " ^ (str_of_individual i) ^ " " ^ str_of_class_expression c ^ ")"
  | C.ObjectPropertyAssertion (r, i1, i2) ->
      "(instance-role " ^ (str_of_individual i1) ^ " " ^ (str_of_individual i2) ^ " " ^ str_of_object_property_expression r ^ ")"
  | C.NegativeObjectPropertyAssertion -> "Unsupported"
  | C.DataPropertyAssertion -> "Unsupported"
  | C.NegativeDataPropertyAssertion -> "Unsupported"
;;

(**============== printing the ontology into a file ==================**)

let print_ontology_ch ont out =
  Printf.fprintf out "\n;;================;;\n";
  Printf.fprintf out ";;  RBox axioms:  ;;\n";
  Printf.fprintf out ";;----------------;;\n\n";
  O.iter_record_ObjectPropertyAxiom (fun ax ->
          Printf.fprintf out "%s\n" (str_of_object_property_expression_axiom ax)
    ) ont;
  
  Printf.fprintf out "\n;;================;;\n";
  Printf.fprintf out ";;  TBox axioms:  ;;\n";
  Printf.fprintf out ";;----------------;;\n\n";
  O.iter_record_ClassExpressionAxiom (fun ax ->
          Printf.fprintf out "%s\n" (str_of_class_expression_axiom ax)
    ) ont;
  
  Printf.fprintf out "\n;;================;;\n";
  Printf.fprintf out ";;  ABox axioms:  ;;\n";
  Printf.fprintf out ";;----------------;;\n\n";
  O.iter_record_Assertion (fun ax ->
          Printf.fprintf out "%s\n" (str_of_assertion ax)
    ) ont;
;;

let save_ontology ont file_name =
  let file = open_out file_name in
  print_ontology_ch ont file;
  close_out file;
;;

let print_ontology ont out =
  print_ontology_ch ont out
;;
