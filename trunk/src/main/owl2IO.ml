(**=========== input-output for ontologies in functional style syntax ============**)

open Owl2
open Consed.T
module O = Ontology
module PB = ProgressBar
module F = Format

(**=========== loading from the input channel ===========**)

let load_ontology input =
  let lexbuf = Lexing.from_channel input in
  let ont =
    try
      Owl2_fs_parser.owl_ontologyDocument Owl2_fs_lexer.token lexbuf
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
    Cset.iter (fun e ->
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

(**====================== Datatypes =====================**)

let fprint_Datatype f dt =
  let module C = Datatype.Constructor in
  F.fprintf f "%s"
    begin match dt.data with
      | C.IRI iri -> iri
      | C.Rdfs_Literal -> "rdfs:Literal"
      | C.Owl_real -> "owl:real"
      | C.Owl_rational -> "owl:rational"
      | C.Xsd_decimal -> "owl:decimal"
      | C.Xsd_integer -> "xsd:integer"
      | C.Xsd_nonNegativeInteger -> "xsd:nonNegativeInteger"
      | C.Xsd_nonPositiveInteger -> "xsd:nonPositiveInteger"
      | C.Xsd_positiveInteger -> "xsd:positiveInteger"
      | C.Xsd_negativeInteger -> "xsd:negativeInteger"
      | C.Xsd_long -> "xsd:long"
      | C.Xsd_int -> "xsd:int"
      | C.Xsd_short -> "xsd:short"
      | C.Xsd_byte -> "xsd:byte"
      | C.Xsd_unsignedLong -> "xsd:unsignedLong"
      | C.Xsd_unsignedInt -> "xsd:unsignedInt"
      | C.Xsd_unsignedShort -> "xsd:unsignedShort"
      | C.Xsd_unsignedByte -> "xsd:unsignedByte"
      | C.Xsd_double -> "xsd:double"
      | C.Xsd_float -> "xsd:float"
      | C.Xsd_string -> "xsd:string"
      | C.Xsd_normalizedString -> "xsd:normalizedString"
      | C.Xsd_token -> "xsd:token"
      | C.Xsd_language -> "xsd:language"
      | C.Xsd_Name -> "xsd:Name"
      | C.Xsd_NCName -> "xsd:NCName"
      | C.Xsd_NMTOKEN -> "xsd:NMTOKEN"
      | C.Xsd_boolean -> "xsd:boolean"
      | C.Xsd_hexBinary -> "xsd:hexBinary"
      | C.Xsd_base64Binary -> "xsd:base64Binary"
      | C.Xsd_anyURI -> "xsd:anyURI"
      | C.Xsd_dateTime -> "xsd:dateTime"
      | C.Xsd_dateTimeStamp -> "xsd:dateTimeStamp"
      | C.Rdf_XMLLiteral -> "rdf:XMLLiteral"
    end
;;

(**================== Constraining Facets ===============**)

let fprint_ConstrainingFacet f cf =
  let module C = ConstrainingFacet.Constructor in
  F.fprintf f "%s"
    begin match cf.data with
      | C.IRI iri -> iri
      | C.Xsd_minInclusive -> "xsd:minInclusive"
      | C.Xsd_maxInclusive -> "xsd:maxInclusive"
      | C.Xsd_minExclusive -> "xsd:minExclusive"
      | C.Xsd_maxExclusive -> "xsd:maxExclusive"
      | C.Xsd_length -> "xsd:length"
      | C.Xsd_minLength -> "xsd:minLength"
      | C.Xsd_maxLength -> "xsd:maxLength"
      | C.Xsd_pattern -> "xsd:pattern"
      | C.Rdf_langRange -> "rdf:langRange"
    end
;;

(**================= Object Properties ==================**)

let fprint_ObjectProperty f op =
  let module C = ObjectProperty.Constructor in
  F.fprintf f "%s"
    begin match op.data with
      | C.IRI iri -> iri
      | C.TopObjectProperty -> "owl:topObjectProperty"
      | C.BottomObjectProperty -> "owl:bottomObjectProperty"
    end
;;

(**=================== Data Properties ==================**)

let fprint_DataProperty f dp =
  let module C = DataProperty.Constructor in
  F.fprintf f "%s"
    begin match dp.data with
      | C.IRI iri -> iri
      | C.TopDataProperty -> "owl:topDataProperty"
      | C.BottomDataProperty -> "owl:bottomDataProperty"
    end
;;

(**================ Annotation Properties ===============**)

let fprint_AnnotationProperty f ap =
  let module C = AnnotationProperty.Constructor in
  F.fprintf f "%s"
    begin match ap.data with
      | C.IRI iri -> iri
      | C.Rdfs_label -> "rdfs:label"
      | C.Rdfs_comment -> "rdfs:comment"
      | C.Rdfs_seeAlso -> "rdfs:seeAlso"
      | C.Rdfs_isDefinedBy -> "rdfs:isDefinedBy"
      | C.Owl_deprecated -> "owl:deprecated"
      | C.Owl_versionInfo -> "owl:versionInfo"
      | C.Owl_priorVersion -> "owl:priorVersion"
      | C.Owl_backwardCompatibleWith -> "owl:backwardCompatibleWith"
      | C.Owl_incompatibleWith -> "owl:incompatibleWith"
    end
;;

(**====================== Classes =======================**)

let fprint_Class f c =
  let module C = Class.Constructor in
  F.fprintf f "%s"
    begin match c.data with
      | C.IRI iri -> iri
      | C.Thing -> "owl:Thing"
      | C.Nothing -> "owl:Nothing"
    end
;;

(**==================== Individuals =====================**)

let fprint_Individual f i =
  let module C = Individual.Constructor in
  F.fprintf f "%s"
    begin match i.data with
      | C.NamedIndividual iri -> iri
      | C.AnonymousIndividual st -> st
    end
;;

(**======================= Literals =====================**)

let fprint_Literal f lt =
  let module C = Literal.Constructor in
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

(**============ Object Property Expressions =============**)

let fprint_ObjectPropertyExpression f ope =
  let module C = ObjectPropertyExpression.Constructor in
  begin match ope.data with
    | C.ObjectProperty op ->
        fprint_ObjectProperty f op;
    | C.InverseObjectProperty op ->
        F.fprintf f "@[<hv 2>InverseObjectProperty(@,";
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

(**============= Data Property Expressions ==============**)

let fprint_DataPropertyExpression f dpe =
  let module C = DataPropertyExpression.Constructor in
  begin match dpe.data with
    | C.DataProperty dp -> fprint_DataProperty f dp
  end
;;

(**==================== Data Ranges =====================**)

let rec fprint_DataRange f dr =
  let module C = DataRange.Constructor in
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

(**==================== Class Expressions ==================**)

let rec fprint_ClassExpression f ce =
  let module C = ClassExpression.Constructor in
  begin match ce.data with
    | C.Class c ->
        fprint_Class f c;
    | C.ObjectIntersectionOf ce_set ->
        F.fprintf f "@[<hv 2>ObjectIntersectionOf(@,";
        P.fprint_cset f fprint_ClassExpression "@ " ce_set;
        F.fprintf f "@;<0 -2>)@]";
    | C.ObjectUnionOf ce_set ->
        F.fprintf f "@[<hv 2>ObjectUnionOf(@,";
        P.fprint_cset f fprint_ClassExpression "@ " ce_set;
        F.fprintf f "@;<0 -2>)@]";
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

(**================= Class Expression Axioms ===============**)

let fprint_ClassExpressionAxiom f ax =
  let module C = ClassExpressionAxiom.Constructor in
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

(**================= Object Property Axioms ================**)

let fprint_ObjectPropertyAxiom f ax =
  let module C = ObjectPropertyAxiom.Constructor in
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

(**================= Data Property Axioms ================**)

let fprint_DataPropertyAxiom f ax =
  let module C = DataPropertyAxiom.Constructor in
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

(**====================== Assertions ======================**)

let fprint_Assertion f ax =
  let module C = Assertion.Constructor in
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

(**======================== Ontology =======================**)

let fprint_ontology f ont =  
  (* periodically flushing to improve the performance of GC *)
  F.fprintf f "@[<v 2>Ontology(";
  O.iter_record_ObjectPropertyAxiom
    (fun ax -> F.fprintf f "@?@[<v 2>@,"; fprint_ObjectPropertyAxiom f ax) ont;  
  O.iter_record_ClassExpressionAxiom
    (fun ax -> F.fprintf f "@?@[<v 2>@,"; fprint_ClassExpressionAxiom f ax) ont;    
  O.iter_record_Assertion
    (fun ax -> F.fprintf f "@?@[<v 2>@,"; fprint_Assertion f ax) ont;
  F.fprintf f "@;<0 -2>)@]%!";  
;;

let print_ontology_ch ont out =  
  let f = F.formatter_of_out_channel out in
  fprint_ontology f ont;
;;

let save_ontology ont file_name =
  let file = open_out file_name in
  print_ontology_ch ont file;
  close_out file;
;;

(**===================== printing into strings =======================**)

let str_of_Datatype dt =
  fprint_Datatype F.str_formatter dt;
  F.flush_str_formatter ()
;;

let str_of_ConstrainingFacet cf =
  fprint_ConstrainingFacet F.str_formatter cf;
  F.flush_str_formatter ()
;;

let str_of_ObjectProperty op =
  fprint_ObjectProperty F.str_formatter op;
  F.flush_str_formatter ()
;;

let str_of_DataProperty dp =
  fprint_DataProperty F.str_formatter dp;
  F.flush_str_formatter ()
;;

let str_of_Class c =
  fprint_Class F.str_formatter c;
  F.flush_str_formatter ()
;;

let str_of_Individual i =
  fprint_Individual F.str_formatter i;
  F.flush_str_formatter ()
;;

let str_of_Literal lt =
  fprint_Literal F.str_formatter lt;
  F.flush_str_formatter ()
;;

let str_of_ObjectPropertyExpression ope =
  fprint_ObjectPropertyExpression F.str_formatter ope;
  F.flush_str_formatter ()
;;

let str_of_DataPropertyExpression dpe =
  fprint_DataPropertyExpression F.str_formatter dpe;
  F.flush_str_formatter ()
;;

let str_of_DataRange dr =
  fprint_DataRange F.str_formatter dr;
  F.flush_str_formatter ()
;;

let str_of_ClassExpression ce =
  fprint_ClassExpression F.str_formatter ce;
  F.flush_str_formatter ()
;;

let str_of_ClassExpressionAxiom ax =
  fprint_ClassExpressionAxiom F.str_formatter ax;
  F.flush_str_formatter ()
;;

let str_of_ObjectPropertyAxiom ax =
  fprint_ObjectPropertyAxiom F.str_formatter ax;
  F.flush_str_formatter ()
;;

let str_of_DataPropertyAxiom ax =
  fprint_DataPropertyAxiom F.str_formatter ax;
  F.flush_str_formatter ()
;;

let str_of_Assertion ax =
  fprint_Assertion F.str_formatter ax;
  F.flush_str_formatter ()
;;