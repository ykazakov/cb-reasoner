(** lexer for owl files in functional-style syntax according to the specification on
http://www.w3.org/TR/owl2-syntax/
*)

{
open Owl_fs_parser
module PT = Progress_tracker

let pt_lst_ref = ref []				
										    
let update_pos lexbuf =  
  let pos = lexbuf.Lexing.lex_curr_p in	
  let p = pos.Lexing.pos_cnum in
	PT.jump !pt_lst_ref p;
  lexbuf.Lexing.lex_curr_p <- 
  { pos with
    Lexing.pos_lnum = succ pos.Lexing.pos_lnum;
    Lexing.pos_bol = pos.Lexing.pos_cnum
    };
 ;;  

(* because of too many keywords, the automaton can be big; *)
(* instead we generate a hastable for lookup *)

let keyword_table = Hashtbl.create 140
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
  [ 
  (* Reserved Keywords *)
  "owl:backwardCompatibleWith",      Owl_backwardCompatibleWith; 
  "owl:bottomDataProperty",          Owl_bottomDataProperty;
  "owl:bottomObjectProperty",        Owl_bottomObjectProperty;  
  "owl:deprecated",                  Owl_deprecated;
  "owl:incompatibleWith",            Owl_incompatibleWith;
  "owl:Nothing",                     Owl_Nothing;
  "owl:priorVersion",                Owl_priorVersion;
  "owl:rational",                    Owl_rational; 
  "owl:real",                        Owl_real;
  "owl:versionInfo",                 Owl_versionInfo;  
  "owl:Thing",                       Owl_Thing;
  "owl:topDataProperty",             Owl_topDataProperty;
  "owl:topObjectProperty",           Owl_topObjectProperty;
  "rdf:langRange",                   Rdf_langRange;
  "rdf:PlainLiteral",                Rdf_PlainLiteral;
  "rdf:XMLLiteral",                  Rdf_XMLLiteral;
  "rdfs:comment",                    Rdfs_comment;
  "rdfs:isDefinedBy",                Rdfs_isDefinedBy;
  "rdfs:label",                      Rdfs_label;
  "rdfs:Literal",                    Rdfs_Literal;
  "rdfs:seeAlso",                    Rdfs_seeAlso;
  "xsd:anyURI",                      Xsd_anyURI;
  "xsd:base64Binary",                Xsd_base64Binary;
  "xsd:boolean",                     Xsd_boolean;
  "xsd:byte",                        Xsd_byte;
  "xsd:dateTime",                    Xsd_dateTime;
  "xsd:dateTimeStamp",               Xsd_dateTimeStamp;
  "xsd:decimal",                     Xsd_decimal;
  "xsd:double",                      Xsd_double;  
  "xsd:float",                       Xsd_float;
  "xsd:hexBinary",                   Xsd_hexBinary;  
  "xsd:int",                         Xsd_int;
  "xsd:integer",                     Xsd_integer;
  "xsd:language",                    Xsd_language;
  "xsd:length",                      Xsd_length;
  "xsd:long",                        Xsd_long;
  "xsd:maxExclusive",                Xsd_maxExclusive;
  "xsd:maxInclusive",                Xsd_maxInclusive;
  "xsd:maxLength",                   Xsd_maxLength;
  "xsd:minExclusive",                Xsd_minExclusive;
  "xsd:minInclusive",                Xsd_minInclusive;
  "xsd:minLength",                   Xsd_minLength;
  "xsd:Name",                        Xsd_Name;
  "xsd:NCName",                      Xsd_NCName;
  "xsd:negativeInteger",             Xsd_negativeInteger;
  "xsd:NMTOKEN",                     Xsd_NMTOKEN;
  "xsd:nonNegativeInteger",          Xsd_nonNegativeInteger;
  "xsd:nonPositiveInteger",          Xsd_nonPositiveInteger;
  "xsd:normalizedString",            Xsd_normalizedString;
  "xsd:pattern",                     Xsd_pattern;
  "xsd:positiveInteger",             Xsd_positiveInteger;
  "xsd:short",                       Xsd_short;
  "xsd:string",                      Xsd_string;
  "xsd:token",                       Xsd_token;
  "xsd:unsignedByte",                Xsd_unsignedByte;
  "xsd:unsignedInt",                 Xsd_unsignedInt;
  "xsd:unsignedLong",                Xsd_unsignedLong;
  "xsd:unsignedShort",               Xsd_unsignedShort;

(* Ontology Keywords *)
  "Prefix",                          Prefix;
  "Ontology",                        Ontology;
  "Import",                          Import;

(* Entities and Literals *)
  "Class",                           Class;
  "Datatype",                        Datatype;
  "ObjectProperty",                  ObjectProperty;
  "DataProperty",                    DataProperty;
  "AnnotationProperty",              AnnotationProperty;
  "NamedIndividual",                 NamedIndividual;
(* Declaration *)

  "Declaration",                     Declaration;
(* Property Expressions *)

  "ObjectInverseOf",                 ObjectInverseOf;
  "ObjectPropertyChain",             ObjectPropertyChain;
  
(* Data Ranges *)
  "ComplementOf",                    ComplementOf;
  "OneOf",                           OneOf;
  "DatatypeRestriction",             DatatypeRestriction;
  
(* Class Expressions *)  
  "ObjectIntersectionOf",            ObjectIntersectionOf;  
  "ObjectUnionOf",                   ObjectUnionOf;
  "ObjectComplementOf",              ObjectComplementOf;
  "ObjectOneOf",                     ObjectOneOf;
  "ObjectSomeValuesFrom",            ObjectSomeValuesFrom;
  "ObjectAllValuesFrom",             ObjectAllValuesFrom;
  "ObjectHasValue",                  ObjectHasValue;
  "ObjectHasSelf",                   ObjectHasSelf;
  "ObjectMinCardinality",            ObjectMinCardinality; 
  "ObjectMaxCardinality",            ObjectMaxCardinality;
  "ObjectExactCardinality",          ObjectExactCardinality;

  "DataIntersectionOf",              DataIntersectionOf;    
  "DataUnionOf",                     DataUnionOf;         
  "DataComplementOf",                DataComplementOf;    
  "DataOneOf",                       DataOneOf;
  "DataSomeValuesFrom",              DataSomeValuesFrom;  
  "DataAllValuesFrom",               DataAllValuesFrom;   
  "DataHasValue",                    DataHasValue;        
  "DataHasSelf",                     DataHasSelf;         
  "DataMinCardinality",              DataMinCardinality;  
  "DataMaxCardinality",              DataMaxCardinality;  
  "DataExactCardinality",            DataExactCardinality; 

(* Class Expressions Axioms *)
  "SubClassOf",                      SubClassOf ;
  "EquivalentClasses",               EquivalentClasses;
  "DisjointClasses",                 DisjointClasses  ;    
  "DisjointUnion",                   DisjointUnion;  

(* Object Property Axioms *) 
  "SubObjectPropertyOf",             SubObjectPropertyOf;
  "EquivalentObjectProperties",      EquivalentObjectProperties;
  "DisjointObjectProperties",        DisjointObjectProperties;
  "ObjectPropertyDomain",            ObjectPropertyDomain;
  "ObjectPropertyRange",             ObjectPropertyRange;
  "InverseObjectProperties",         InverseObjectProperties; 
  "FunctionalObjectProperty",        FunctionalObjectProperty;
  "InverseFunctionalObjectProperty", InverseFunctionalObjectProperty;
  "ReflexiveObjectProperty",         ReflexiveObjectProperty;
  "IrreflexiveObjectProperty",       IrreflexiveObjectProperty;
  "SymmetricObjectProperty",         SymmetricObjectProperty;
  "AsymmetricObjectProperty",        AsymmetricObjectProperty;
  "TransitiveObjectProperty",        TransitiveObjectProperty;
 
(* Data Property Axioms *)          
  "SubDataPropertyOf",               SubDataPropertyOf;          
  "EquivalentDataProperties",        EquivalentDataProperties;   
  "DisjointDataProperties",          DisjointDataProperties;     
  "DataPropertyDomain",              DataPropertyDomain;         
  "DataPropertyRange",               DataPropertyRange;              
  "InverseDataProperties",           InverseDataProperties;      
  "FunctionalDataProperty",          FunctionalDataProperty;     
  "DatatypeDefinition",              DatatypeDefinition;
  "InverseFunctionalDataProperty",   InverseFunctionalDataProperty; 
  "ReflexiveDataProperty",           ReflexiveDataProperty;         
  "IrreflexiveDataProperty",         IrreflexiveDataProperty;       
  "SymmetricDataProperty",           SymmetricDataProperty;         
  "AsymmetricDataProperty",          AsymmetricDataProperty;        
  "TransitiveDataProperty",          TransitiveDataProperty;

(* Keys *)        
  "HasKey",                          HasKey;
  
(* Assertions *)
  "SameIndividual",                  SameIndividual;
  "DifferentIndividuals",            DifferentIndividuals;
  "ClassAssertion",                  ClassAssertion;
  "ObjectPropertyAssertion",         ObjectPropertyAssertion;
  "NegativeObjectPropertyAssertion", NegativeObjectPropertyAssertion;
  "DataPropertyAssertion",           DataPropertyAssertion;
  "NegativeDataPropertyAssertion",   NegativeDataPropertyAssertion; 

(* Annotations *)
  "Annotation",                      Annotation;
  "AnnotationAssertion",             AnnotationAssertion;
  "SubAnnotationPropertyOf",         SubAnnotationPropertyOf;
  "AnnotationPropertyDomain",        AnnotationPropertyDomain;         
  "AnnotationPropertyRange",         AnnotationPropertyRange;
  ]
  
}

let ident = [^ ' ' '\t' '\n' '(' ')' '=' '^' '@' '<' '>']+ 

rule entry pt_lst = parse
  ""                                     {pt_lst_ref := pt_lst; token lexbuf }
and token = parse

(* Structural Symbols *)
    [' ' '\t']                           { token lexbuf }
  | '\n'                                 { update_pos lexbuf; token lexbuf}  
  | eof                                  { update_pos lexbuf; EOF }  
  | '('                                  { LeftParen    }
  | ')'                                  { RightParen   }
  | '='                                  { Equal        }
  | "^^"                                 { DoubleSuperscript }
  | '@'                                  { At }  
  | '<'                                  { LeftAngle }
  | '>'                                  { RightAngle }
  | '#'                                  { Hash }
(* comments annotations *)    
  | "//" [^ '\n']*                       { token lexbuf }
(* integers, Strings, and Node IDs *)
  | ('0' | ['1'-'9'] ['0'-'9']*)         { NonNegativeInteger (int_of_string (Lexing.lexeme lexbuf)) }
  | '"' ([^ '"' '\\']* (("\\\"" | "\\\\") [^ '"' '\\']*)* as s) '"' { 
      (* take into account that quoted string can span over multiple lines *)
      String.iter (fun c -> if c = '\n' then update_pos lexbuf) s;
      QuotedString (s)     
    } 
  | "_:" ident                  { NodeID (Lexing.lexeme lexbuf) }
	| "<" ident ">"               { FullIRI (Lexing.lexeme lexbuf) }
(* identifiers *)
  | ident as id    
     { try Hashtbl.find keyword_table id
       with Not_found -> Identifier (Lexing.lexeme lexbuf) }