/** parser for owl files in functional-style syntax according to the specification on
http://www.w3.org/TR/owl2-syntax/ */

/* non-terminal symbols are prefixed with "_" to be consistent with the notation from 
the specification; the symbol "-" is also replaced with "_". */

%{

open Owl2 
open Consed.T

(* abbreviations for commonly used modules *)
module O = Ontology
module D = Datatype.Constructor
module CF = ConstrainingFacet.Constructor
module OP = ObjectProperty.Constructor 
module DP = DataProperty.Constructor
module AP = AnnotationProperty.Constructor
module C = Class.Constructor
module I = Individual.Constructor
module L = Literal.Constructor
module OPE = ObjectPropertyExpression.Constructor
module DPE = DataPropertyExpression.Constructor
module DR = DataRange.Constructor
module CE = ClassExpression.Constructor
module CEA = ClassExpressionAxiom.Constructor
module OPA = ObjectPropertyAxiom.Constructor
module DPA = DataPropertyAxiom.Constructor
module DD = DatatypeDefinition.Constructor
module K = Key.Constructor
module A = Assertion.Constructor
module AS = AnnotationSubject.Constructor
module AV = AnnotationValue.Constructor
module AN = Annotation.Constructor
module AA = AnnotationAxiom.Constructor

let rec expand_ObjectIntersectionOf l =
  let c_set = ref Cset.empty in
  List.iter (fun c -> 
    match c.data with
      | CE.ObjectIntersectionOf c_set1 ->
          c_set := Cset.union c_set1 !c_set
       | _ -> c_set := Cset.add c !c_set
     ) l;
  !c_set
;;

%}

/* structure keywords */
%token LeftParen RightParen Equal LeftAngle RightAngle

/* reserved keywords */
%token Owl_backwardCompatibleWith Owl_bottomDataProperty Owl_bottomObjectProperty Owl_deprecated 
%token Owl_incompatibleWith Owl_Nothing Owl_priorVersion Owl_rational Owl_real Owl_versionInfo
%token Owl_Thing Owl_topDataProperty Owl_topObjectProperty Rdf_langRange Rdf_PlainLiteral Rdf_XMLLiteral
%token Rdfs_comment Rdfs_isDefinedBy Rdfs_label Rdfs_Literal Rdfs_seeAlso Xsd_anyURI Xsd_base64Binary
%token Xsd_boolean Xsd_byte Xsd_dateTime Xsd_dateTimeStamp Xsd_decimal Xsd_double Xsd_float Xsd_hexBinary
%token Xsd_int Xsd_integer Xsd_language Xsd_length Xsd_long Xsd_maxExclusive Xsd_maxInclusive Xsd_maxLength
%token Xsd_minExclusive Xsd_minInclusive Xsd_minLength Xsd_Name Xsd_NCName Xsd_negativeInteger Xsd_NMTOKEN
%token Xsd_nonNegativeInteger Xsd_nonPositiveInteger Xsd_normalizedString Xsd_pattern Xsd_positiveInteger
%token Xsd_short Xsd_string Xsd_token Xsd_unsignedByte Xsd_unsignedInt Xsd_unsignedLong Xsd_unsignedShort 

/* ontology keywords */
%token Prefix Ontology Import

/* Integers, Characters, Strings, Language Tags, and Node IDs */
%token <int> NonNegativeInteger 
%token <string> QuotedString 
%token <string> NodeID

/* entities and individuals */ 
%token Class Datatype ObjectProperty DataProperty AnnotationProperty NamedIndividual DoubleSuperscript At

/* declaration */
%token Declaration

/* property expressions */
%token ObjectInverseOf ObjectPropertyChain
  
/* data ranges */
%token ComplementOf OneOf DatatypeRestriction

/* class expressions */ 
%token ObjectIntersectionOf ObjectUnionOf ObjectComplementOf ObjectOneOf ObjectSomeValuesFrom ObjectAllValuesFrom
%token ObjectHasValue ObjectHasSelf ObjectMinCardinality ObjectMaxCardinality ObjectExactCardinality 
%token DataIntersectionOf DataUnionOf DataComplementOf DataOneOf DataSomeValuesFrom DataAllValuesFrom 
%token DataHasValue DataHasSelf DataMinCardinality DataMaxCardinality DataExactCardinality

/* class expressions axioms */
%token SubClassOf EquivalentClasses DisjointClasses DisjointUnion  

/* object property axioms, data property axioms, keys */ 
%token PropertyChain 
%token SubObjectPropertyOf EquivalentObjectProperties DisjointObjectProperties ObjectPropertyDomain 
%token ObjectPropertyRange InverseObjectProperties FunctionalObjectProperty InverseFunctionalObjectProperty 
%token ReflexiveObjectProperty IrreflexiveObjectProperty SymmetricObjectProperty AsymmetricObjectProperty 
%token TransitiveObjectProperty SubDataPropertyOf EquivalentDataProperties DisjointDataProperties DataPropertyDomain 
%token DataPropertyRange InverseDataProperties DatatypeDefinition FunctionalDataProperty InverseFunctionalDataProperty 
%token ReflexiveDataProperty IrreflexiveDataProperty SymmetricDataProperty AsymmetricDataProperty
%token TransitiveDataProperty HasKey 
  
/* assertions */
%token SameIndividual DifferentIndividuals ClassAssertion ObjectPropertyAssertion DataPropertyAssertion 
%token NegativeObjectPropertyAssertion NegativeDataPropertyAssertion
 
/* annotations */  
%token Annotation AnnotationAssertion SubAnnotationPropertyOf AnnotationPropertyDomain AnnotationPropertyRange

/* identifiers */
%token <string> Identifier

/* comments */
%token Ignore

/* eof */
%token EOF

/**==========================================================================================**/

%start owl_ontologyDocument
%type <Ontology.t> owl_ontologyDocument
%%

/* 2 Preliminary Definitions */

/* 2.3 Integers, Characters, Strings, Language Tags, and Node IDs */

owl_nonNegativeInteger: NonNegativeInteger { $1 }
owl_quotedString: QuotedString             { $1 }
owl_languageTag:    /* specified in BCP 47 [BCP 47] */
  | At Identifier                          { $2 }
owl_nodeID: NodeID                         { $1 }


/* 2.3 IRIs */

owl_fullIRI:       /* defined in [RFC3987] */
  | LeftAngle Identifier RightAngle  { "<" ^ $2 ^ ">" }
owl_prefixName:         /* a finite sequence of characters matching the as PNAME_NS production of [SPARQL] */
  | Identifier                 { $1 }
owl_abbreviatedIRI:     /* a finite sequence of characters matching the PNAME_LN production of [SPARQL] */
  | Identifier                 { $1 }  
owl_IRI: 
  | owl_fullIRI               { $1 } 
  | owl_abbreviatedIRI         { $1 }
 
 
/* 3 Ontologies */

/* 3.5 Ontology Annotations */
owl_ontologyAnnotations: /* empty */   {}
  | owl_ontologyAnnotations owl_Annotation {}

/* 3.7 Functional-Style Syntax */
owl_ontologyDocument:   
  | owl_prefixDeclarations owl_Ontology EOF   { $2 }
owl_prefixDeclarations: /* empty */          {}
  | owl_prefixDeclarations owl_prefixDeclaration {}
owl_prefixDeclaration:
  | Prefix LeftParen owl_prefixName Equal owl_fullIRI RightParen {}   
owl_Ontology:
  | Ontology LeftParen owl_directlyImportsDocuments owl_ontologyAnnotations owl_axioms RightParen
    { $5 }
  | Ontology LeftParen owl_ontologyIRI owl_directlyImportsDocuments owl_ontologyAnnotations owl_axioms RightParen
    { $6 }
  | Ontology LeftParen owl_ontologyIRI owl_versionIRI owl_directlyImportsDocuments owl_ontologyAnnotations 
    owl_axioms RightParen { $7 } 
owl_ontologyIRI: owl_IRI      {}
owl_versionIRI: owl_IRI       {}
owl_directlyImportsDocuments: /* empty */      {}
  | owl_directlyImportsDocuments Import LeftParen owl_IRI RightParen        {}
owl_axioms: /* empty */       { O.create () }  
  | owl_axioms owl_Axiom      { let ont = $1 in $2 ont; ont }

/* 4 Datatype Maps */

/* 4.1 Real Numbers, Decimal Numbers, and Integers */
owl_DTRealDecimalIntegers:
  | Owl_real                 { D.Owl_real }
  | Owl_rational             { D.Owl_rational }  
  | Xsd_decimal              { D.Xsd_decimal }
  | Xsd_integer              { D.Xsd_integer }
  | Xsd_nonNegativeInteger   { D.Xsd_nonNegativeInteger }
  | Xsd_nonPositiveInteger   { D.Xsd_nonPositiveInteger }
  | Xsd_positiveInteger      { D.Xsd_positiveInteger }
  | Xsd_negativeInteger      { D.Xsd_negativeInteger }
  | Xsd_long                 { D.Xsd_long }
  | Xsd_int                  { D.Xsd_int }
  | Xsd_short                { D.Xsd_short }
  | Xsd_byte                 { D.Xsd_byte }
  | Xsd_unsignedLong         { D.Xsd_unsignedLong }
  | Xsd_unsignedInt          { D.Xsd_unsignedInt }
  | Xsd_unsignedShort        { D.Xsd_unsignedShort }
  | Xsd_unsignedByte         { D.Xsd_unsignedByte }

/* 4.2 Floating-Point Numbers */
owl_DTFloats:
  | Xsd_double               { D.Xsd_double }
  | Xsd_float                { D.Xsd_float }

/* 4.3 Strings */
owl_DTStrings:
  | Xsd_string               { D.Xsd_string }
  | Xsd_normalizedString     { D.Xsd_normalizedString }
  | Xsd_token                { D.Xsd_token }
  | Xsd_language             { D.Xsd_language }
  | Xsd_Name                 { D.Xsd_Name }
  | Xsd_NCName               { D.Xsd_NCName }
  | Xsd_NMTOKEN              { D.Xsd_NMTOKEN }

/* 4.4 Boolean Values */
owl_DTBooleans:
  | Xsd_boolean              { D.Xsd_boolean }

/* 4.5 Binary Data */
owl_DTBinaries:
  | Xsd_hexBinary            { D.Xsd_hexBinary }
  | Xsd_base64Binary         { D.Xsd_base64Binary }

/* 4.6 IRIs */
owl_DTIRIs:
  | Xsd_anyURI               { D.Xsd_anyURI }

/* 4.7 Time Instants */
owl_DTTimes:
  | Xsd_dateTimeStamp        { D.Xsd_dateTimeStamp }

/* 4.8 XML Literals */
owl_DTXMLs:
  | Rdf_XMLLiteral           { D.Rdf_XMLLiteral }

/* 5 Entities and Literals */

/* 5.1 Classes */
owl_Class: owl_Class_        { Class.cons $1 }
owl_Class_:  
  | owl_IRI                  { C.IRI $1 }  
  | Owl_Thing                { C.Thing }
  | Owl_Nothing              { C.Nothing }

/* 5.2 Datatypes */
owl_Datatype: owl_Datatype_     { Datatype.cons $1 }
owl_Datatype_:
  | owl_IRI                     { D.IRI $1 }  
  | Rdfs_Literal                { D.Rdfs_Literal }
  | owl_DTRealDecimalIntegers   { $1 }
  | owl_DTFloats                { $1 }
  | owl_DTStrings               { $1 }
  | owl_DTBooleans              { $1 }
  | owl_DTBinaries              { $1 }
  | owl_DTIRIs                  { $1 }
  | owl_DTTimes                 { $1 }
  | owl_DTXMLs                  { $1 }
  
/* 5.3 Object Properties */   
owl_ObjectProperty: owl_ObjectProperty_ { ObjectProperty.cons $1 }
owl_ObjectProperty_: 
  | owl_IRI                  { OP.IRI $1 }
  | Owl_topObjectProperty    { OP.TopObjectProperty }
  | Owl_bottomObjectProperty { OP.BottomObjectProperty }

/* 5.4 Data Properties */
owl_DataProperty: owl_DataProperty_ { DataProperty.cons $1 }
owl_DataProperty_:
  | owl_IRI                  { DP.IRI $1 }
  | Owl_topDataProperty      { DP.TopDataProperty }
  | Owl_bottomDataProperty   { DP.BottomDataProperty }

/* 5.5 Annotation Properties */
owl_AnnotationProperty: owl_AnnotationProperty_ { AnnotationProperty.cons $1 }
owl_AnnotationProperty_:
  | owl_IRI                     { AP.IRI $1 }
  | Rdfs_label                  { AP.Rdfs_label }
  | Rdfs_comment                { AP.Rdfs_comment }
  | Rdfs_seeAlso                { AP.Rdfs_seeAlso }
  | Rdfs_isDefinedBy            { AP.Rdfs_isDefinedBy }
  | Owl_deprecated              { AP.Owl_deprecated }
  | Owl_priorVersion            { AP.Owl_priorVersion }
  | Owl_backwardCompatibleWith  { AP.Owl_backwardCompatibleWith }
  | Owl_incompatibleWith        { AP.Owl_incompatibleWith }

/* 5.6 Individuals */
owl_Individuals: /* empty */       { [] }
  | owl_Individuals owl_Individual { $2 :: $1 }
owl_Individual: owl_Individual_   { Individual.cons $1 } 
owl_Individual_:
  | owl_NamedIndividual           { $1 } 
  | owl_AnonymousIndividual       { $1 }

/* 5.6.1 Named Individuals */
owl_NamedIndividual: owl_IRI         { I.NamedIndividual $1 }

/* 5.6.2 Anonymous Individuals */
owl_AnonymousIndividual: owl_nodeID  { I.AnonymousIndividual $1 }

/* 5.7 Literals */
owl_Literals: /* empty */            { [] }  
  | owl_Literals owl_Literal         { $2 :: $1 }
owl_Literal: owl_Literal_            { Literal.cons $1 }
owl_Literal_:
  | owl_typedLiteral                 { $1 }
  | owl_stringLiteralNoLanguage      { $1 }
  | owl_stringLiteralWithLanguage    { $1 }
owl_typedLiteral:
  | owl_lexicalForm DoubleSuperscript owl_Datatype { L.TypedLiteral ($1, $3) }
owl_lexicalForm: owl_quotedString    { $1 }
owl_stringLiteralNoLanguage: 
  | owl_quotedString                 { L.StringLiteralNoLanguage $1 }
owl_stringLiteralWithLanguage: 
  | owl_quotedString owl_languageTag { L.StringLiteralWithLanguage ($1, $2) }

/* 5.8 Entity Declarations and Typing */
owl_Declaration:
  | Declaration LeftParen owl_axiomAnnotations owl_Entity RightParen  {}  
owl_Entity:
  | Class LeftParen owl_Class RightParen                           {}
  | Datatype LeftParen owl_Datatype RightParen                     {}
  | ObjectProperty LeftParen owl_ObjectProperty RightParen         {}
  | DataProperty LeftParen owl_DataProperty RightParen             {}
  | AnnotationProperty LeftParen owl_AnnotationProperty RightParen {}
  | NamedIndividual LeftParen owl_NamedIndividual RightParen       {}

/* 6 Property Expressions */
owl_ObjectPropertyExpressions:  /* empty */                 { [] }
  | owl_ObjectPropertyExpressions owl_ObjectPropertyExpression { $2 :: $1 }
owl_ObjectPropertyExpression: owl_ObjectPropertyExpression_ { ObjectPropertyExpression.cons $1}
owl_ObjectPropertyExpression_:
  | owl_ObjectProperty                 { OPE.ObjectProperty $1 }  
  | owl_InverseObjectProperty          { $1 }

/* 6.1.1 Inverse Object Properties */
owl_InverseObjectProperty:
  | ObjectInverseOf LeftParen owl_ObjectProperty RightParen { OPE.InverseObjectProperty $3 }

/* 6.2 Data Property Expressions */
owl_DataPropertyExpressions: /* empty */                   { [] }  
  | owl_DataPropertyExpressions owl_DataPropertyExpression { $2 :: $1 }
owl_DataPropertyExpression: owl_DataPropertyExpression_    { DataPropertyExpression.cons $1 }
owl_DataPropertyExpression_:
  | owl_DataProperty                                       { DPE.DataProperty $1 }  

/* 7 Data Ranges */
owl_DataRanges:  /* empty */          { [] }
  | owl_DataRanges owl_DataRange      { $2 :: $1 }
owl_DataRange: owl_DataRange_         { DataRange.cons $1 }
owl_DataRange_:
  | owl_Datatype                      { DR.Datatype $1 }
  | owl_DataIntersectionOf            { $1 }
  | owl_DataUnionOf                   { $1 }
  | owl_DataComplementOf              { $1 } 
  | owl_DataOneOf                     { $1 }
  | owl_DatatypeRestriction           { $1 }

/* 7.1 Intersection of Data Ranges */
owl_DataIntersectionOf:
  | DataIntersectionOf LeftParen owl_DataRange owl_DataRange owl_DataRanges RightParen 
    { DR.DataIntersectionOf (List.fold_left (fun s dr  -> Cset.add dr s) Cset.empty ($3 :: $4 :: $5)) }

/* 7.2 Union of Data Ranges */
owl_DataUnionOf:
  | DataUnionOf LeftParen owl_DataRange owl_DataRange owl_DataRanges RightParen  
    { DR.DataUnionOf (List.fold_left (fun s dr  -> Cset.add dr s) Cset.empty ($3 :: $4 :: $5)) }

/* 7.3 Complement of Data Ranges */
owl_DataComplementOf:
  | DataComplementOf LeftParen owl_DataRange RightParen { DR.DataComplementOf $3 }

/* 7.4 Enumeration of Literals */
owl_DataOneOf:
  | DataOneOf LeftParen owl_Literal owl_Literals RightParen 
    { DR.DataOneOf (List.fold_left (fun s dr  -> Cset.add dr s) Cset.empty ($3 :: $4)) }

/* 7.5 Datatype Restrictions */
owl_DatatypeRestriction: 
  | DatatypeRestriction LeftParen owl_Datatype owl_constrainingFacet owl_restrictionValue 
    owl_constrainingFacet_restrictionValues RightParen 
    { DR.DatatypeRestriction ($3, ($4, $5) :: $6 ) }
owl_constrainingFacet_restrictionValues:  /* empty */                                  { [] }  
  | owl_constrainingFacet_restrictionValues owl_constrainingFacet owl_restrictionValue { ($2, $3) :: $1 }
owl_constrainingFacet: owl_constrainingFacet_ { ConstrainingFacet.cons $1 }
owl_constrainingFacet_:
  | owl_IRI          { CF.IRI $1 }
  | Xsd_minInclusive { CF.Xsd_minInclusive }
  | Xsd_maxInclusive { CF.Xsd_maxInclusive }
  | Xsd_minExclusive { CF.Xsd_minExclusive }
  | Xsd_maxExclusive { CF.Xsd_maxExclusive }
  | Xsd_length       { CF.Xsd_length }
  | Xsd_minLength    { CF.Xsd_minLength }
  | Xsd_maxLength    { CF.Xsd_maxLength }
  | Xsd_pattern      { CF.Xsd_pattern }
  | Rdf_langRange    { CF.Rdf_langRange }
owl_restrictionValue: owl_Literal { $1 }

/* 8 Class Expressions */
owl_ClassExpressions:  /* empty */         { [] }
  | owl_ClassExpressions owl_ClassExpression  { $2 :: $1 }
owl_ClassExpression: owl_ClassExpression_ { ClassExpression.cons $1 }
owl_ClassExpression_:  
  | owl_Class                   { (CE.Class $1) }  
  | owl_ObjectIntersectionOf    { $1 }
  | owl_ObjectUnionOf           { $1 }
  | owl_ObjectComplementOf      { $1 }
  | owl_ObjectOneOf             { $1 }
  | owl_ObjectSomeValuesFrom    { $1 }
  | owl_ObjectAllValuesFrom     { $1 }
  | owl_ObjectHasValue          { $1 }
  | owl_ObjectHasSelf           { $1 }
  | owl_ObjectMinCardinality    { $1 }
  | owl_ObjectMaxCardinality    { $1 }
  | owl_ObjectExactCardinality  { $1 }
  | owl_DataSomeValuesFrom      { $1 }
  | owl_DataAllValuesFrom       { $1 }
  | owl_DataHasValue            { $1 }
  | owl_DataMinCardinality      { $1 }
  | owl_DataMaxCardinality      { $1 }
  | owl_DataExactCardinality    { $1 }

/* 8.1 Propositional Connectives and Enumeration of Individuals */

/* 8.1.1 Intersection of Class Expressions */
owl_ObjectIntersectionOf: 
  | ObjectIntersectionOf LeftParen owl_ClassExpression owl_ClassExpression owl_ClassExpressions RightParen 
    { CE.ObjectIntersectionOf (expand_ObjectIntersectionOf ($3 :: $4 :: $5)) }

/* 8.1.2 Union of Class Expressions */
owl_ObjectUnionOf:
  | ObjectUnionOf LeftParen owl_ClassExpression owl_ClassExpression owl_ClassExpressions RightParen 
    { CE.ObjectUnionOf (List.fold_left (fun s ce  -> Cset.add ce s) Cset.empty ($3 :: $4 :: $5)) }
 
/* 8.1.3 Complement of Class Expressions */
owl_ObjectComplementOf:
  | ObjectComplementOf LeftParen owl_ClassExpression RightParen 
    { CE.ObjectComplementOf ($3) }

/* 8.1.4 Enumeration of Individuals */
owl_ObjectOneOf: 
  | ObjectOneOf LeftParen owl_Individual owl_Individuals RightParen 
    { CE.ObjectOneOf (List.fold_left (fun s ce  -> Cset.add ce s) Cset.empty ($3 :: $4)) }

/* 8.2 Object Property Restrictions */

/* 8.2.1 Existential Quantification */
owl_ObjectSomeValuesFrom:
  | ObjectSomeValuesFrom LeftParen owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { CE.ObjectSomeValuesFrom ($3, $4) }
 
/* 8.2.2 Universal Quantification */
owl_ObjectAllValuesFrom:
  | ObjectAllValuesFrom LeftParen owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { CE.ObjectAllValuesFrom ($3, $4) }

/* 8.2.3 Individual Value Restriction */
owl_ObjectHasValue:
  | ObjectHasValue LeftParen owl_ObjectPropertyExpression owl_Individual RightParen 
    { CE.ObjectHasValue ($3, $4) }

/* 8.2.4 Self-Restriction */
owl_ObjectHasSelf: 
  | ObjectHasSelf LeftParen owl_ObjectPropertyExpression RightParen  
    { CE.ObjectHasSelf $3 }

/* 8.3 Object Property Cardinality Restrictions */

/* 8.3.1 Minimum Cardinality */
owl_ObjectMinCardinality:
  | ObjectMinCardinality LeftParen owl_nonNegativeInteger owl_ObjectPropertyExpression RightParen  
    { CE.ObjectMinCardinality ($3, $4, None) }
  | ObjectMinCardinality LeftParen owl_nonNegativeInteger owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { CE.ObjectMinCardinality ($3, $4, Some $5) }

/* 8.3.2 Maximum Cardinality */
owl_ObjectMaxCardinality:
  | ObjectMaxCardinality LeftParen owl_nonNegativeInteger owl_ObjectPropertyExpression RightParen 
    { CE.ObjectMaxCardinality ($3, $4, None) }
  | ObjectMaxCardinality LeftParen owl_nonNegativeInteger owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { CE.ObjectMaxCardinality ($3, $4, Some $5) }

/* 8.3.3 Exact Cardinality */
owl_ObjectExactCardinality:
  | ObjectExactCardinality LeftParen owl_nonNegativeInteger owl_ObjectPropertyExpression RightParen 
    { CE.ObjectExactCardinality ($3, $4, None) }
  | ObjectExactCardinality LeftParen owl_nonNegativeInteger owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { CE.ObjectExactCardinality ($3, $4, Some $5) }

/* 8.4 Data Property Restrictions */

/* 8.4.1 Existential Quantification */
owl_DataSomeValuesFrom:
  | DataSomeValuesFrom LeftParen owl_DataPropertyExpression owl_DataPropertyExpressions owl_DataRange RightParen 
    { CE.DataSomeValuesFrom ($3 :: $4,  $5) }

/* 8.4.2 Universal Quantification */
owl_DataAllValuesFrom:
  | DataAllValuesFrom LeftParen owl_DataPropertyExpression owl_DataPropertyExpressions owl_DataRange RightParen 
    { CE.DataAllValuesFrom ($3 :: $4,  $5) }

/* 8.4.3 Literal Value Restriction */
owl_DataHasValue:
  | DataHasValue LeftParen owl_DataPropertyExpression owl_Literal RightParen 
    { CE.DataHasValue ($3, $4) }

/* 8.5 Data Property Cardinality Restrictions */

/* 8.5.1 Minimum Cardinality */
owl_DataMinCardinality:
  | DataMinCardinality LeftParen owl_nonNegativeInteger owl_DataPropertyExpression RightParen 
    { CE.DataMinCardinality ($3, $4, None) }
  | DataMinCardinality LeftParen owl_nonNegativeInteger owl_DataPropertyExpression owl_DataRange RightParen 
    { CE.DataMinCardinality ($3, $4, Some $5) }

/* 8.5.2 Maximum Cardinality */
owl_DataMaxCardinality:
  | DataMaxCardinality LeftParen owl_nonNegativeInteger owl_DataPropertyExpression RightParen 
    { CE.DataMaxCardinality ($3, $4, None) }
  | DataMaxCardinality LeftParen owl_nonNegativeInteger owl_DataPropertyExpression owl_DataRange RightParen 
    { CE.DataMaxCardinality ($3, $4, Some $5) } 

/* 8.5.3 Exact Cardinality */
owl_DataExactCardinality:
  | DataExactCardinality LeftParen owl_nonNegativeInteger owl_DataPropertyExpression RightParen 
    { CE.DataExactCardinality ($3, $4, None) }
  | DataExactCardinality LeftParen owl_nonNegativeInteger owl_DataPropertyExpression owl_DataRange RightParen 
    { CE.DataExactCardinality ($3, $4, Some $5) }

/* 9 Axioms */
owl_Axiom:
  | owl_Declaration         { fun ont -> () }
  | owl_ClassAxiom          { fun ont -> O.add_ClassExpressionAxiom ont $1 }
  | owl_ObjectPropertyAxiom { fun ont -> O.add_ObjectPropertyAxiom ont $1 }
  | owl_DataPropertyAxiom   { fun ont -> () }
  | owl_DatatypeDefinition  { fun ont -> () }
  | owl_HasKey              { fun ont -> () }
  | owl_Assertion           { fun ont -> O.add_Assertion ont $1 }
  | owl_AnnotationAxiom     { fun ont -> ()}
owl_axiomAnnotations: /* empty */    {}
  | owl_axiomAnnotations owl_Annotation {}

/* 9.1 Class Expression Axioms */
owl_ClassAxiom: owl_ClassAxiom_ { ClassExpressionAxiom.cons $1 }
owl_ClassAxiom_:
  | owl_SubClassOf        { $1 }
  | owl_EquivalentClasses { $1 }
  | owl_DisjointClasses   { $1 }
  | owl_DisjointUnion     { $1 }

/* 9.1.1 Subclass Axioms */
owl_SubClassOf:
  | SubClassOf LeftParen owl_axiomAnnotations owl_subClassExpression owl_superClassExpression RightParen 
    { CEA.SubClassOf ($4, $5) }
owl_subClassExpression: owl_ClassExpression   { $1 }
owl_superClassExpression: owl_ClassExpression { $1 }

/* 9.1.2 Equivalent Classes */
owl_EquivalentClasses:
  | EquivalentClasses LeftParen owl_axiomAnnotations owl_ClassExpression owl_ClassExpression owl_ClassExpressions RightParen 
    { CEA.EquivalentClasses (List.fold_left (fun s ce  -> Cset.add ce s) Cset.empty ($4 :: $5 :: $6)) }
    
/* 9.1.3 Disjoint Classes */
owl_DisjointClasses:
  | DisjointClasses LeftParen owl_axiomAnnotations owl_ClassExpression owl_ClassExpression owl_ClassExpressions RightParen 
    { CEA.DisjointClasses (List.fold_left (fun s ce  -> Cset.add ce s) Cset.empty ($4 :: $5 :: $6)) }
    
/* 9.1.4 Disjoint Union of Class Expressions */
owl_DisjointUnion:
  | DisjointUnion LeftParen owl_axiomAnnotations owl_Class owl_disjointClassExpressions RightParen 
    { CEA.DisjointUnion ($4, List.fold_left (fun s ce  -> Cset.add ce s) Cset.empty $5) }
owl_disjointClassExpressions: /* empty */ { [] }
  | owl_ClassExpression owl_ClassExpression owl_ClassExpressions { $1 :: $2 :: $3 }

/* 9.2 Object Property Axioms */
owl_ObjectPropertyAxiom: owl_ObjectPropertyAxiom_ { ObjectPropertyAxiom.cons $1 }
owl_ObjectPropertyAxiom_:
  | owl_SubObjectPropertyOf             { $1 }
  | owl_EquivalentObjectProperties      { $1 }
  | owl_DisjointObjectProperties        { $1 }
  | owl_InverseObjectProperties         { $1 }
  | owl_ObjectPropertyDomain            { $1 }
  | owl_ObjectPropertyRange             { $1 }
  | owl_FunctionalObjectProperty        { $1 }
  | owl_InverseFunctionalObjectProperty { $1 }
  | owl_ReflexiveObjectProperty         { $1 }
  | owl_IrreflexiveObjectProperty       { $1 }
  | owl_SymmetricObjectProperty         { $1 }
  | owl_AsymmetricObjectProperty        { $1 }
  | owl_TransitiveObjectProperty        { $1 }  

/* 9.2.1 Object Subproperties */
owl_SubObjectPropertyOf:
  | SubObjectPropertyOf LeftParen owl_axiomAnnotations owl_subObjectPropertyExpression owl_superObjectPropertyExpression RightParen 
    { OPA.SubObjectPropertyOf ($4, $5) }
owl_subObjectPropertyExpression:
  | owl_ObjectPropertyExpression { [$1] }
  | owl_propertyExpressionChain  { $1 }
owl_propertyExpressionChain:
  | ObjectPropertyChain LeftParen owl_ObjectPropertyExpression owl_ObjectPropertyExpression owl_ObjectPropertyExpressions RightParen 
    { $3 :: $4 :: $5 }
owl_superObjectPropertyExpression: owl_ObjectPropertyExpression { $1 }

/* 9.2.2 Equivalent Object Properties */
owl_EquivalentObjectProperties:
  | EquivalentObjectProperties LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_ObjectPropertyExpression  
    owl_ObjectPropertyExpressions RightParen 
    { OPA.EquivalentObjectProperties (List.fold_left (fun s ce  -> Cset.add ce s) Cset.empty ($4 :: $5 :: $6)) }

/* 9.2.3 Disjoint Object Properties */
owl_DisjointObjectProperties:
  | DisjointObjectProperties LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_ObjectPropertyExpression
    owl_ObjectPropertyExpressions RightParen 
    { OPA.DisjointObjectProperties (List.fold_left (fun s ce  -> Cset.add ce s) Cset.empty ($4 :: $5 :: $6)) }

/* 9.2.4 Inverse Object Properties */
owl_InverseObjectProperties:
  | InverseObjectProperties LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_ObjectPropertyExpression RightParen 
    { OPA.InverseObjectProperties ($4, $5) }

/* 9.2.5 Object Property Domain */
owl_ObjectPropertyDomain: 
  | ObjectPropertyDomain LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { OPA.ObjectPropertyDomain ($4, $5) }

/* 9.2.6 Object Property Range */
owl_ObjectPropertyRange: 
  | ObjectPropertyRange LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { OPA.ObjectPropertyRange ($4, $5) }

/* 9.2.7 Functional Object Properties */
owl_FunctionalObjectProperty: 
  | FunctionalObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPA.FunctionalObjectProperty ($4) }

/* 9.2.8 Inverse-Functional Object Properties */
owl_InverseFunctionalObjectProperty: 
  | InverseFunctionalObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPA.InverseFunctionalObjectProperty ($4) }

/* 9.2.9 Reflexive Object Properties */
owl_ReflexiveObjectProperty: 
  | ReflexiveObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPA.ReflexiveObjectProperty ($4) }

/* 9.2.10 Irreflexive Object Properties */
owl_IrreflexiveObjectProperty: 
  | IrreflexiveObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPA.IrreflexiveObjectProperty ($4) }
    
/* 9.2.11 Symmetric Object Properties */    
owl_SymmetricObjectProperty: 
  | SymmetricObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPA.SymmetricObjectProperty ($4) }

/* 9.2.12 Asymmetric Object Properties */
owl_AsymmetricObjectProperty: 
  | AsymmetricObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPA.AsymmetricObjectProperty ($4) }

/* 9.2.13 Transitive Object Properties */
owl_TransitiveObjectProperty: 
  | TransitiveObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPA.TransitiveObjectProperty ($4) }

/* 9.3 Data Property Axioms */
owl_DataPropertyAxiom: owl_DataPropertyAxiom_ { DataPropertyAxiom.cons $1 }
owl_DataPropertyAxiom_:
  | owl_SubDataPropertyOf        { $1 }
  | owl_EquivalentDataProperties { $1 }
  | owl_DisjointDataProperties   { $1 }
  | owl_DataPropertyDomain       { $1 }
  | owl_DataPropertyRange        { $1 }
  | owl_FunctionalDataProperty   { $1 }

/* 9.3.1 Data Subproperties */
owl_SubDataPropertyOf:
  | SubDataPropertyOf LeftParen owl_axiomAnnotations owl_subDataPropertyExpression owl_superDataPropertyExpression RightParen 
    { DPA.SubDataPropertyOf ($4, $5) }
owl_subDataPropertyExpression: owl_DataPropertyExpression   { $1 } 
owl_superDataPropertyExpression: owl_DataPropertyExpression { $1 }

/* 9.3.2 Equivalent Data Properties */
owl_EquivalentDataProperties:
  | EquivalentDataProperties LeftParen owl_axiomAnnotations owl_DataPropertyExpression owl_DataPropertyExpression 
    owl_DataPropertyExpressions RightParen 
    { DPA.EquivalentDataProperties (List.fold_left (fun s dpe  -> Cset.add dpe s) Cset.empty ($4 :: $5 :: $6)) }

/* 9.3.3 Disjoint Data Properties */
owl_DisjointDataProperties:
  | DisjointDataProperties LeftParen owl_axiomAnnotations owl_DataPropertyExpression owl_DataPropertyExpression 
    owl_DataPropertyExpressions RightParen 
    { DPA.EquivalentDataProperties (List.fold_left (fun s dpe  -> Cset.add dpe s) Cset.empty ($4 :: $5 :: $6)) }

/* 9.3.4 Data Property Domain */
owl_DataPropertyDomain: 
  | DataPropertyDomain LeftParen owl_axiomAnnotations owl_DataPropertyExpression owl_ClassExpression RightParen 
    { DPA.DataPropertyDomain ($4, $5) }

/* 9.3.5 Data Property Range */
owl_DataPropertyRange: 
  | DataPropertyRange LeftParen owl_axiomAnnotations owl_DataPropertyExpression owl_DataRange RightParen
    { DPA.DataPropertyRange ($4, $5) }

/* 9.3.6 Functional Data Properties */
owl_FunctionalDataProperty: 
  | FunctionalDataProperty LeftParen owl_axiomAnnotations owl_DataPropertyExpression RightParen
    { DPA.FunctionalDataProperty ($4) }

/* 9.4 Datatype Definitions */
owl_DatatypeDefinition: owl_DatatypeDefinition_ { DatatypeDefinition.cons $1 }
owl_DatatypeDefinition_: 
  | DatatypeDefinition LeftParen owl_axiomAnnotations owl_Datatype owl_DataRange RightParen 
    { DD.DatatypeDefinition ($4, $5) }

/* 9.5 Keys */
owl_HasKey:
  | HasKey LeftParen owl_axiomAnnotations owl_ClassExpression 
    LeftParen owl_ObjectPropertyExpressions RightParen LeftParen owl_DataPropertyExpression RightParen {}

/* 9.6 Assertions */
owl_Assertion: owl_Assertion_           { Assertion.cons $1 }
owl_Assertion_:
  | owl_SameIndividual                  { $1 }
  | owl_DifferentIndividuals            { $1 }
  | owl_ClassAssertion                  { $1 }
  | owl_ObjectPropertyAssertion         { $1 }
  | owl_NegativeObjectPropertyAssertion { $1 }
  | owl_DataPropertyAssertion           { $1 }
  | owl_NegativeDataPropertyAssertion   { $1 }
owl_sourceIndividual: owl_Individual { $1 }
owl_targetIndividual: owl_Individual { $1 }
owl_targetValue: owl_Literal { $1 }

/* 9.6.1 Individual Equality */
owl_SameIndividual: 
  | SameIndividual LeftParen owl_axiomAnnotations owl_Individual owl_Individual owl_Individuals RightParen 
    { A.SameIndividual (List.fold_left (fun s i  -> Cset.add i s) Cset.empty ($4 :: $5 :: $6)) }

/* 9.6.2 Individual Inequality */
owl_DifferentIndividuals: 
  | DifferentIndividuals LeftParen owl_axiomAnnotations owl_Individual owl_Individual owl_Individuals RightParen 
    { A.DifferentIndividuals (List.fold_left (fun s i  -> Cset.add i s) Cset.empty ($4 :: $5 :: $6)) }

/* 9.6.3 Class Assertions */
owl_ClassAssertion: 
  | ClassAssertion LeftParen owl_axiomAnnotations owl_ClassExpression owl_Individual RightParen 
    { A.ClassAssertion ($4, $5) }

/* 9.6.4 Positive Object Property Assertions */
owl_ObjectPropertyAssertion: 
  | ObjectPropertyAssertion LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_sourceIndividual 
    owl_targetIndividual RightParen 
    { A.ObjectPropertyAssertion ($4, $5, $6) }

/* 9.6.5 Negative Object Property Assertions */
owl_NegativeObjectPropertyAssertion: 
  | NegativeObjectPropertyAssertion LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_sourceIndividual 
    owl_targetIndividual RightParen 
    { A.NegativeObjectPropertyAssertion ($4, $5, $6) }

/* 9.6.6 Positive Data Property Assertions */
owl_DataPropertyAssertion: 
  | DataPropertyAssertion LeftParen owl_axiomAnnotations owl_DataPropertyExpression owl_sourceIndividual owl_targetValue RightParen 
    { A.DataPropertyAssertion ($4, $5, $6) }

/* 9.6.7 Negative Data Property Assertions */
owl_NegativeDataPropertyAssertion: 
  | NegativeDataPropertyAssertion LeftParen owl_axiomAnnotations owl_DataPropertyExpression owl_sourceIndividual 
    owl_targetValue RightParen 
    { A.NegativeDataPropertyAssertion ($4, $5, $6) }

/* 10 Annotations */

/* 10.1 Annotations of Ontologies, Axioms, and other Annotations */
owl_Annotations: /* empty */   {}
  | owl_Annotations owl_Annotation {}
owl_Annotation:
  | Annotation LeftParen owl_annotationAnnotations owl_AnnotationProperty owl_AnnotationValue RightParen {}
 
owl_annotationAnnotations: owl_Annotations {}
owl_AnnotationValue: 
  | owl_AnonymousIndividual {}
  | owl_IRI                 {}
  | owl_Literal             {}

/* 10.2 Annotation Axioms */
owl_AnnotationAxiom:
  | owl_AnnotationAssertion      {}
  | owl_SubAnnotationPropertyOf  {}
  | owl_AnnotationPropertyDomain {}
  | owl_AnnotationPropertyRange  {}

/* 10.2.1 Annotation Assertion */
owl_AnnotationAssertion:
  | AnnotationAssertion LeftParen owl_axiomAnnotations owl_AnnotationProperty owl_AnnotationSubject owl_AnnotationValue RightParen {}
owl_AnnotationSubject:
  | owl_IRI                 {}
  | owl_AnonymousIndividual {}

/* 10.2.2 Annotation Subproperties */
owl_SubAnnotationPropertyOf:
  | SubAnnotationPropertyOf LeftParen owl_axiomAnnotations owl_subAnnotationProperty owl_superAnnotationProperty RightParen {}
owl_subAnnotationProperty: owl_AnnotationProperty {}
owl_superAnnotationProperty: owl_AnnotationProperty {}  

/* 10.2.3 Annotation Property Domain */
owl_AnnotationPropertyDomain: AnnotationPropertyDomain LeftParen owl_axiomAnnotations owl_AnnotationProperty owl_IRI RightParen {}

/* 10.2.4 Annotation Property Range */
owl_AnnotationPropertyRange: AnnotationPropertyRange LeftParen owl_axiomAnnotations owl_AnnotationProperty owl_IRI RightParen {}
