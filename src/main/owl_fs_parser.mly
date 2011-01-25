/** parser for owl files in functional-style syntax according to the specification on
http://www.w3.org/TR/owl2-syntax/ */

/* non-terminal symbols are prefixed with "_" to be consistent with the notation from 
the specification; the symbol "-" is also replaced with "_". */

%{

open Owl

(* abbreviations for commonly used modules *)
module O = Ontology
module IRIc = IRI_Constructor
module NIDc = NodeID_Constructor
module Dc = Datatype_Constructor
module CFc = ConstrainingFacet_Constructor
module OPc = ObjectProperty_Constructor
module DPc = DataProperty_Constructor
module APc = AnnotationProperty_Constructor
module Cc = Class_Constructor
module Ic = Individual_Constructor
module Lc = Literal_Constructor
module OPEc = ObjectPropertyExpression_Constructor
module DPEc = DataPropertyExpression_Constructor
module DRc = DataRange_Constructor
module CEc = ClassExpression_Constructor
module CAc = ClassAxiom_Constructor
module OPAc = ObjectPropertyAxiom_Constructor
module DPAc = DataPropertyAxiom_Constructor
module DDc = DatatypeDefinition_Constructor
module Kc = Key_Constructor
module Ac = Assertion_Constructor
module ASc = AnnotationSubject_Constructor
module AVc = AnnotationValue_Constructor
module ANc = Annotation_Constructor
module AAc = AnnotationAxiom_Constructor

(* temporary variables to track the polarity *)
let pol = ref Polarity.Both
(* for constructors with both polarities such as ObjectExactCardinality *)
let pol_save = ref Polarity.Both 
let pol_nesting = ref 0

(* the variable to store the ontology *)
let ont = ref None

let o () = match !ont with
  | Some ont -> ont
  | None -> invalid_arg "owl2_fs_parser.o"

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
%token Underscore Hash

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

/* IRIs, NodeIDs */
%token <string> Identifier
%token <string> NodeID
%token <string> FullIRI

/* comments */
%token Ignore

/* eof */
%token EOF

/**==========================================================================================**/

%start entry
%type <Ontology.t> entry
%%

/* Initializing Ontology */
entry: init owl_ontologyDocument { $1; $2; o () }
init: /* empty */ { ont := Some (Ontology.create ()) }

/* 2 Preliminary Definitions */

/* 2.3 Integers, Characters, Strings, Language Tags, and Node IDs */

owl_nonNegativeInteger: NonNegativeInteger { $1 }
owl_quotedString: QuotedString             { $1 }
owl_languageTag:    /* specified in BCP 47 [BCP 47] */
  | At Identifier                          { $2 }
owl_nodeID: NodeID                         { NodeID.cons (NIDc.NodeID $1) } /* specified in [RDF Test Cases] */


/* 2.3 IRIs */

owl_fullIRI: FullIRI                     { $1 }  
owl_prefixName: Identifier               { $1 }
owl_IRI: owl_IRI_                        { IRI.cons (IRIc.IRI $1) }
owl_IRI_: 
  | owl_fullIRI   { $1 }
  | Identifier    { $1 }                      
 
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
owl_axioms: /* empty */       { }  
  | owl_axioms owl_Axiom      { $1; $2 }

/* 4 Datatype Maps */

/* 4.1 Real Numbers, Decimal Numbers, and Integers */
owl_DTRealDecimalIntegers:
  | Owl_real                 { Dc.Owl_real }
  | Owl_rational             { Dc.Owl_rational }  
  | Xsd_decimal              { Dc.Xsd_decimal }
  | Xsd_integer              { Dc.Xsd_integer }
  | Xsd_nonNegativeInteger   { Dc.Xsd_nonNegativeInteger }
  | Xsd_nonPositiveInteger   { Dc.Xsd_nonPositiveInteger }
  | Xsd_positiveInteger      { Dc.Xsd_positiveInteger }
  | Xsd_negativeInteger      { Dc.Xsd_negativeInteger }
  | Xsd_long                 { Dc.Xsd_long }
  | Xsd_int                  { Dc.Xsd_int }
  | Xsd_short                { Dc.Xsd_short }
  | Xsd_byte                 { Dc.Xsd_byte }
  | Xsd_unsignedLong         { Dc.Xsd_unsignedLong }
  | Xsd_unsignedInt          { Dc.Xsd_unsignedInt }
  | Xsd_unsignedShort        { Dc.Xsd_unsignedShort }
  | Xsd_unsignedByte         { Dc.Xsd_unsignedByte }

/* 4.2 Floating-Point Numbers */
owl_DTFloats:
  | Xsd_double               { Dc.Xsd_double }
  | Xsd_float                { Dc.Xsd_float }

/* 4.3 Strings */
owl_DTStrings:
  | Rdf_PlainLiteral         { Dc.Rdf_PlainLiteral }
  | Xsd_string               { Dc.Xsd_string }
  | Xsd_normalizedString     { Dc.Xsd_normalizedString }
  | Xsd_token                { Dc.Xsd_token }
  | Xsd_language             { Dc.Xsd_language }
  | Xsd_Name                 { Dc.Xsd_Name }
  | Xsd_NCName               { Dc.Xsd_NCName }
  | Xsd_NMTOKEN              { Dc.Xsd_NMTOKEN }

/* 4.4 Boolean Values */
owl_DTBooleans:
  | Xsd_boolean              { Dc.Xsd_boolean }

/* 4.5 Binary Data */
owl_DTBinaries:
  | Xsd_hexBinary            { Dc.Xsd_hexBinary }
  | Xsd_base64Binary         { Dc.Xsd_base64Binary }

/* 4.6 IRIs */
owl_DTIRIs:
  | Xsd_anyURI               { Dc.Xsd_anyURI }

/* 4.7 Time Instants */
owl_DTTimes:
  | Xsd_dateTimeStamp        { Dc.Xsd_dateTimeStamp }

/* 4.8 XML Literals */
owl_DTXMLs:
  | Rdf_XMLLiteral           { Dc.Rdf_XMLLiteral }

/* 5 Entities and Literals */

/* 5.1 Classes */
owl_Class: owl_Class_        { $1 }
owl_Class_:  
  | owl_IRI                  { Cc.IRI $1 }  
  | Owl_Thing                { Cc.Thing }
  | Owl_Nothing              { Cc.Nothing }

/* 5.2 Datatypes */
owl_Datatype: owl_Datatype_     { $1 }
owl_Datatype_:
  | owl_IRI                     { Dc.IRI $1 }  
  | Rdfs_Literal                { Dc.Rdfs_Literal }
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
  | owl_IRI                  { OPc.IRI $1 }
  | Owl_topObjectProperty    { OPc.TopObjectProperty }
  | Owl_bottomObjectProperty { OPc.BottomObjectProperty }

/* 5.4 Data Properties */
owl_DataProperty: owl_DataProperty_ { $1 }
owl_DataProperty_:
  | owl_IRI                  { DPc.IRI $1 }
  | Owl_topDataProperty      { DPc.TopDataProperty }
  | Owl_bottomDataProperty   { DPc.BottomDataProperty }

/* 5.5 Annotation Properties */
owl_AnnotationProperty: owl_AnnotationProperty_ { $1 }
owl_AnnotationProperty_:
  | owl_IRI                     { APc.IRI $1 }
  | Rdfs_label                  { APc.Rdfs_label }
  | Rdfs_comment                { APc.Rdfs_comment }
  | Rdfs_seeAlso                { APc.Rdfs_seeAlso }
  | Rdfs_isDefinedBy            { APc.Rdfs_isDefinedBy }
  | Owl_deprecated              { APc.Owl_deprecated }
	| Owl_versionInfo             { APc.Owl_versionInfo }
  | Owl_priorVersion            { APc.Owl_priorVersion }
  | Owl_backwardCompatibleWith  { APc.Owl_backwardCompatibleWith }
  | Owl_incompatibleWith        { APc.Owl_incompatibleWith }

/* 5.6 Individuals */
owl_Individuals: /* empty */       { [] }
  | owl_Individuals owl_Individual { $2 :: $1 }
owl_Individual: owl_Individual_   { $1 } 
owl_Individual_:
  | owl_NamedIndividual           { $1 } 
  | owl_AnonymousIndividual       { $1 }

/* 5.6.1 Named Individuals */
owl_NamedIndividual: owl_IRI         { Ic.NamedIndividual $1 }

/* 5.6.2 Anonymous Individuals */
owl_AnonymousIndividual: owl_nodeID  { Ic.AnonymousIndividual $1 }

/* 5.7 Literals */
owl_Literals: /* empty */            { [] }  
  | owl_Literals owl_Literal         { $2 :: $1 }
owl_Literal: owl_Literal_            { Literal.cons $1 }
owl_Literal_:
  | owl_typedLiteral                 { $1 }
  | owl_stringLiteralNoLanguage      { $1 }
  | owl_stringLiteralWithLanguage    { $1 }
owl_typedLiteral:
  | owl_lexicalForm DoubleSuperscript owl_Datatype { Lc.TypedLiteral ($1, $3) }
owl_lexicalForm: owl_quotedString    { $1 }
owl_stringLiteralNoLanguage: 
  | owl_quotedString                 { Lc.StringLiteralNoLanguage $1 }
owl_stringLiteralWithLanguage: 
  | owl_quotedString owl_languageTag { Lc.StringLiteralWithLanguage ($1, $2) }

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
  | owl_ObjectProperty                 { OPEc.ObjectProperty $1 }  
  | owl_InverseObjectProperty          { $1 }

/* 6.1.1 Inverse Object Properties */
owl_InverseObjectProperty:
  | ObjectInverseOf LeftParen owl_ObjectProperty RightParen { OPEc.ObjectInverseOf $3 }

/* 6.2 Data Property Expressions */
owl_DataPropertyExpressions: /* empty */                   { [] }  
  | owl_DataPropertyExpressions owl_DataPropertyExpression { $2 :: $1 }
owl_DataPropertyExpression: owl_DataPropertyExpression_    { DataPropertyExpression.cons $1 }
owl_DataPropertyExpression_:
  | owl_DataProperty                                       { DPEc.DataProperty $1 }  

/* 7 Data Ranges */
owl_DataRanges:  /* empty */          { [] }
  | owl_DataRanges owl_DataRange      { $2 :: $1 }
owl_DataRange: owl_DataRange_         { DataRange.cons $1 }
owl_DataRange_:
  | owl_Datatype                      { DRc.Datatype $1 }
  | owl_DataIntersectionOf            { $1 }
  | owl_DataUnionOf                   { $1 }
  | owl_DataComplementOf              { $1 } 
  | owl_DataOneOf                     { $1 }
  | owl_DatatypeRestriction           { $1 }

/* 7.1 Intersection of Data Ranges */
owl_DataIntersectionOf:
  | DataIntersectionOf LeftParen owl_DataRange owl_DataRange owl_DataRanges RightParen { DRc.DataIntersectionOf ($3 :: $4 :: $5) }

/* 7.2 Union of Data Ranges */
owl_DataUnionOf:
  | DataUnionOf LeftParen owl_DataRange owl_DataRange owl_DataRanges RightParen { DRc.DataUnionOf ($3 :: $4 :: $5) }

/* 7.3 Complement of Data Ranges */
owl_DataComplementOf:
  | DataComplementOf LeftParen owl_DataRange RightParen { DRc.DataComplementOf $3 }

/* 7.4 Enumeration of Literals */
owl_DataOneOf:
  | DataOneOf LeftParen owl_Literal owl_Literals RightParen { DRc.DataOneOf ($3 :: $4) }

/* 7.5 Datatype Restrictions */
owl_DatatypeRestriction: 
  | DatatypeRestriction LeftParen owl_Datatype owl_constrainingFacet owl_restrictionValue 
    owl_constrainingFacet_restrictionValues RightParen 
    { DRc.DatatypeRestriction ($3, ($4, $5) :: $6 ) }
owl_constrainingFacet_restrictionValues:  /* empty */                                  { [] }  
  | owl_constrainingFacet_restrictionValues owl_constrainingFacet owl_restrictionValue { ($2, $3) :: $1 }
owl_constrainingFacet: owl_constrainingFacet_ { $1 }
owl_constrainingFacet_:
  | owl_IRI          { CFc.IRI $1 }
  | Xsd_minInclusive { CFc.Xsd_minInclusive }
  | Xsd_maxInclusive { CFc.Xsd_maxInclusive }
  | Xsd_minExclusive { CFc.Xsd_minExclusive }
  | Xsd_maxExclusive { CFc.Xsd_maxExclusive }
  | Xsd_length       { CFc.Xsd_length }
  | Xsd_minLength    { CFc.Xsd_minLength }
  | Xsd_maxLength    { CFc.Xsd_maxLength }
  | Xsd_pattern      { CFc.Xsd_pattern }
  | Rdf_langRange    { CFc.Rdf_langRange }
owl_restrictionValue: owl_Literal { $1 }

/* 8 Class Expressions */
owl_ClassExpressions:  /* empty */            { [] }
  | owl_ClassExpressions owl_ClassExpression  { $2 :: $1 }
owl_ClassExpression: owl_ClassExpression_ { ClassExpression.cons $1 }
owl_ClassExpression_:  
  | owl_Class_                   { CEc.Class $1 }  
  | owl_ObjectIntersectionOf_    {$1}
  | owl_ObjectUnionOf_           {$1}
  | owl_ObjectComplementOf_      {$1}
  | owl_ObjectOneOf_             {$1}
  | owl_ObjectSomeValuesFrom_    {$1}
  | owl_ObjectAllValuesFrom_     {$1}
  | owl_ObjectHasValue_          {$1}
  | owl_ObjectHasSelf_           {$1}
  | owl_ObjectMinCardinality_    {$1}
  | owl_ObjectMaxCardinality_    {$1}
  | owl_ObjectExactCardinality_  {$1}
  | owl_DataSomeValuesFrom_      {$1}
  | owl_DataAllValuesFrom_       {$1}
  | owl_DataHasValue_            {$1}
  | owl_DataMinCardinality_      {$1}
  | owl_DataMaxCardinality_      {$1}
  | owl_DataExactCardinality_    {$1}

/* 8.1 Propositional Connectives and Enumeration of Individuals */

/* 8.1.1 Intersection of Class Expressions */
owl_ObjectIntersectionOf_: 
  | ObjectIntersectionOf LeftParen owl_ClassExpression owl_ObjectIntersectionOfrec RightParen 
    { CEc.ObjectIntersectionOf ($3, $4) }
owl_ObjectIntersectionOfrec:
  | owl_ClassExpression  { $1 }
  | owl_ObjectIntersectionOfrec owl_ClassExpression  { ClassExpression.cons (CEc.ObjectIntersectionOfrec ($1, $2)) }

/* 8.1.2 Union of Class Expressions */
owl_ObjectUnionOf_: 
  | ObjectUnionOf LeftParen owl_ClassExpression owl_ObjectUnionOfrec RightParen 
    { CEc.ObjectUnionOf ($3, $4) }
owl_ObjectUnionOfrec:
  | owl_ClassExpression  { $1 }
  | owl_ObjectUnionOfrec owl_ClassExpression  { ClassExpression.cons (CEc.ObjectUnionOfrec ($1, $2)) }
 
/* 8.1.3 Complement of Class Expressions */
owl_ObjectComplementOf_:
  | pol_ObjectComplementOf LeftParen owl_ClassExpression RightParen 
   { pol := Polarity.invert !pol; CEc.ObjectComplementOf ($3) }
pol_ObjectComplementOf: ObjectComplementOf {pol := Polarity.invert !pol}

/* 8.1.4 Enumeration of Individuals */
owl_ObjectOneOf_: 
  | ObjectOneOf LeftParen owl_Individual owl_Individuals RightParen 
    { CEc.ObjectOneOf ($3 :: $4) }

/* 8.2 Object Property Restrictions */

/* 8.2.1 Existential Quantification */
owl_ObjectSomeValuesFrom_:
  | ObjectSomeValuesFrom LeftParen owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { CEc.ObjectSomeValuesFrom ($3, $4) }
 
/* 8.2.2 Universal Quantification */
owl_ObjectAllValuesFrom_:
  | ObjectAllValuesFrom LeftParen owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { CEc.ObjectAllValuesFrom ($3, $4) }

/* 8.2.3 Individual Value Restriction */
owl_ObjectHasValue_:
  | ObjectHasValue LeftParen owl_ObjectPropertyExpression owl_Individual RightParen 
    { CEc.ObjectHasValue ($3, $4) }

/* 8.2.4 Self-Restriction */
owl_ObjectHasSelf_: 
  | ObjectHasSelf LeftParen owl_ObjectPropertyExpression RightParen  
    { CEc.ObjectHasSelf $3 }

/* 8.3 Object Property Cardinality Restrictions */

/* 8.3.1 Minimum Cardinality */
owl_ObjectMinCardinality_:
  | ObjectMinCardinality LeftParen owl_nonNegativeInteger owl_ObjectPropertyExpression RightParen  
    { CEc.ObjectMinCardinality ($3, $4, None) }
  | ObjectMinCardinality LeftParen owl_nonNegativeInteger owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { CEc.ObjectMinCardinality ($3, $4, Some $5) }

/* 8.3.2 Maximum Cardinality */
owl_ObjectMaxCardinality_:
  | pol_ObjectMaxCardinality LeftParen owl_nonNegativeInteger owl_ObjectPropertyExpression RightParen 
    { pol := Polarity.invert !pol; CEc.ObjectMaxCardinality ($3, $4, None) }
  | pol_ObjectMaxCardinality LeftParen owl_nonNegativeInteger owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { pol := Polarity.invert !pol; CEc.ObjectMaxCardinality ($3, $4, Some $5) }
pol_ObjectMaxCardinality: ObjectMaxCardinality { pol := Polarity.invert !pol }

/* 8.3.3 Exact Cardinality */
owl_ObjectExactCardinality_:
  | pol_ObjectExactCardinality LeftParen owl_nonNegativeInteger owl_ObjectPropertyExpression RightParen 
    { decr pol_nesting; if !pol_nesting = 0 then pol := !pol_save; CEc.ObjectExactCardinality ($3, $4, None) }
  | pol_ObjectExactCardinality LeftParen owl_nonNegativeInteger owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { decr pol_nesting; if !pol_nesting = 0 then pol := !pol_save; CEc.ObjectExactCardinality ($3, $4, Some $5) }
pol_ObjectExactCardinality: ObjectExactCardinality { if !pol_nesting = 0 then pol_save := !pol; incr pol_nesting }

/* 8.4 Data Property Restrictions */

/* 8.4.1 Existential Quantification */
owl_DataSomeValuesFrom_:
  | DataSomeValuesFrom LeftParen owl_DataPropertyExpression owl_DataPropertyExpressions owl_DataRange RightParen 
    { CEc.DataSomeValuesFrom ($3 :: $4,  $5) }

/* 8.4.2 Universal Quantification */
owl_DataAllValuesFrom_:
  | DataAllValuesFrom LeftParen owl_DataPropertyExpression owl_DataPropertyExpressions owl_DataRange RightParen 
    { CEc.DataAllValuesFrom ($3 :: $4,  $5) }

/* 8.4.3 Literal Value Restriction */
owl_DataHasValue_:
  | DataHasValue LeftParen owl_DataPropertyExpression owl_Literal RightParen 
    { CEc.DataHasValue ($3, $4) }

/* 8.5 Data Property Cardinality Restrictions */

/* 8.5.1 Minimum Cardinality */
owl_DataMinCardinality_:
  | DataMinCardinality LeftParen owl_nonNegativeInteger owl_DataPropertyExpression RightParen 
    { CEc.DataMinCardinality ($3, $4, None) }
  | DataMinCardinality LeftParen owl_nonNegativeInteger owl_DataPropertyExpression owl_DataRange RightParen 
    { CEc.DataMinCardinality ($3, $4, Some $5) }

/* 8.5.2 Maximum Cardinality */
owl_DataMaxCardinality_:
  | pol_DataMaxCardinality LeftParen owl_nonNegativeInteger owl_DataPropertyExpression RightParen 
    { pol := Polarity.invert !pol; CEc.DataMaxCardinality ($3, $4, None) }
  | pol_DataMaxCardinality LeftParen owl_nonNegativeInteger owl_DataPropertyExpression owl_DataRange RightParen 
    { pol := Polarity.invert !pol; CEc.DataMaxCardinality ($3, $4, Some $5) } 
pol_DataMaxCardinality: DataMaxCardinality { pol := Polarity.invert !pol }

/* 8.5.3 Exact Cardinality */
owl_DataExactCardinality_:
  | pol_DataExactCardinality LeftParen owl_nonNegativeInteger owl_DataPropertyExpression RightParen 
    { decr pol_nesting; if !pol_nesting = 0 then pol := !pol_save; CEc.DataExactCardinality ($3, $4, None) }
  | pol_DataExactCardinality LeftParen owl_nonNegativeInteger owl_DataPropertyExpression owl_DataRange RightParen 
    { decr pol_nesting; if !pol_nesting = 0 then pol := !pol_save; CEc.DataExactCardinality ($3, $4, Some $5) }
pol_DataExactCardinality: DataExactCardinality { if !pol_nesting = 0 then pol_save := !pol; incr pol_nesting }

/* 9 Axioms */
owl_Axiom:
  | owl_Declaration         { }
  | owl_ClassAxiom          { O.add_ClassAxiom (o()) $1 }
  | owl_ObjectPropertyAxiom { O.add_ObjectPropertyAxiom (o()) $1 }
  | owl_DataPropertyAxiom   { let _ = $1 in () }
  | owl_DatatypeDefinition  { let _ = $1 in () }
  | owl_HasKey              { let _ = $1 in () }
  | owl_Assertion           { O.add_Assertion (o()) $1 }
  | owl_AnnotationAxiom     { }
owl_axiomAnnotations: /* empty */    {}
  | owl_axiomAnnotations owl_Annotation {}

/* 9.1 Class Expression Axioms */
owl_ClassAxiom: owl_ClassAxiom_ { ClassAxiom.cons $1 }
owl_ClassAxiom_:
  | owl_SubClassOf        { $1 }
  | owl_EquivalentClasses { $1 }
  | owl_DisjointClasses   { $1 }
  | owl_DisjointUnion     { $1 }

/* 9.1.1 Subclass Axioms */
owl_SubClassOf:
  | pol_SubClassOf LeftParen owl_axiomAnnotations owl_subClassExpression owl_superClassExpression RightParen 
    { pol := Polarity.Both; CAc.SubClassOf ($4, $5) }
pol_SubClassOf: SubClassOf { pol := Polarity.Negative }    
owl_subClassExpression: owl_ClassExpression   { pol := Polarity.Positive; $1 }
owl_superClassExpression: owl_ClassExpression { $1 }

/* 9.1.2 Equivalent Classes */
owl_EquivalentClasses:
  | EquivalentClasses LeftParen owl_axiomAnnotations owl_ClassExpression owl_ClassExpression owl_ClassExpressions RightParen 
    { CAc.EquivalentClasses ($4 :: $5 :: $6) }
    
/* 9.1.3 Disjoint Classes */
owl_DisjointClasses:
  | pol_DisjointClasses LeftParen owl_axiomAnnotations owl_ClassExpression owl_ClassExpression owl_ClassExpressions RightParen 
    { pol := Polarity.Both; CAc.DisjointClasses ($4 :: $5 :: $6) }
pol_DisjointClasses: DisjointClasses { pol := Polarity.Negative }
    
            
/* 9.1.4 Disjoint Union of Class Expressions */
owl_DisjointUnion:
  | DisjointUnion LeftParen owl_axiomAnnotations owl_Class owl_disjointClassExpressions RightParen 
    { CAc.DisjointUnion ($4, $5) }
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
    { OPAc.SubObjectPropertyOf ($4, $5) }
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
    { OPAc.EquivalentObjectProperties ($4 :: $5 :: $6) }

/* 9.2.3 Disjoint Object Properties */
owl_DisjointObjectProperties:
  | DisjointObjectProperties LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_ObjectPropertyExpression
    owl_ObjectPropertyExpressions RightParen 
    { OPAc.DisjointObjectProperties ($4 :: $5 :: $6) }

/* 9.2.4 Inverse Object Properties */
owl_InverseObjectProperties:
  | InverseObjectProperties LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_ObjectPropertyExpression RightParen 
    { OPAc.InverseObjectProperties ($4, $5) }

/* 9.2.5 Object Property Domain */
owl_ObjectPropertyDomain: 
  | pol_ObjectPropertyDomain LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { pol := Polarity.Both; OPAc.ObjectPropertyDomain ($4, $5) }
pol_ObjectPropertyDomain: ObjectPropertyDomain { pol := Polarity.Positive }

/* 9.2.6 Object Property Range */
owl_ObjectPropertyRange: 
  | pol_ObjectPropertyRange LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_ClassExpression RightParen 
    { pol := Polarity.Both; OPAc.ObjectPropertyRange ($4, $5) }
pol_ObjectPropertyRange: ObjectPropertyRange { pol := Polarity.Positive }

/* 9.2.7 Functional Object Properties */
owl_FunctionalObjectProperty: 
  | FunctionalObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPAc.FunctionalObjectProperty ($4) }

/* 9.2.8 Inverse-Functional Object Properties */
owl_InverseFunctionalObjectProperty: 
  | InverseFunctionalObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPAc.InverseFunctionalObjectProperty ($4) }

/* 9.2.9 Reflexive Object Properties */
owl_ReflexiveObjectProperty: 
  | ReflexiveObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPAc.ReflexiveObjectProperty ($4) }

/* 9.2.10 Irreflexive Object Properties */
owl_IrreflexiveObjectProperty: 
  | IrreflexiveObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPAc.IrreflexiveObjectProperty ($4) }
    
/* 9.2.11 Symmetric Object Properties */    
owl_SymmetricObjectProperty: 
  | SymmetricObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPAc.SymmetricObjectProperty ($4) }

/* 9.2.12 Asymmetric Object Properties */
owl_AsymmetricObjectProperty: 
  | AsymmetricObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPAc.AsymmetricObjectProperty ($4) }

/* 9.2.13 Transitive Object Properties */
owl_TransitiveObjectProperty: 
  | TransitiveObjectProperty LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression RightParen 
    { OPAc.TransitiveObjectProperty ($4) }

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
    { DPAc.SubDataPropertyOf ($4, $5) }
owl_subDataPropertyExpression: owl_DataPropertyExpression   { $1 } 
owl_superDataPropertyExpression: owl_DataPropertyExpression { $1 }

/* 9.3.2 Equivalent Data Properties */
owl_EquivalentDataProperties:
  | EquivalentDataProperties LeftParen owl_axiomAnnotations owl_DataPropertyExpression owl_DataPropertyExpression 
    owl_DataPropertyExpressions RightParen 
    { DPAc.EquivalentDataProperties ($4 :: $5 :: $6) }

/* 9.3.3 Disjoint Data Properties */
owl_DisjointDataProperties:
  | DisjointDataProperties LeftParen owl_axiomAnnotations owl_DataPropertyExpression owl_DataPropertyExpression 
    owl_DataPropertyExpressions RightParen 
    { DPAc.EquivalentDataProperties ($4 :: $5 :: $6) }

/* 9.3.4 Data Property Domain */
owl_DataPropertyDomain: 
  | pol_DataPropertyDomain LeftParen owl_axiomAnnotations owl_DataPropertyExpression owl_ClassExpression RightParen 
    { pol := Polarity.Both; DPAc.DataPropertyDomain ($4, $5) }
pol_DataPropertyDomain: DataPropertyDomain { pol := Polarity.Positive }

/* 9.3.5 Data Property Range */
owl_DataPropertyRange: 
  | DataPropertyRange LeftParen owl_axiomAnnotations owl_DataPropertyExpression owl_DataRange RightParen
    { DPAc.DataPropertyRange ($4, $5) }

/* 9.3.6 Functional Data Properties */
owl_FunctionalDataProperty: 
  | FunctionalDataProperty LeftParen owl_axiomAnnotations owl_DataPropertyExpression RightParen
    { DPAc.FunctionalDataProperty ($4) }

/* 9.4 Datatype Definitions */
owl_DatatypeDefinition: owl_DatatypeDefinition_ { DatatypeDefinition.cons $1 }
owl_DatatypeDefinition_: 
  | DatatypeDefinition LeftParen owl_axiomAnnotations owl_Datatype owl_DataRange RightParen 
    { DDc.DatatypeDefinition ($4, $5) }

/* 9.5 Keys */
owl_HasKey: owl_HasKey_ { Key.cons $1 }
owl_HasKey_:
  | pol_HasKey LeftParen owl_axiomAnnotations owl_ClassExpression 
    LeftParen owl_ObjectPropertyExpressions RightParen LeftParen owl_DataPropertyExpressions RightParen 
    { pol := Polarity.Both; Kc.HasKey ($4, $6, $9) }
pol_HasKey: HasKey { pol := Polarity.Negative }

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
    { Ac.SameIndividual ($4 :: $5 :: $6) }

/* 9.6.2 Individual Inequality */
owl_DifferentIndividuals: 
  | DifferentIndividuals LeftParen owl_axiomAnnotations owl_Individual owl_Individual owl_Individuals RightParen 
    { Ac.DifferentIndividuals ($4 :: $5 :: $6) }

/* 9.6.3 Class Assertions */
owl_ClassAssertion: 
  | pol_ClassAssertion LeftParen owl_axiomAnnotations owl_ClassExpression owl_Individual RightParen 
    { Ac.ClassAssertion ($4, $5) }
pol_ClassAssertion: ClassAssertion { pol := Polarity.Positive }

/* 9.6.4 Positive Object Property Assertions */
owl_ObjectPropertyAssertion: 
  | ObjectPropertyAssertion LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_sourceIndividual 
    owl_targetIndividual RightParen 
    { Ac.ObjectPropertyAssertion ($4, $5, $6) }

/* 9.6.5 Negative Object Property Assertions */
owl_NegativeObjectPropertyAssertion: 
  | NegativeObjectPropertyAssertion LeftParen owl_axiomAnnotations owl_ObjectPropertyExpression owl_sourceIndividual 
    owl_targetIndividual RightParen 
    { Ac.NegativeObjectPropertyAssertion ($4, $5, $6) }

/* 9.6.6 Positive Data Property Assertions */
owl_DataPropertyAssertion: 
  | DataPropertyAssertion LeftParen owl_axiomAnnotations owl_DataPropertyExpression owl_sourceIndividual owl_targetValue RightParen 
    { Ac.DataPropertyAssertion ($4, $5, $6) }

/* 9.6.7 Negative Data Property Assertions */
owl_NegativeDataPropertyAssertion: 
  | NegativeDataPropertyAssertion LeftParen owl_axiomAnnotations owl_DataPropertyExpression owl_sourceIndividual 
    owl_targetValue RightParen 
    { Ac.NegativeDataPropertyAssertion ($4, $5, $6) }

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
