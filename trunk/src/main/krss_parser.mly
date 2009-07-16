%{

open OwlSyntax ;;
(* abbreviations for commonly used modules *)

module O = Ontology

module C = Class.Constructor
module OP = ObjectProperty.Constructor 
module CE = ClassExpression.Constructor
module OPE = ObjectPropertyExpression.Constructor

module CEA = ClassExpressionAxiom.Constructor
module OPA = ObjectPropertyAxiom.Constructor
module A = Assertion.Constructor

(* we accumulate the resulted ontology here *)
let ont = O.create ()

(*let parse_error s =            *)
(*  Printf.fprintf stderr "%s" s;*)
(*  raise Parsing.Parse_error    *)
(*;;                             *)

%}

/* parentheses */

%token LeftParen RightParen

/* axioms keywords */
%token Implies Equivalent EquivalentRole ImpliesRole Inverse Functional Transitive Composition

/* constructors */
%token And Or Some All Not Inv Top Bottom

/* identifiers */
%token <string> Identifier

/* comments */
%token <string> Comment

/* eof */
%token EOF

%start ontology
%type <Ontology.t> ontology
%%

ontology : axioms EOF {$1; ont}

axioms : /* empty */ {}  
 | axioms axiom  { $1; O.insert_axiom ont $2 } 
;;

axiom :
 | concept_axiom { ClassExpressionAxiom $1 }
 | role_axiom    { ObjectPropertyAxiom $1 }
;;

concept_axiom :
 | LeftParen Implies concept concept RightParen     { CEA.SubClassOf ($3, $4) }    
 | LeftParen Implies concept RightParen             { CEA.SubClassOf ($3, $3) }    
 | LeftParen Equivalent concept concept RightParen  { CEA.EquivalentClasses ($3, $4) }
;; 

role_axiom :
 | LeftParen EquivalentRole role role RightParen    { OPA.EquivalentProperties ($3, $4) }
 | LeftParen ImpliesRole role role RightParen       { OPA.SubPropertyOf ([$3], $4) }
 | LeftParen ImpliesRole role RightParen            { OPA.SubPropertyOf ([$3], $3) }
 | LeftParen Inverse role role RightParen           { OPA.InverseProperties ($3, $4) }
 | LeftParen Functional role RightParen             { OPA.FunctionalProperty $3 }
 | LeftParen Transitive role RightParen             { OPA.TransitiveProperty $3 }
 | LeftParen Composition role role role RightParen  { OPA.RoleComposition ($3, $4, $5)}
;;

concept :
 | Identifier                                 { O.insert_class_expression ont ( CE.Class (O.insert_class ont (C.IRI $1) ) ) }
 | LeftParen Not concept RightParen           { O.insert_class_expression ont ( CE.ComplementOf ($3) ) }
 | LeftParen And concept concept concepts RightParen { O.insert_class_expression ont ( CE.IntersectionOf (List.fast_sort ClassExpression.compare ($3 :: $4 :: $5))) }
 | LeftParen Or concept concept concepts RightParen { O.insert_class_expression ont ( CE.UnionOf (List.fast_sort ClassExpression.compare ($3 :: $4 :: $5))) }
 | LeftParen Some role concept RightParen     { O.insert_class_expression ont ( CE.SomeValuesFrom ($3, $4) ) }
 | LeftParen All role concept RightParen      { O.insert_class_expression ont ( CE.AllValuesFrom ($3, $4) ) }
 | Top                                        { O.insert_class_expression ont ( CE.Class (O.insert_class ont C.Thing)) }  
 | Bottom                                     { O.insert_class_expression ont ( CE.Class (O.insert_class ont C.Nothing)) } 
;;

concepts : /* empty */                   { [] }
 | concepts concept                       { $2 :: $1 }
;;

role :
 | Identifier                                     
   { O.insert_object_property_expression ont ( OPE.ObjectProperty (O.insert_object_property ont (OP.IRI $1)) ) }
 | LeftParen Inv Identifier RightParen            
   { O.insert_object_property_expression ont ( OPE.InverseObjectProperty (O.insert_object_property ont (OP.IRI $3)) ) }
;;
