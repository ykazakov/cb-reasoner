open Owl2
open Consed.T
open Printf

module PC = Polarity.Counter

(**======================= ontology ========================**)

module OrderedClass =
struct
  type t = Class.t
  let compare c1 c2 = String.compare (Class.str_of c1) (Class.str_of c2)
end

module ClassMap = Map.Make (OrderedClass)
module ClassSet = Set.Make (OrderedClass)

type t = {
  cons_IRI : IRI.Cons.t;
  cons_NodeID : NodeID.Cons.t;
	cons_ObjectProperty : ObjectProperty.Cons.t;
  cons_Literal : Literal.Cons.t;
  cons_ObjectPropertyExpression : ObjectPropertyExpression.Cons.t;
  cons_DataPropertyExpression : DataPropertyExpression.Cons.t;
  cons_DataRange : DataRange.Cons.t;
  cons_ClassExpression : ClassExpression.Cons.t;
  cons_ClassExpressionAxiom : ClassExpressionAxiom.Cons.t;
  cons_ObjectPropertyAxiom : ObjectPropertyAxiom.Cons.t;
  cons_DataPropertyAxiom : DataPropertyAxiom.Cons.t;
  cons_DatatypeDefinition : DatatypeDefinition.Cons.t;
  cons_Key : Key.Cons.t;
  cons_Assertion : Assertion.Cons.t;
  cons_AnnotationSubject : AnnotationSubject.Cons.t;
  cons_AnnotationValue : AnnotationValue.Cons.t;
  cons_Annotation : Annotation.Cons.t;
  cons_AnnotationAxiom : AnnotationAxiom.Cons.t;
  
  (* ========== signature ========== *)
  mutable record_ObjectProperty : int ObjectProperty.Map.t;
  mutable count_ObjectPropertyIRI : int;
  mutable count_TopObjectProperty : PC.t;
  mutable count_BottomObjectProperty: PC.t;
  
  mutable record_Class : int ClassMap.t;
  mutable count_ClassIRI : int;
  mutable count_Thing : PC.t;
  mutable count_Nothing: PC.t;
  
  mutable record_Individual : int Individual.Map.t;
  mutable count_IndividualIRI : int;
  
  (* ========== structure ========== *)
  mutable record_ComplexObjectPropertyExpression : PC.t ObjectPropertyExpression.Map.t;
  mutable count_InverseObjectProperty : PC.t;
  
  mutable record_ComplexClassExpression : PC.t ClassExpression.Map.t;
  mutable count_ObjectIntersectionOf : PC.t;
  mutable count_ObjectUnionOf : PC.t;
  mutable count_ObjectComplementOf : PC.t;
  mutable count_ObjectOneOf : PC.t;
  mutable count_ObjectSomeValuesFrom : PC.t;
  mutable count_ObjectAllValuesFrom : PC.t;
  mutable count_ObjectHasValue : PC.t;
  mutable count_ObjectHasSelf : PC.t;
  mutable count_ObjectMinCardinality : PC.t;
  mutable count_ObjectMaxCardinality : PC.t;
  mutable count_ObjectExactCardinality : PC.t;
  mutable count_DataSomeValuesFrom : PC.t;
  mutable count_DataAllValuesFrom : PC.t;
  mutable count_DataHasValue : PC.t;
  mutable count_DataMinCardinality : PC.t;
  mutable count_DataMaxCardinality : PC.t;
  mutable count_DataExactCardinality : PC.t;
  
  (* ========== axioms ========== *)
  mutable record_ObjectPropertyAxiom : ObjectPropertyAxiom.Set.t;
  mutable count_SubPropertyOf : int;
  mutable count_EquivalentProperties : int;
  mutable count_InverseProperties : int;
  mutable count_FunctionalProperty : int;
  mutable count_InverseFunctionalProperty : int;
  mutable count_TransitiveProperty : int;
  mutable count_RoleComposition : int;
  
  mutable record_ClassExpressionAxiom : ClassExpressionAxiom.Set.t;
  mutable count_SubClassOf : int;
  mutable count_EquivalentClasses : int;
  
  mutable record_Assertion : Assertion.Set.t;
  mutable count_ClassAssertion : int;
  mutable count_PropertyAssertion : int;
}

(**=================== initialization ======================**)

let create () =	{

  cons_IRI = IRI.Cons.create 127;
	cons_NodeID = NodeID.Cons.create 127;
  cons_ObjectProperty = ObjectProperty.Cons.create 127;
  cons_Literal = Literal.Cons.create 127;
  cons_ObjectPropertyExpression = ObjectPropertyExpression.Cons.create 127;
  cons_DataPropertyExpression = DataPropertyExpression.Cons.create 127;
  cons_DataRange = DataRange.Cons.create 127;
  cons_ClassExpression = ClassExpression.Cons.create 127;
  cons_ClassExpressionAxiom = ClassExpressionAxiom.Cons.create 127;
  cons_ObjectPropertyAxiom = ObjectPropertyAxiom.Cons.create 127;
  cons_DataPropertyAxiom = DataPropertyAxiom.Cons.create 127;
  cons_DatatypeDefinition = DatatypeDefinition.Cons.create 127;
  cons_Key = Key.Cons.create 127;
  cons_Assertion = Assertion.Cons.create 127;
  cons_AnnotationSubject = AnnotationSubject.Cons.create 127;
  cons_AnnotationValue = AnnotationValue.Cons.create 127;
  cons_Annotation = Annotation.Cons.create 127;
  cons_AnnotationAxiom = AnnotationAxiom.Cons.create 127;
  
  (* ========== signature ========== *)
  record_ObjectProperty = ObjectProperty.Map.empty;
  count_ObjectPropertyIRI = 0;
  count_TopObjectProperty = PC.zero;
  count_BottomObjectProperty = PC.zero;
  
  record_Class = ClassMap.empty;
  count_ClassIRI = 0;
  count_Thing = PC.zero;
  count_Nothing = PC.zero;
  
  record_Individual = Individual.Map.empty;
  count_IndividualIRI = 0;
  
  (* ========== structure ========== *)
  record_ComplexObjectPropertyExpression = ObjectPropertyExpression.Map.empty;
  count_InverseObjectProperty = PC.zero;
  
  record_ComplexClassExpression = ClassExpression.Map.empty;
  count_ObjectIntersectionOf = PC.zero;
  count_ObjectUnionOf = PC.zero;
  count_ObjectComplementOf = PC.zero;
  count_ObjectOneOf = PC.zero;
  count_ObjectSomeValuesFrom = PC.zero;
  count_ObjectAllValuesFrom = PC.zero;
  count_ObjectHasValue = PC.zero;
  count_ObjectHasSelf = PC.zero;
  count_ObjectMinCardinality = PC.zero;
  count_ObjectMaxCardinality = PC.zero;
  count_ObjectExactCardinality = PC.zero;
  count_DataSomeValuesFrom = PC.zero;
  count_DataAllValuesFrom = PC.zero;
  count_DataHasValue = PC.zero;
  count_DataMinCardinality = PC.zero;
  count_DataMaxCardinality = PC.zero;
  count_DataExactCardinality = PC.zero;
  
  (* ========== axioms ========== *)
  record_ObjectPropertyAxiom = ObjectPropertyAxiom.Set.empty;
  count_SubPropertyOf = 0;
  count_EquivalentProperties = 0;
  count_InverseProperties = 0;
  count_FunctionalProperty = 0;
  count_InverseFunctionalProperty = 0;
  count_TransitiveProperty = 0;
  count_RoleComposition = 0;
  
  record_ClassExpressionAxiom = ClassExpressionAxiom.Set.empty;
  count_SubClassOf = 0;
  count_EquivalentClasses = 0;
  
  record_Assertion = Assertion.Set.empty;
  count_ClassAssertion = 0;
  count_PropertyAssertion = 0;
}

(**======================= consing =========================**)
let cons_IRI ont id = IRI.cons ont.cons_IRI id
let cons_NodeID ont id = NodeID.cons ont.cons_NodeID id
let cons_ObjectProperty ont op = ObjectProperty.cons ont.cons_ObjectProperty op
let cons_Literal ont lt = Literal.cons ont.cons_Literal lt
let cons_ObjectPropertyExpression ont ope = ObjectPropertyExpression.cons ont.cons_ObjectPropertyExpression ope
let cons_DataPropertyExpression ont dpe = DataPropertyExpression.cons ont.cons_DataPropertyExpression dpe
let cons_DataRange ont dr = DataRange.cons ont.cons_DataRange dr
let cons_ClassExpression ont ce = ClassExpression.cons ont.cons_ClassExpression ce
let cons_ClassExpressionAxiom ont ax = ClassExpressionAxiom.cons ont.cons_ClassExpressionAxiom ax
let cons_ObjectPropertyAxiom ont ax = ObjectPropertyAxiom.cons ont.cons_ObjectPropertyAxiom ax
let cons_DataPropertyAxiom ont ax = DataPropertyAxiom.cons ont.cons_DataPropertyAxiom ax
let cons_DatatypeDefinition ont ax = DatatypeDefinition.cons ont.cons_DatatypeDefinition ax
let cons_Key ont ax = Key.cons ont.cons_Key ax
let cons_Assertion ont ax = Assertion.cons ont.cons_Assertion ax
let cons_AnnotationSubject ont ax = AnnotationSubject.cons ont.cons_AnnotationSubject ax
let cons_AnnotationValue ont ax = AnnotationValue.cons ont.cons_AnnotationValue ax
let cons_Annotation ont ax = Annotation.cons ont.cons_Annotation ax
let cons_AnnotationAxiom ont ax = AnnotationAxiom.cons ont.cons_AnnotationAxiom ax

(**====================== iterators ========================**)

let iter_record_ObjectProperty f ont =
  ObjectProperty.Map.iter f ont.record_ObjectProperty
let iter_record_Class f ont =
  ClassMap.iter f ont.record_Class
let iter_record_Individual f ont =
  Individual.Map.iter f ont.record_Individual

let iter_record_ComplexObjectPropertyExpression f ont =
  ObjectPropertyExpression.Map.iter f ont.record_ComplexObjectPropertyExpression
let iter_record_ComplexClassExpression f ont =
  ClassExpression.Map.iter f ont.record_ComplexClassExpression

let iter_record_ObjectPropertyAxiom f ont =
  ObjectPropertyAxiom.Set.iter f ont.record_ObjectPropertyAxiom
let iter_record_ClassExpressionAxiom f ont =
  ClassExpressionAxiom.Set.iter f ont.record_ClassExpressionAxiom
let iter_record_Assertion f ont =
  Assertion.Set.iter f ont.record_Assertion

(**============== statistical information ================**)

let has_positive_Nothing ont =
  PC.get_pos ont.count_Nothing > 0
let has_positive_ComplementOf ont =
  PC.get_pos ont.count_ObjectComplementOf > 0
let has_negative_Thing ont =
  PC.get_neg ont.count_Thing > 0

let total_ObjectPropertyIRI ont = ont.count_ObjectPropertyIRI
let total_ClassIRI ont = ont.count_ClassIRI
let total_IndividualIRI ont = ont.count_IndividualIRI

let count_TopObjectProperty ont = ont.count_TopObjectProperty
let count_BottomObjectProperty ont = ont.count_BottomObjectProperty
let count_Thing ont = ont.count_Thing
let count_Nothing ont = ont.count_Nothing

let count_InverseObjectProperty ont = ont.count_InverseObjectProperty
let total_InverseObjectProperty ont = PC.get_total ont.count_InverseObjectProperty

let count_ObjectIntersectionOf ont = ont.count_ObjectIntersectionOf
let count_ObjectUnionOf ont = ont.count_ObjectUnionOf
let count_ObjectComplementOf ont = ont.count_ObjectComplementOf
let count_ObjectOneOf ont = ont.count_ObjectOneOf
let count_ObjectSomeValuesFrom ont = ont.count_ObjectSomeValuesFrom
let count_ObjectAllValuesFrom ont = ont.count_ObjectAllValuesFrom
let count_ObjectHasValue ont = ont.count_ObjectHasValue
let count_ObjectHasSelf ont = ont.count_ObjectHasSelf
let count_ObjectMinCardinality ont = ont.count_ObjectMinCardinality
let count_ObjectMaxCardinality ont = ont.count_ObjectMaxCardinality
let count_ObjectExactCardinality ont = ont.count_ObjectExactCardinality
let count_DataSomeValuesFrom ont = ont.count_DataSomeValuesFrom
let count_DataAllValuesFrom ont = ont.count_DataAllValuesFrom
let count_DataHasValue ont = ont.count_DataHasValue
let count_DataMinCardinality ont = ont.count_DataMinCardinality
let count_DataMaxCardinality ont = ont.count_DataMaxCardinality
let count_DataExactCardinality ont = ont.count_DataExactCardinality

let total_ObjectIntersectionOf ont = PC.get_total ont.count_ObjectIntersectionOf
let total_ObjectUnionOf ont = PC.get_total ont.count_ObjectUnionOf
let total_ObjectComplementOf ont = PC.get_total ont.count_ObjectComplementOf
let total_ObjectOneOf ont = PC.get_total ont.count_ObjectOneOf
let total_ObjectSomeValuesFrom ont = PC.get_total ont.count_ObjectSomeValuesFrom
let total_ObjectAllValuesFrom ont = PC.get_total ont.count_ObjectAllValuesFrom
let total_ObjectHasValue ont = PC.get_total ont.count_ObjectHasValue
let total_ObjectHasSelf ont = PC.get_total ont.count_ObjectHasSelf
let total_ObjectMinCardinality ont = PC.get_total ont.count_ObjectMinCardinality
let total_ObjectMaxCardinality ont = PC.get_total ont.count_ObjectMaxCardinality
let total_ObjectExactCardinality ont = PC.get_total ont.count_ObjectExactCardinality
let total_DataSomeValuesFrom ont = PC.get_total ont.count_DataSomeValuesFrom
let total_DataAllValuesFrom ont = PC.get_total ont.count_DataAllValuesFrom
let total_DataHasValue ont = PC.get_total ont.count_DataHasValue
let total_DataMinCardinality ont = PC.get_total ont.count_DataMinCardinality
let total_DataMaxCardinality ont = PC.get_total ont.count_DataMaxCardinality
let total_DataExactCardinality ont = PC.get_total ont.count_DataExactCardinality

let total_SubPropertyOf ont = ont.count_SubPropertyOf
let total_EquivalentProperties ont = ont.count_EquivalentProperties
let total_InverseProperties ont = ont.count_InverseProperties
let total_FunctionalProperty ont = ont.count_FunctionalProperty
let total_InverseFunctionalProperty ont = ont.count_InverseFunctionalProperty
let total_TransitiveProperty ont = ont.count_TransitiveProperty
let total_RoleComposition ont = ont.count_RoleComposition
let total_SubClassOf ont = ont.count_SubClassOf
let total_EquivalentClasses ont = ont.count_EquivalentClasses
let total_ClassAssertion ont = ont.count_ClassAssertion
let total_PropertyAssertion ont = ont.count_PropertyAssertion

(**==== insertion, computation of polarities and stats =====**)

let add_ObjectProperty_pc ont op pc =
  let module M = ObjectProperty.Map in
  let module C = ObjectProperty.Constructor in
  match op.data with
  | C.IRI _ -> (* adding only iris in the record *)
      ont.record_ObjectProperty <- M.add op
        (let count = try M.find op ont.record_ObjectProperty
            with Not_found ->
                ont.count_ObjectPropertyIRI <-
                succ ont.count_ObjectPropertyIRI;
                0
          in count + PC.get_total pc
        ) ont.record_ObjectProperty;
  | C.TopObjectProperty -> ont.count_TopObjectProperty <-
      PC.sum ont.count_TopObjectProperty pc
  | C.BottomObjectProperty -> ont.count_BottomObjectProperty <-
      PC.sum ont.count_BottomObjectProperty pc
;;

let add_Class_pc ont c pc =
  let module M = ClassMap in
  let module C = Class.Constructor in
  match c with
  | C.IRI iri -> (* adding only iris in the record *)
      ont.record_Class <- M.add c
        (let count = try M.find c ont.record_Class
            with Not_found ->
                ont.count_ClassIRI <-
                succ ont.count_ClassIRI;
                0
          in count + PC.get_total pc
        ) ont.record_Class
  | C.Thing -> ont.count_Thing <-
      PC.sum ont.count_Thing pc
  | C.Nothing -> ont.count_Nothing <-
      PC.sum ont.count_Nothing pc
;;

let add_Individual_pc ont i pc =
  let module M = Individual.Map in
  let module C = Individual.Constructor in
  (* adding only iris in the record *)
  ont.record_Individual <- M.add i
    (let count = try M.find i ont.record_Individual
        with Not_found ->
            ont.count_IndividualIRI <-
            succ ont.count_IndividualIRI;
            0
      in succ count
    ) ont.record_Individual
;;

let rec add_ObjectPropertyExpression_pc ont ope pc =
  let module M = ObjectPropertyExpression.Map in
  let module C = ObjectPropertyExpression.Constructor in
  begin match ope.data with
    | C.ObjectProperty _ -> ()
    | _ -> ont.record_ComplexObjectPropertyExpression <-
        M.process ope (function
            | Some pc_old -> Some (PC.sum pc_old pc)
            | None -> Some pc
          ) ont.record_ComplexObjectPropertyExpression;
  end;
  propagate_ObjectPropertyExpression_pc ont ope pc
and propagate_ObjectPropertyExpression_pc ont ope pc =
  let module M = ObjectPropertyExpression.Map in
  let module C = ObjectPropertyExpression.Constructor in
  match ope.data with
  | C.ObjectProperty op -> add_ObjectProperty_pc ont op pc
  | C.InverseObjectProperty op -> add_ObjectProperty_pc ont op pc;
      ont.count_InverseObjectProperty <-
      PC.sum ont.count_InverseObjectProperty pc
;;

let rec add_ClassExpression_pc ont ce pc =
  let module M = ClassExpression.Map in
  let module C = ClassExpression.Constructor in
  begin match ce.data with
    | C.Class _ -> ()
    | _ -> ont.record_ComplexClassExpression <-
        M.process ce (function
            | Some pc_old -> Some (PC.sum pc_old pc)
            | None -> Some pc
          ) ont.record_ComplexClassExpression;
  end;
  propagate_ClassExpression_pc ont ce pc
and propagate_ClassExpression_pc ont ce pc =
  let module C = ClassExpression.Constructor in
  match ce.data with
  | C.Class c -> add_Class_pc ont c pc
  | C.ObjectIntersectionOf (ce, cce) ->
      add_ClassExpression_pc ont ce pc;
      add_ClassExpression_pc ont cce pc;
      ont.count_ObjectIntersectionOf <-
      PC.sum ont.count_ObjectIntersectionOf pc  
  | C.ObjectUnionOf (ce, cce) ->
      add_ClassExpression_pc ont ce pc;
      add_ClassExpression_pc ont cce pc;
      ont.count_ObjectUnionOf <-
      PC.sum ont.count_ObjectUnionOf pc  
  | C.ObjectComplementOf de ->
      add_ClassExpression_pc ont de (PC.inverse pc);
      ont.count_ObjectComplementOf <-
      PC.sum ont.count_ObjectComplementOf pc
  | C.ObjectOneOf i_lst ->
      List.iter (fun i -> add_Individual_pc ont i pc) i_lst;
      ont.count_ObjectOneOf <-
      PC.sum ont.count_ObjectOneOf pc
  | C.ObjectSomeValuesFrom (ope, de) ->
      add_ObjectPropertyExpression_pc ont ope pc;
      add_ClassExpression_pc ont de pc;
      ont.count_ObjectSomeValuesFrom <-
      PC.sum ont.count_ObjectSomeValuesFrom pc
  | C.ObjectAllValuesFrom (ope, de) ->
      add_ObjectPropertyExpression_pc ont ope (PC.inverse pc);
      add_ClassExpression_pc ont de pc;
      ont.count_ObjectAllValuesFrom <-
      PC.sum ont.count_ObjectAllValuesFrom pc
  | C.ObjectHasValue (ope, i) ->
      add_ObjectPropertyExpression_pc ont ope pc;
      add_Individual_pc ont i pc;
      ont.count_ObjectHasValue <-
      PC.sum ont.count_ObjectHasValue pc
  | C.ObjectHasSelf ope ->
      add_ObjectPropertyExpression_pc ont ope pc;
      ont.count_ObjectHasSelf <-
      PC.sum ont.count_ObjectHasSelf pc
  | C.ObjectMinCardinality (n, ope, ceo) ->
      add_ObjectPropertyExpression_pc ont ope pc;
      begin match ceo with
        | None -> ()
        | Some ce -> add_ClassExpression_pc ont ce pc;
      end;
      ont.count_ObjectMinCardinality <-
      PC.sum ont.count_ObjectMinCardinality pc
  | C.ObjectMaxCardinality (n, ope, ceo) ->
      add_ObjectPropertyExpression_pc ont ope (PC.inverse pc);
      begin match ceo with
        | None -> ()
        | Some ce -> add_ClassExpression_pc ont ce (PC.inverse pc);
      end;
      ont.count_ObjectMinCardinality <-
      PC.sum ont.count_ObjectMinCardinality pc
  | C.ObjectExactCardinality (n, ope, ceo) ->
      add_ObjectPropertyExpression_pc ont ope (PC.symm pc);
      begin match ceo with
        | None -> ()
        | Some ce -> add_ClassExpression_pc ont ce (PC.symm pc);
      end;
      ont.count_ObjectMinCardinality <-
      PC.sum ont.count_ObjectMinCardinality pc
  | _ -> ()
;;

let add_ObjectPropertyAxiom ont opa =
  let module S = ObjectPropertyAxiom.Set in
  let module C = ObjectPropertyAxiom.Constructor in
  if not (S.mem opa ont.record_ObjectPropertyAxiom) then
    begin
      ont.record_ObjectPropertyAxiom <- S.add opa
        ont.record_ObjectPropertyAxiom;
      match opa.data with
      | C.SubObjectPropertyOf (ope_ch, ope) ->
          List.iter (fun ope -> add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Negative)) ope_ch;
          add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Positive);
          ont.count_SubPropertyOf <- succ ont.count_SubPropertyOf
      | C.EquivalentObjectProperties (ope_lst) ->
          List.iter (fun ope -> add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Both)) ope_lst;
          ont.count_EquivalentProperties <- succ ont.count_EquivalentProperties
      | C.DisjointObjectProperties _ -> ()
      | C.InverseObjectProperties (ope1, ope2) ->
          add_ObjectPropertyExpression_pc ont ope1 (PC.to_elt Polarity.Both);
          add_ObjectPropertyExpression_pc ont ope2 (PC.to_elt Polarity.Both);
          ont.count_InverseProperties <- succ ont.count_InverseProperties
      | C.ObjectPropertyDomain _ -> ()
      | C.ObjectPropertyRange _ -> ()
      | C.FunctionalObjectProperty ope ->
          add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Negative);
          ont.count_FunctionalProperty <- succ ont.count_FunctionalProperty
      | C.InverseFunctionalObjectProperty ope ->
          add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Negative);
          ont.count_InverseFunctionalProperty <- succ ont.count_InverseFunctionalProperty
      | C.ReflexiveObjectProperty _ -> ()
      | C.IrreflexiveObjectProperty _ -> ()
      | C.SymmetricObjectProperty _ -> ()
      | C.AsymmetricObjectProperty _ -> ()
      | C.TransitiveObjectProperty ope ->
          add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Both);
          ont.count_TransitiveProperty <- succ ont.count_TransitiveProperty
    end
;;

let add_ClassExpressionAxiom ont cea =
  let module S = ClassExpressionAxiom.Set in
  let module C = ClassExpressionAxiom.Constructor in
  if not (S.mem cea ont.record_ClassExpressionAxiom) then
    begin
      ont.record_ClassExpressionAxiom <- S.add cea
        ont.record_ClassExpressionAxiom;
      match cea.data with
      | C.SubClassOf (ce1, ce2) ->
          add_ClassExpression_pc ont ce1 (PC.to_elt Polarity.Negative);
          add_ClassExpression_pc ont ce2 (PC.to_elt Polarity.Positive);
          ont.count_SubClassOf <- succ ont.count_SubClassOf
      | C.EquivalentClasses (c_lst) ->
          List.iter (fun ce -> add_ClassExpression_pc ont ce (PC.to_elt Polarity.Both)) c_lst;
          ont.count_EquivalentClasses <- succ ont.count_EquivalentClasses
      | C.DisjointClasses _ -> ()
      | C.DisjointUnion _ -> ()
    end
;;

let add_Assertion ont a =
  let module S = Assertion.Set in
  let module C = Assertion.Constructor in
  if not (S.mem a ont.record_Assertion) then
    begin
      ont.record_Assertion <- S.add a
        ont.record_Assertion;
      match a.data with
      | C.SameIndividual _ -> ()
      | C.DifferentIndividuals _ -> ()
      | C.ClassAssertion (ce, i) ->
          add_ClassExpression_pc ont ce (PC.to_elt Polarity.Positive);
          add_Individual_pc ont i Polarity.Negative;
      | C.ObjectPropertyAssertion (ope, i1, i2) ->
          add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Positive);
          add_Individual_pc ont i1 (PC.to_elt Polarity.Negative);
          add_Individual_pc ont i2 (PC.to_elt Polarity.Negative);
      | C.NegativeObjectPropertyAssertion _ -> ()
      | C.DataPropertyAssertion _ -> ()
      | C.NegativeDataPropertyAssertion _ -> ()
    end
;;

(** =============== printing various information ================== **)

let print_staticstics ont out =
  fprintf out "Ontology information:\n";
  fprintf out "==============================\n";
  fprintf out "Signature (element counts):\n";
  fprintf out "---------------------------\n";
  fprintf out "\tobject properties:\t\t %n\n" (total_ObjectPropertyIRI ont);
  fprintf out "\tclasses:\t\t\t %n\n" (total_ClassIRI ont);
  fprintf out "\tindividuals:\t\t\t %n\n" (total_IndividualIRI ont);
  fprintf out "\n";
  fprintf out "Structure (expression counts):\t\t(pos)\t(neg)\n";
  fprintf out "------------------------------\n";
  fprintf out "\tinverse object porperties:\t %n\t %n\n"
    (PC.get_pos ont.count_InverseObjectProperty)
    (PC.get_neg ont.count_InverseObjectProperty);
  fprintf out "\tobject complements:\t\t %n\t %n\n"
    (PC.get_pos ont.count_ObjectComplementOf)
    (PC.get_neg ont.count_ObjectComplementOf);
  fprintf out "\tobject intersections:\t\t %n\t %n\n"
    (PC.get_pos ont.count_ObjectIntersectionOf)
    (PC.get_neg ont.count_ObjectIntersectionOf);
  fprintf out "\tobject unions:\t\t\t %n\t %n\n"
    (PC.get_pos ont.count_ObjectUnionOf)
    (PC.get_neg ont.count_ObjectUnionOf);
  fprintf out "\tobject some values from:\t %n\t %n\n"
    (PC.get_pos ont.count_ObjectSomeValuesFrom)
    (PC.get_neg ont.count_ObjectSomeValuesFrom);
  fprintf out "\tobject all values from:\t\t %n\t %n\n"
    (PC.get_pos ont.count_ObjectAllValuesFrom)
    (PC.get_neg ont.count_ObjectAllValuesFrom);
  fprintf out "\t\"owl:Thing\":\t\t\t %s\t %s\n"
    (if PC.get_pos ont.count_Thing > 0 then "+" else "-")
    (if PC.get_neg ont.count_Thing > 0 then "+" else "-");
  fprintf out "\t\"owl:Nothing\":\t\t\t %s\t %s\n"
    (if PC.get_pos ont.count_Nothing > 0 then "+" else "-")
    (if PC.get_neg ont.count_Nothing > 0 then "+" else "-");
  fprintf out "\n";
  fprintf out "Theory (axiom counts):\n";
  fprintf out "----------------------\n";
  fprintf out "\tsub object properties:\t\t\t %n\n" ont.count_SubPropertyOf;
  fprintf out "\tequivalent object properties:\t\t %n\n" ont.count_EquivalentProperties;
  fprintf out "\tinverse object properties:\t\t %n\n" ont.count_InverseProperties;
  fprintf out "\tfunctional object properties:\t\t %n\n" ont.count_FunctionalProperty;
  fprintf out "\tinverse functional object properties:\t %n\n" ont.count_InverseFunctionalProperty;
  fprintf out "\ttransitive object properties:\t\t %n\n" ont.count_TransitiveProperty;
  fprintf out "\tobject property chains:\t\t\t %n\n" ont.count_RoleComposition;
  fprintf out "\tsub classes:\t\t\t\t %n\n" ont.count_SubClassOf;
  fprintf out "\tequivalent classes:\t\t\t %n\n" ont.count_EquivalentClasses;
  fprintf out "\tclass assertions:\t\t\t %n\n" ont.count_ClassAssertion;
  fprintf out "\tobject property assertions:\t\t %n\n" ont.count_PropertyAssertion;
  fprintf out "\n";
;;