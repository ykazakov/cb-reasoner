(* index for quick computations *)
open Owl2
open Consed.T
module O = Ontology
module OPE = ObjectPropertyExpression.Constructor
module CE = ClassExpression.Constructor

(* information stored a concept [C] *)
type concept_record = {
  (* a list of concepts [D] such that axioms [(implies C D)] or            *)
  (* [(equivalent C D)] occur in the ontology                              *)
  c_impl : ClassExpression.Set.t;
  
  (* a map from concepts [D] to a negatively occurred binary conjunction   *)
  (* [(and C D)] = [(and D C)]; we internally introduce new conjunctions   *)
  (* to deal only with binary conjunctions                                 *)
  c_conj : ClassExpression.t ClassExpression.Map.t;
  (*|  c_conja : ClassExpression.t ClassExpression.HMap.t;*)
  
  (* a map from an atomic role [r] to a set of positive and a set of       *)
  (* negative concepts [D] such that [C] implies [(all r D)]               *)
  c_succ : (ClassExpression.Set.t * ClassExpression.Set.t) ObjectProperty.Map.t;
  
  (* a map from an atomic role [r] to a set of positive and a set of       *)
  (* negative concepts [D] such that [C] implies [(all (inv r) D)]         *)
  c_succi : (ClassExpression.Set.t * ClassExpression.Set.t) ObjectProperty.Map.t;
}

(* information stored for an atomic role [r] *)
type role_record = {
  (* a map from concepts [C] to a list of positive and a list of negative  *)
  (* concepts [D] such that [C] implies [(all r D)]                        *)
  r_succ : (ClassExpression.Set.t * ClassExpression.Set.t) ClassExpression.Map.t;
  
  (* a map from concepts [C] to a list of positive and a list of negative  *)
  (* concepts [D] such that [C] implies [(all (inv r) D)]                  *)
  r_succi : (ClassExpression.Set.t * ClassExpression.Set.t) ClassExpression.Map.t;
  
  (* the set of atomic roles having a common functional supperrole with    *)
  (* [r]                                                                   *)
  r_sibl : ObjectProperty.Set.t;
  
  (* the set of atomic roles whose inverse has a common functional         *)
  (* supperrole with r                                                     *)
  r_isibl : ObjectProperty.Set.t;
  
  (* the set of atomic roles that have a common functional superrole with  *)
  (* inverse of r                                                          *)
  r_sibli : ObjectProperty.Set.t;
  
  (* the set of atomic roles whose inverse has a common functional         *)
  (* superrole with the inverse of r                                       *)
  r_isibli : ObjectProperty.Set.t;
}

let create_concept_record () = {
  c_impl = ClassExpression.Set.empty;
  c_conj = ClassExpression.Map.empty;
  (*|  c_conja = ClassExpression.HMap.create 1;*)
  c_succ = ObjectProperty.Map.empty;
  c_succi = ObjectProperty.Map.empty;
}

let empty_role_record = {
  r_succ = ClassExpression.Map.empty;
  r_succi = ClassExpression.Map.empty;
  r_sibl = ObjectProperty.Set.empty;
  r_isibl = ObjectProperty.Set.empty;
  r_sibli = ObjectProperty.Set.empty;
  r_isibli = ObjectProperty.Set.empty;
}

type t = {
  hcr : concept_record ClassExpression.Hashtbl.t;
  hrr : role_record ObjectProperty.Hashtbl.t;
}

let find_concept_record index c =
  try ClassExpression.Hashtbl.find index.hcr c
  with Not_found -> create_concept_record ()
;;

let find_role_record index r =
  try ObjectProperty.Hashtbl.find index.hrr r
  with Not_found -> empty_role_record
;;

(* functions for adding bindings to records in concept and role indexes *)
let add_c_impl index c d =
  ClassExpression.Hashtbl.replace_f index c
    ( fun () ->
          { (create_concept_record ()) with c_impl = ClassExpression.Set.singleton d }
    )
    ( fun cr ->	{ cr with c_impl = ClassExpression.Set.add d cr.c_impl })
;;

let add_c_conj index c d e =
  ClassExpression.Hashtbl.replace_f index c
    ( fun () ->
          let cr = create_concept_record () in
          (*|          ClassExpression.HMap.add cr.c_conja d e;*)
          (*|          cr                                      *)
          { cr with c_conj = ClassExpression.Map.singleton d e }
    )
    ( fun cr ->
      (*|          ClassExpression.HMap.replace_f cr.c_conja d              *)
      (*|            (fun () -> e)                                          *)
      (*|            (fun h -> if h != e then raise Index_collision else e);*)
      (*|          cr;                                                      *)
          { cr with c_conj =
              ClassExpression.Map.process d ( function
                  | None -> Some e
                  | Some h ->	Some (if h != e then invalid_arg "IntexTBox.add_c_conj" else e)
                ) cr.c_conj
          }
    )
;;

(* below [p] is a polarity: [true = positive], [false = negative] *)
let add_c_succ index c r d p =
  ClassExpression.Hashtbl.replace_f index c
    ( fun () -> { (create_concept_record ()) with
            c_succ = ObjectProperty.Map.singleton r
                (if p then ClassExpression.Set.singleton d, ClassExpression.Set.empty
                  else ClassExpression.Set.empty, ClassExpression.Set.singleton d)
          }
    )
    ( fun cr ->
          { cr with c_succ =
              ObjectProperty.Map.process r (function
                  | None -> Some (
                        if p then ClassExpression.Set.singleton d, ClassExpression.Set.empty
                        else ClassExpression.Set.empty, ClassExpression.Set.singleton d
                      )
                  | Some (sp, sn) -> Some (
                        if p then ClassExpression.Set.add d sp, sn
                        else sp, ClassExpression.Set.add d sn
                      )
                ) cr.c_succ
          }
    )
;;

let add_c_succi index c r d p =
  ClassExpression.Hashtbl.replace_f index c
    ( fun () -> { (create_concept_record ()) with
            c_succi = ObjectProperty.Map.singleton r
                (if p then ClassExpression.Set.singleton d, ClassExpression.Set.empty
                  else ClassExpression.Set.empty, ClassExpression.Set.singleton d)
          }
    )
    ( fun cr ->
          { cr with c_succi =
              ObjectProperty.Map.process r (function
                  | None -> Some (
                        if p then ClassExpression.Set.singleton d, ClassExpression.Set.empty
                        else ClassExpression.Set.empty, ClassExpression.Set.singleton d
                      )
                  | Some (sp, sn) -> Some (
                        if p then ClassExpression.Set.add d sp, sn
                        else sp, ClassExpression.Set.add d sn
                      )
                ) cr.c_succi
          }
    )
;;

let add_r_succ index r c d p =
  ObjectProperty.Hashtbl.replace_f index r
    ( fun () -> {
            empty_role_record with
            r_succ = ClassExpression.Map.singleton c
                (if p then ClassExpression.Set.singleton d, ClassExpression.Set.empty
                  else ClassExpression.Set.empty, ClassExpression.Set.singleton d)
          }
    )
    ( fun rr ->
          { rr with r_succ =
              ClassExpression.Map.process c (function
                  | None -> Some (
                        if p then ClassExpression.Set.singleton d, ClassExpression.Set.empty
                        else ClassExpression.Set.empty, ClassExpression.Set.singleton d
                      )
                  | Some (sp, sn) -> Some (
                        if p then ClassExpression.Set.add d sp, sn
                        else sp, ClassExpression.Set.add d sn
                      )
                ) rr.r_succ
          }
    )
;;

let add_r_succi index r c d p =
  ObjectProperty.Hashtbl.replace_f index r
    ( fun () -> {
            empty_role_record with
            r_succi = ClassExpression.Map.singleton c
                (if p then ClassExpression.Set.singleton d, ClassExpression.Set.empty
                  else ClassExpression.Set.empty, ClassExpression.Set.singleton d)
          }
    )
    ( fun rr ->
          { rr with r_succi =
              ClassExpression.Map.process c (function
                  | None -> Some (
                        if p then ClassExpression.Set.singleton d, ClassExpression.Set.empty
                        else ClassExpression.Set.empty, ClassExpression.Set.singleton d
                      )
                  | Some (sp, sn) -> Some (
                        if p then ClassExpression.Set.add d sp, sn
                        else sp, ClassExpression.Set.add d sn
                      )
                ) rr.r_succi
          }
    )
;;

let add_r_sibl index r s =
  ObjectProperty.Hashtbl.replace_f index r
    ( fun () -> {
            empty_role_record with
            r_sibl = ObjectProperty.Set.singleton s
          }
    )
    ( fun rr -> { rr with r_sibl = ObjectProperty.Set.add s rr.r_sibl }
    )
;;

let union_r_sibl index r ss =
  ObjectProperty.Hashtbl.replace_f index r
    ( fun () -> {
            empty_role_record with
            r_sibl = ss
          }
    )
    ( fun rr -> { rr with r_sibl = ObjectProperty.Set.union ss rr.r_sibl }
    )
;;

let add_r_isibl index r s =
  ObjectProperty.Hashtbl.replace_f index r
    ( fun () -> {
            empty_role_record with
            r_isibl = ObjectProperty.Set.singleton s
          }
    )
    ( fun rr -> { rr with r_isibl = ObjectProperty.Set.add s rr.r_isibl }
    )
;;

let union_r_isibl index r ss =
  ObjectProperty.Hashtbl.replace_f index r
    ( fun () -> {
            empty_role_record with
            r_isibl = ss
          }
    )
    ( fun rr -> { rr with r_isibl = ObjectProperty.Set.union ss rr.r_isibl }
    )
;;

let add_r_sibli index r s =
  ObjectProperty.Hashtbl.replace_f index r
    ( fun () -> {
            empty_role_record with
            r_sibli = ObjectProperty.Set.singleton s
          }
    )
    ( fun rr -> { rr with r_sibli = ObjectProperty.Set.add s rr.r_sibli }
    )
;;

let union_r_sibli index r ss =
  ObjectProperty.Hashtbl.replace_f index r
    ( fun () -> {
            empty_role_record with
            r_sibli = ss
          }
    )
    ( fun rr -> { rr with r_sibli = ObjectProperty.Set.union ss rr.r_sibli }
    )
;;

let add_r_isibli index r s =
  ObjectProperty.Hashtbl.replace_f index r
    ( fun () -> {
            empty_role_record with
            r_isibli = ObjectProperty.Set.singleton s
          }
    )
    ( fun rr -> { rr with r_isibli = ObjectProperty.Set.add s rr.r_isibli }
    )
;;

let union_r_isibli index r ss =
  ObjectProperty.Hashtbl.replace_f index r
    ( fun () -> {
            empty_role_record with
            r_isibli = ss
          }
    )
    ( fun rr -> { rr with r_isibli = ObjectProperty.Set.union ss rr.r_isibli }
    )
;;

let add_succ c_index r_index c r d p =
  add_c_succ c_index c r d p;
  add_r_succ r_index r c d p;
;;

let add_succi c_index r_index c r d p =
  add_c_succi c_index c r d p;
  add_r_succi r_index r c d p;
;;

(* an exeption if we encounter an empty conjunction [(and )], although the *)
(* krss parser should not alloow this                                      *)
exception Empty_conjunction

(* the following function indexes a negative conjunction of concepts in    *)
(* [c_lst]; if there are more than two elements in [c_lst] then it         *)
(* introduces auxiliary concept conjunctions.                              *)
(*|let rec index_conj index ont c c_set =                                     *)
(*|  if not (Cset.is_singleton c_set) then                                    *)
(*|    begin                                                                  *)
(*|      let c_set1, c_set2 = Cset.divide c_set in                            *)
(*|      let c1 = if Cset.is_singleton c_set1 then Cset.choose c_set1         *)
(*|        else begin                                                         *)
(*|          let c1 = ClassExpression.cons (CE.ObjectIntersectionOf c_set1) in*)
(*|          index_conj index ont c1 c_set1;                                  *)
(*|          c1;                                                              *)
(*|        end in                                                             *)
(*|      let c2 = if Cset.is_singleton c_set2 then Cset.choose c_set2         *)
(*|        else begin                                                         *)
(*|          let c2 = ClassExpression.cons (CE.ObjectIntersectionOf c_set2) in*)
(*|          index_conj index ont c2 c_set2;                                  *)
(*|          c2;                                                              *)
(*|        end in                                                             *)
(*|      add_c_conj index c1 c2 c;                                            *)
(*|      add_c_conj index c2 c1 c;                                            *)
(*|    end                                                                    *)
(*|;;                                                                         *)

let rec cons_conj_bin index ont c_set =
  if Cset.is_singleton c_set then Cset.choose c_set
  else begin
    let c_set1, c_set2 = Cset.divide c_set in
    let c1 = cons_conj_bin index ont c_set1 in
    let c2 = cons_conj_bin index ont c_set2 in
    let c = ClassExpression.cons (CE.ObjectIntersectionOf (c_set)) in
    add_c_conj index c1 c2 c;
    add_c_conj index c2 c1 c;
    c
  end

let index_conj index ont c c_set =
  if not (Cset.is_singleton c_set) then begin
    let c_set1, c_set2 = Cset.divide c_set in
    let c1 = cons_conj_bin index ont c_set1 in
    let c2 = cons_conj_bin index ont c_set2 in
    add_c_conj index c1 c2 c;
    add_c_conj index c2 c1 c;
  end

let estimated_concept_index_size ont =
  O.total_SubClassOf ont + O.total_ObjectIntersectionOf ont

let estimated_role_index_size ont =
  Polarity.Counter.get_pos (O.count_ObjectSomeValuesFrom ont)

(* initialize the index from an ontology [ont] *)
let init ont =
  
  let concept_index = ClassExpression.Hashtbl.create (estimated_concept_index_size ont) in
  let role_index = ObjectProperty.Hashtbl.create (estimated_role_index_size ont) in
  
  let module A = ClassExpressionAxiom.Constructor in
  O.iter_record_ClassExpressionAxiom (fun ax -> match ax.data with
          | A.SubClassOf (ce1, ce2) ->
              add_c_impl concept_index ce1 ce2
          | A.EquivalentClasses ce_set ->
              let ce_c = ClassExpression.Set.choose ce_set in
              ClassExpression.Set.iter (fun ce ->
                      if ce <> ce_c then (
                        add_c_impl concept_index ce ce_c;
                        add_c_impl concept_index ce_c ce
                      )) ce_set
          | A.DisjointClasses _ -> ()
          | A.DisjointUnion _ -> ()
    ) ont;
  
  (* We call an atomic role [r] relevant if some concept [(some r C)] or   *)
  (* [(some (inv r) C)] occurs positively. We compute the set of relevant  *)
  (* atomic roles.                                                         *)
  
  let ar_ex = ref ObjectProperty.Set.empty in
  
  O.iter_record_ComplexClassExpression (fun c p ->
          if Polarity.Counter.get_pos p > 0 then
            match c.data with
            | CE.ObjectSomeValuesFrom (r, c1) -> (
                  match r.data with
                  | OPE.ObjectProperty ar -> ar_ex := ObjectProperty.Set.add ar !ar_ex
                  | OPE.InverseObjectProperty ar -> ar_ex := ObjectProperty.Set.add ar !ar_ex
                )
            | _ -> ()
    ) ont;
  
  (* insert propagation rules for bottom into the index *)
  if O.has_positive_Nothing ont || O.has_positive_ComplementOf ont then (
    let bot = ClassExpression.cons
        (ClassExpression.Constructor.Class
          (Class.cons Class.Constructor.Nothing)) in
    ObjectProperty.Set.iter ( fun ar ->
        (* [bot] implies [(all ar bot)] and [(all (inv ar) bot)] *)
            add_succ concept_index role_index bot ar bot true;
            add_succi concept_index role_index bot ar bot true;
      ) !ar_ex;
  );
  
  let rt = ReasonerRBox.saturate ont in
  
  (* iterating over relevant atomic subproperties of [h]: iterate [fs]     *)
  (* over subproperties; iterate [fsi] over inversed subproperties; [fst]  *)
  (* over subtransitive subproperties, and [fsti] over subtransitive       *)
  (* inversed subproperties.                                               *)
  let iter_sa_si_sta_sti h fs fsi fst fsti =
    (* computing subproperties and subtransitive subproperties of [h] *)
    let (h_sa, h_si), (h_sta, h_sti) = (
        match h.data with
        | OPE.ObjectProperty ar ->
            ReasonerRBox.find_subproperties rt ar,
            ReasonerRBox.find_sub_trans rt ar
        | OPE.InverseObjectProperty ar ->
            Brole.Set.inv (ReasonerRBox.find_subproperties rt ar),
            Brole.Set.inv (ReasonerRBox.find_sub_trans rt ar)
      ) in
    ObjectProperty.Set.iter2 (fun ar -> fs ar) !ar_ex h_sa;
    ObjectProperty.Set.iter2 (fun ar -> fsi ar) !ar_ex h_si;
    ObjectProperty.Set.iter2 (fun ar -> fst ar) !ar_ex h_sta;
    ObjectProperty.Set.iter2 (fun ar -> fsti ar) !ar_ex h_sti;
  in
  
  O.iter_record_ComplexClassExpression (fun c p ->
          match c.data with
          | CE.ObjectIntersectionOf c_set when Polarity.Counter.get_neg p > 0 ->
              index_conj concept_index ont c c_set
          | CE.ObjectUnionOf c_set when Polarity.Counter.get_neg p > 0 ->
              Cset.iter (fun d -> add_c_impl concept_index d c) c_set;
          | CE.ObjectComplementOf d when Polarity.Counter.get_pos p > 0 ->
              let bot = ClassExpression.cons
                  (ClassExpression.Constructor.Class
                    (Class.cons Class.Constructor.Nothing)) in
              add_c_conj concept_index c d bot;
              add_c_conj concept_index d c bot;
          | CE.ObjectSomeValuesFrom (h, d) when Polarity.Counter.get_neg p > 0 ->
              iter_sa_si_sta_sti h
                (* if [r] implies [h] then [d] implies [(all (inv r) c)] *)
                (fun r -> add_succi concept_index role_index d r c false)
                (* if [r] implies [(inv h)] then [d] implies [(all r c)] *)
                (fun r -> add_succ concept_index role_index d r c false)
                (* if [r] subtr [h] then [c] implies [(all (inv r) c)] *)
                (fun r -> add_succi concept_index role_index c r c false)
                (* if [r] subtr [(inv h)] then [c] implies [(all r c)] *)
                (fun r -> add_succ concept_index role_index c r c false);
          | CE.ObjectAllValuesFrom (h, d) when Polarity.Counter.get_pos p > 0 ->
              iter_sa_si_sta_sti h
                (* if [r] implies [h] then [c] implies [(all r d)] *)
                (fun r -> add_succ concept_index role_index c r d true)
                (* if [r] implies [(inv h)] then [c] implies [(all (inv r) *)
                (* d)]                                                     *)
                (fun r -> add_succi concept_index role_index c r d true)
                (* if [r] subtr [h] then [c] implies [(all r c)] *)
                (fun r -> add_succ concept_index role_index c r c true)
                (* if [r] subtr [(inv h)] then [c] implies [(all (inv r)   *)
                (* c)]                                                     *)
                (fun r -> add_succi concept_index role_index c r c true);
          | _ -> (); (**! to be extended for other constructors *)
    ) ont;
  
  (* computing functional superroles and siblings *)
  (*|  let m = ref ObjectProperty.Map.empty in*)
  ObjectProperty.Set.iter ( fun af ->
          let sibl_f = ref ObjectProperty.Set.empty in
          let isibl_f = ref ObjectProperty.Set.empty in
          let af_sa, af_si = ReasonerRBox.find_subproperties rt af in
          ObjectProperty.Set.iter2
            (fun ar -> sibl_f := ObjectProperty.Set.add ar !sibl_f)
            !ar_ex af_sa;
          ObjectProperty.Set.iter2
            (fun ar -> isibl_f := ObjectProperty.Set.add ar !isibl_f)
            !ar_ex af_si;
          ObjectProperty.Set.iter (fun ar ->
                  union_r_sibl role_index ar !sibl_f;
                  union_r_isibl role_index ar !isibl_f;
            ) !sibl_f;
          ObjectProperty.Set.iter (fun ar ->
                  union_r_sibli role_index ar !sibl_f;
                  union_r_isibli role_index ar !isibl_f;
            ) !isibl_f;
    ) (ReasonerRBox.find_funct_roles rt);
  
  ObjectProperty.Set.iter ( fun af ->
          let sibl_f = ref ObjectProperty.Set.empty in
          let isibl_f = ref ObjectProperty.Set.empty in
          let af_sa, af_si = ReasonerRBox.find_subproperties rt af in
          ObjectProperty.Set.iter2
            (fun ar -> sibl_f := ObjectProperty.Set.add ar !sibl_f)
            !ar_ex af_si;
          ObjectProperty.Set.iter2
            (fun ar -> isibl_f := ObjectProperty.Set.add ar !isibl_f)
            !ar_ex af_sa;
          ObjectProperty.Set.iter (fun ar ->
                  union_r_sibl role_index ar !sibl_f;
                  union_r_isibl role_index ar !isibl_f;
            ) !sibl_f;
          ObjectProperty.Set.iter (fun ar ->
                  union_r_sibli role_index ar !sibl_f;
                  union_r_isibli role_index ar !isibl_f;
            ) !isibl_f;
    ) (ReasonerRBox.find_inv_funct_roles rt);
  (*|  Gc.compact ();  (* <- slow but useful in the long run *)*)
  {
    hcr = concept_index;
    hrr = role_index;
  }
;;

let print_statistics index =
  Printf.printf "Concept index contains:\n";
  Printf.printf "-----------------------\n";
  Printf.printf "\tConcept records: \t %n\n" (ClassExpression.Hashtbl.length index.hcr);
  Printf.printf "\tRole records: \t %n\n" (ObjectProperty.Hashtbl.length index.hrr)
;;