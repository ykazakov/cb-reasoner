(** a module for computing and printing of the object property taxonomy *)

open Owl2
module O = Ontology
module H = ObjectProperty.Hashtbl
module S = ObjectProperty.Set
module M = ObjectProperty.Map
module I = IndexRBox
module RS = Brole.Set
module RM = Brole.Map

(* information kept for every object property [ar] *)
type impl_set = {
  (* the set of subproperties *)
  mutable subproperties : RS.t;
  (* the stack of unprocessed subproperties *)
  mutable stack_subproperties : Brole.t list ;
  (* the set of subproperties of transitive subproperties *)
  mutable sub_trans : RS.t;
  (* the stack of unprocessed subproperties of transitive subproperties *)
  mutable stack_sub_trans : Brole.t list;
  (* the set of object properties that can entail an object property via   *)
  (* inverses, inclusions and role compositions                            *)
  mutable dependent : S.t;
  (* the stack of dependent object properties *)
  mutable stack_dependent : ObjectProperty.t list;
}

let empty ={
  subproperties = RS.empty;
  stack_subproperties = [];
  sub_trans = RS.empty;
  stack_sub_trans = [];
  dependent = S.empty;
  stack_dependent = [];
}

let init_is index a =
  let r = (a, true) in
  let s = RS.singleton r in
  if S.mem a index.I.trans_roles then
    {
      subproperties = s;
      stack_subproperties = [r];
      sub_trans = s;
      stack_sub_trans = [r];
      dependent = S.singleton a;
      stack_dependent = [a];
    }
  else { empty with
      subproperties = s;
      stack_subproperties = [r];
      dependent = S.singleton a;
      stack_dependent = [a];
    }
;;

(* The main datastructure for performing saturation under inference rules  *)
(* consists of two fileds. The filed [is] is a mapping from object         *)
(* properties the implication sets for them. The field [stack] stores the  *)
(* implication sets for which the inferences should be applied.            *)

type t = {
  is : impl_set H.t;
  mutable funct_roles : S.t;
  mutable inv_funct_roles : S.t;
  mutable stack : impl_set list;
}

let create i = {
  is =	H.create i;
  funct_roles = S.empty;
  inv_funct_roles = S.empty;
  stack = [];
}
;;

let find_subproperties t a =
  try (H.find t.is a).subproperties
  with Not_found -> RS.singleton (a, true)
;;

let find_subproperties_r t (a, ata) =
  let sa = find_subproperties t a in
  if ata then sa else RS.inv sa
;;

let find_sub_trans t a =
  try (H.find t.is a).sub_trans
  with Not_found ->	RS.empty
;;

let find_dependent t a =
  try (H.find t.is a).dependent
  with Not_found -> S.singleton a
;;

let find_funct_roles t = t.funct_roles
let find_inv_funct_roles t = t.inv_funct_roles

(* the function for creating / finding an implication set for object       *)
(* property [a]                                                            *)

let cons t index a =
  try H.find t.is a
  with Not_found ->
      let is_a = init_is index a in
      H.add t.is a is_a;
      t.stack <- is_a :: t.stack;
      is_a
;;

let process_implset t index is =
  let add_sub_trans r =
    if not (RS.mem r is.sub_trans) then (
      is.sub_trans <- RS.add r is.sub_trans;
      is.stack_sub_trans <- r :: is.stack_sub_trans;
    );
  in
  
  let add_subrole ((a, ata) as r) =
    if not (RS.mem r is.subproperties) then (
      is.subproperties <- RS.add r is.subproperties;
      is.stack_subproperties <- r :: is.stack_subproperties;
    );
    if S.mem a index.I.trans_roles then
      add_sub_trans r
  in
  
  let add_dependent a =
    if not (S.mem a is.dependent) then (
      is.dependent <- S.add a is.dependent;
      is.stack_dependent <- a :: is.stack_dependent;
    );
  in
  
  let process_subrole (a, ata) =
    try	let subproperties = H.find index.I.subproperties a in
      RS.iter add_subrole (if ata then subproperties else RS.inv subproperties);
    with Not_found -> ()
  in
  
  let process_dependent a =
    (* adding subrole dependancies *)
    S.iter add_dependent (
        try (RS.atomic (H.find index.I.subproperties a))
        with Not_found -> S.empty
      );
    (* adding compositional dependancies *)
    let ma, mi = try H.find index.I.subcomps a
      with Not_found -> RM.empty, RM.empty
    in RM.iter (fun (a, _) _ -> add_dependent a) ma;
    RM.iter (fun (a, _) _ -> add_dependent a) mi
  in
  
  let process_sub_trans (a, ata) =
    try	let subproperties = H.find index.I.subproperties a in
      RS.iter add_sub_trans (if ata then subproperties else RS.inv subproperties);
    with Not_found -> ()
  in
  
  let rec process_is () =
    (* invariant: the contents of stacks already belong to the respecive   *)
    (* sets; terminates only if all stacks are empty                       *)
    
    match is.stack_subproperties with
    | r :: stack ->
        is.stack_subproperties <- stack;
        process_subrole r;
        process_is () (* tail recursive! *)
    | [] ->
    
        match is.stack_sub_trans with
        | r :: stack ->
            is.stack_sub_trans <- stack;
            process_sub_trans r;
            process_is () (* tail recursive! *)
        | [] ->
        
            match is.stack_dependent with
            | a :: stack ->
                is.stack_dependent <- stack;
                process_dependent a;
                process_is () (* tail recursive! *)
            | [] ->
            
                () (* all stacks are empty; we are done with [is] *)
  
  in process_is ()
;;

let rec process_implset_stack t index =
  match t.stack with
  | [] -> ()
  | is :: stack ->
      t.stack <- stack;
      process_implset t index is;
      process_implset_stack t index
;;

let saturate ont =
  let index = I.init ont in
  let t = create (O.total_ObjectPropertyIRI ont) in
  
  H.iter ( fun a _ ->
          let _ = cons t index a in
          process_implset_stack t index;
    ) index.I.subproperties;
  
  H.iter ( fun a _ ->
          let _ = cons t index a in
          process_implset_stack t index;
    ) index.I.subcomps;
  
  S.iter ( fun a ->
          let _ = cons t index a in
          process_implset_stack t index;
    ) index.I.trans_roles;
  
  t.funct_roles <- index.I.funct_roles;
  t.inv_funct_roles <- index.I.inv_funct_roles;
  
  (*|  (*| ---- checking associativity for role compositions --- *)                                           *)
  (*|  (* we check that when [(v1 o v2) o v3 => r] follows from comositional or *)                            *)
  (*|  (* inclusion axioms, we also have [v1 o (v2 o v3) => r], where the       *)                            *)
  (*|  (* parentheses indicate the order of application of compositions         *)                            *)
  (*|                                                                                                         *)
  (*|  (* here we collect the set of object properties in the head of           *)                            *)
  (*|  (* compositions                                                          *)                            *)
  (*|  let hd_comp = ref S.empty in                                                                           *)
  (*|  (* here we collect the set of roles in the rhs of compositions *)                                      *)
  (*|  let rhs_comp = ref RS.empty in                                                                         *)
  (*|  H.iter (fun a (ma, mi) ->                                                                              *)
  (*|          hd_comp := S.add a !hd_comp;                                                                   *)
  (*|          RM.iter (fun r s -> rhs_comp := RS.union !rhs_comp s) ma;                                      *)
  (*|          RM.iter (fun r s -> rhs_comp := RS.union !rhs_comp s) mi;                                      *)
  (*|    ) index.I.subcomps;                                                                                  *)
  (*|  (* will not modify [hd_comp] and [rhs_comp] from now on *)                                             *)
  (*|  let hd_comp = !hd_comp in                                                                              *)
  (*|  let rhs_comp = !rhs_comp in                                                                            *)
  (*|  (* roles that occur in lhs and body of role compositions *)                                            *)
  (*|  let lhs_comp = RS.inv rhs_comp in                                                                      *)
  (*|                                                                                                         *)
  (*|  (* [close_subproperties_r sf r] computes the set of subproperties of [r] *)                            *)
  (*|  (* that occur in the set [sf]                                            *)                            *)
  (*|  let close_subproperties_r sf r = RS.inter (find_subproperties_r t r) sf                                *)
  (*|  in                                                                                                     *)
  (*|  (* [close_subproperties_s sf s] computes the set of subproperties of the *)                            *)
  (*|  (* roles in [s] that occur in [sf]                                       *)                            *)
  (*|  let close_subproperties_s sf s = RS.fold (fun r -> RS.union (close_subproperties_r sf r)) s RS.empty   *)
  (*|  in                                                                                                     *)
  (*|  (* [close_subproperties_m m sf1 sf2] computes the map containing for     *)                            *)
  (*|  (* each binding [r -> s] in [m] a binding [r1 -> s1] where [r1] is a     *)                            *)
  (*|  (* subrole of [r] in [sf1] and [s1] is the set of all subproperties of   *)                            *)
  (*|  (* [s] in [sf2]                                                          *)                            *)
  (*|  let close_subproperties_m sf1 sf2 m =                                                                  *)
  (*|    RM.fold (fun r s m ->                                                                                *)
  (*|            let s_new = close_subproperties_s sf2 s in                                                   *)
  (*|            RS.fold (fun r m -> RM.process r                                                             *)
  (*|                      (function | None -> Some s_new | Some s -> Some (RS.union s_new s) )               *)
  (*|                      m) (close_subproperties_r sf1 r) m                                                 *)
  (*|      ) m RM.empty                                                                                       *)
  (*|  in                                                                                                     *)
  (*|                                                                                                         *)
  (*|  (*| We used an additional hashtable [hd_concept_record] to quickly check some *)                       *)
  (*|  (*| compositional properties. It is obtained from [index.I.subcomps] by *)                             *)
  (*|  (*| taking for every binding [r -> m] represented by [a -> ma, mi] a    *)                             *)
  (*|  (*| binding [v -> m1, m2] represented by [b -> m1a, m1i, m2a, m2i] where*)                             *)
  (*|  (*| [v] is a (relevant) subrole of [r] and [m1] and [m2] are obtained   *)                             *)
  (*|  (*| from [m] by closing under (relevant) subproperties                       *)                        *)
  (*|  let hd_concept_record = H.create (H.length index.I.subcomps) in                                        *)
  (*|  S.iter (fun a ->                                                                                       *)
  (*|          let m1a = ref RM.empty in                                                                      *)
  (*|          let m1i = ref RM.empty in                                                                      *)
  (*|          let m2a = ref RM.empty in                                                                      *)
  (*|          let m2i = ref RM.empty in                                                                      *)
  (*|          (* we iterate under subproperties of [a] that can be results of  *)                            *)
  (*|          (* compositions                                                  *)                            *)
  (*|          RS.iter2 (fun (b, atb) ->                                                                      *)
  (*|                  let ma, mi = H.find index.I.subcomps b in                                              *)
  (*|                  let ma, mi = if atb then ma, mi else mi, ma in                                         *)
  (*|                  (* the first pair of maps is closed under subproperties  *)                            *)
  (*|                  (* in lhs and hd of compositions                         *)                            *)
  (*|                  m1a := RM.union (RS.union) (close_subproperties_m lhs_comp (hd_comp, hd_comp) ma) !m1a;*)
  (*|                  m1i := RM.union (RS.union) (close_subproperties_m lhs_comp (hd_comp, hd_comp) mi) !m1i;*)
  (*|                  (* the second pair of maps is closed under subproperties *)                            *)
  (*|                  (* in rhs of compositions                                *)                            *)
  (*|                  m2a := RM.union (RS.union) (close_subproperties_m rhs_comp rhs_comp ma) !m2a;          *)
  (*|                  m2i := RM.union (RS.union) (close_subproperties_m rhs_comp rhs_comp mi) !m2i;          *)
  (*|            ) (find_subproperties t a) (hd_comp, hd_comp);                                               *)
  (*|          (* we add a set consisting of equivalenent via dependence atomic *)                            *)
  (*|          (* concepts                                                      *)                            *)
  (*|          let s =                                                                                        *)
  (*|            S.fold (fun b s ->                                                                           *)
  (*|                    if S.mem a (find_dependent t b) then S.add b s else s                                *)
  (*|              ) (find_dependent t a) S.empty in                                                          *)
  (*|          H.add hd_concept_record a (!m1a, !m1i, !m2a, !m2i, s)                                          *)
  (*|    ) hd_comp;                                                                                           *)
  (*|                                                                                                         *)
  (*|  (* shortcut for finding equivalent dependent *)                                                        *)
  (*|  let find_dep_eq a =                                                                                    *)
  (*|    let _, _, _, _, s = H.find hd_concept_record a in s                                                  *)
  (*|  in                                                                                                     *)
  (*|                                                                                                         *)
  (*|  (*| [iter_non_inclusions f s1 s2 s] iterates [f r v] over all [r] in [s1]*)                            *)
  (*|  (*| and [v] in [s2] such that [r o v => w] holds for no [w] in [s]       *)                            *)
  (*|  let iter_non_inclusions f s1 s2 s =                                                                    *)
  (*|    RS.iter (fun r ->                                                                                    *)
  (*|        (* collect in [sr] elements such that [r o sr => s] *)                                           *)
  (*|            let sr = ref RS.empty in                                                                     *)
  (*|            RS.iter (fun (a, ata) ->                                                                     *)
  (*|                    try let _, _, ma, mi, _ = H.find hd_concept_record a in                              *)
  (*|                      let m = if ata then ma else mi in                                                  *)
  (*|                      sr := RS.union !sr                                                                 *)
  (*|                        (try RM.find r m with Not_found -> RS.empty)                                     *)
  (*|                    with Not_found -> ()                                                                 *)
  (*|              ) s;                                                                                       *)
  (*|            (* iterate over the elements in [s2] that are not in [s1] *)                                 *)
  (*|            RS.iter_diff (fun v -> f r v) s2 !sr                                                         *)
  (*|      ) s1;                                                                                              *)
  (*|  in                                                                                                     *)
  (*|                                                                                                         *)
  (*|  (* warns when [(v1 o v2) o v3 => r] but [v1 o (v2 o v3) !=> r] *)                                      *)
  (*|  let print_warning_composition v1 v2 v3 r =                                                             *)
  (*|    let (_, ata) = r in                                                                                  *)
  (*|    let v1, v2, v3, r = if ata then v1, v2, v3, r else                                                   *)
  (*|        Brole.inv v3, Brole.inv v2, Brole.inv v1, Brole.inv r in                                         *)
  (*|    (if ata then                                                                                         *)
  (*|        Printf.fprintf stdout "\nWarning! Composition is not provable: %s o (%s o %s) => %s"             *)
  (*|      else                                                                                               *)
  (*|        Printf.fprintf stdout "\nWarning! Composition is not provable: (%s o %s) o %s => %s"             *)
  (*|    ) (Brole.str v1) (Brole.str v2) (Brole.str v3) (Brole.str r)                                         *)
  (*|  in                                                                                                     *)
  (*|                                                                                                         *)
  (*|  (* [iter_comp_r f r] iterates [f] over all [v,s] such that [v o s => r] *)                             *)
  (*|  let iter_comp_r f (a, ata) =                                                                           *)
  (*|    let ma, mi = H.find index.I.subcomps a in                                                            *)
  (*|    (* iterating over [r s] such that [r o s => (a, ata) ] *)                                            *)
  (*|    RM.iter f (if ata then ma else mi)                                                                   *)
  (*|  in                                                                                                     *)
  (*|  (* the same as before but [v] should be in set filter [sf] *)                                          *)
  (*|  let iter_comp_r_sf f sf (a, ata) =                                                                     *)
  (*|    let ma, mi = H.find index.I.subcomps a in                                                            *)
  (*|    RM.iter_s f (if ata then ma else mi) sf                                                              *)
  (*|  in                                                                                                     *)
  (*|  (* [iter_comp_s f s] iterates [f] over all [v,s1] such that [v o s1 =>   *)                            *)
  (*|  (* s]                                                                    *)                            *)
  (*|  let iter_comp_s f = RS.iter2 (iter_comp_r f) (hd_comp, hd_comp)                                        *)
  (*|  in                                                                                                     *)
  (*|                                                                                                         *)
  (*|  (*| [check_compositions r m] given a role [r] and a map [m] containing*)                               *)
  (*|  (*|  [v -> s] such that [v o s => r] checks if [(v1 o v2) o v3 => r]  *)                               *)
  (*|  (*| implies [v o (v2 o v3) => r]; if not, a warning is issued         *)                               *)
  (*|  let check_compositions (a, ata as r) m =                                                               *)
  (*|    let dep_eq_a = find_dep_eq a in                                                                      *)
  (*|    let dep_eq_a = (dep_eq_a, dep_eq_a) in                                                               *)
  (*|    iter_comp_r_sf (fun u s2 ->                                                                          *)
  (*|            iter_comp_s (fun v1 s1 ->                                                                    *)
  (*|                (* [(v1 o s1) o s2 => r ] *)                                                             *)
  (*|                    let s1 = RS.inter s1 dep_eq_a in                                                     *)
  (*|                    let s = try RM.find v1 m with Not_found -> RS.empty                                  *)
  (*|                    in                                                                                   *)
  (*|                    (* [s] contains all roles such that [v1 o s => r], we  *)                            *)
  (*|                    (* need to check that [s1 o s2] => s                   *)                            *)
  (*|                    iter_non_inclusions                                                                  *)
  (*|                      (fun v2 v3 -> print_warning_composition v1 v2 v3 r)                                *)
  (*|                      s1 s2 s                                                                            *)
  (*|              ) (find_subproperties_r t u)                                                               *)
  (*|      ) dep_eq_a r                                                                                       *)
  (*|  in                                                                                                     *)
  (*|                                                                                                         *)
  (*|  (* checking overlapping compositions *)                                                                *)
  (*|  S.iter (fun a ->                                                                                       *)
  (*|          let ma, mi, _, _, _ = H.find hd_concept_record a in                                            *)
  (*|          check_compositions (a, true) ma;                                                               *)
  (*|          check_compositions (a, false) mi;                                                              *)
  (*|    ) hd_comp;                                                                                           *)
  (*|                                                                                                         *)
  (*| ------- checking for safe compositions  --------- *)
  (*|  let op_ex = ref RS.empty in                                                                          *)
  (*|  let prop_comp = ref RS.empty in                                                                      *)
  (*|  let module CC = ClassExpression.Constructor in                                                       *)
  (*|  O.iter_concept_polarity (fun c p ->                                                                  *)
  (*|          match ClassExpression.data c with                                                            *)
  (*|          | CC.SomeValuesFrom (r, _) ->                                                                *)
  (*|              if Polarity.is_positive p then op_ex := RS.add (Brole.to_elt r) !op_ex;                  *)
  (*|              if Polarity.is_negative p then                                                           *)
  (*|                prop_comp := RS.union (RS.inv (                                                        *)
  (*|                        RS.inter (hd_comp, hd_comp)                                                    *)
  (*|                          (find_subproperties_r t (Brole.to_elt r))                                    *)
  (*|                      )) !prop_comp;                                                                   *)
  (*|                                                                                                       *)
  (*|          | CC.AllValuesFrom (r, _) ->                                                                 *)
  (*|              if Polarity.is_positive p then                                                           *)
  (*|                prop_comp := RS.union (                                                                *)
  (*|                    RS.inter (hd_comp, hd_comp)                                                        *)
  (*|                      (find_subproperties_r t (Brole.to_elt r))                                        *)
  (*|                  ) !prop_comp;                                                                        *)
  (*|              if Polarity.is_negative p then op_ex := RS.add (Brole.to_elt r) !op_ex;                  *)
  (*|          | _ -> ()                                                                                    *)
  (*|    ) ont;                                                                                             *)
  (*|  ObjectProperty.Set.iter (fun a ->                                                                    *)
  (*|          let ma, mi = H.find index.I.subcomps a in                                                    *)
  (*|          RM.iter (fun r s ->                                                                          *)
  (*|                  if not (RS.is_empty (RS.inter (find_subproperties_r t (Brole.inv r)) !op_ex)) then   *)
  (*|                    RS.iter (fun v ->                                                                  *)
  (*|                            if not (RS.is_empty (RS.inter (find_subproperties_r t v) !op_ex)) then     *)
  (*|                              Printf.fprintf stderr "\nWarning! Composition is not safe: %s o %s => %s"*)
  (*|                                (Brole.str r) (Brole.str v) (Brole.str (a, true))                      *)
  (*|                      ) s;                                                                             *)
  (*|            ) ma;                                                                                      *)
  (*|          RM.iter (fun r s ->                                                                          *)
  (*|                  if not (RS.is_empty (RS.inter (find_subproperties_r t (Brole.inv r)) !op_ex)) then   *)
  (*|                    RS.iter (fun v ->                                                                  *)
  (*|                            if not (RS.is_empty (RS.inter (find_subproperties_r t v) !op_ex)) then     *)
  (*|                              Printf.fprintf stderr "\nWarning! Composition is not safe: %s o %s => %s"*)
  (*|                                (Brole.str r) (Brole.str v) (Brole.str (a, false))                     *)
  (*|                      ) s;                                                                             *)
  (*|            ) mi;                                                                                      *)
  (*|    ) (RS.atomic !prop_comp);                                                                          *)
  
  (*| ---- checking for transitive functional roles --- *)
  S.iter (fun ar ->
          let sa, si = find_subproperties t ar in
          S.iter2 (fun tr ->
                  Printf.fprintf stderr "Warning! transitive functional role %s!\n"
                    (Owl2IO.str_of_ObjectProperty tr)
            ) sa index.I.trans_roles;
          S.iter2 (fun tr ->
                  Printf.fprintf stderr "Warning! transitive functional role %s!\n"
                    (Owl2IO.str_of_ObjectProperty tr)
            ) si index.I.trans_roles;
    ) index.I.funct_roles;    
    
  S.iter (fun ar ->
          let sa, si = find_subproperties t ar in
          S.iter2 (fun tr ->
                  Printf.fprintf stderr "Warning! transitive inverse functional role %s!\n"
                    (Owl2IO.str_of_ObjectProperty tr)
            ) sa index.I.trans_roles;
          S.iter2 (fun tr ->
                  Printf.fprintf stderr "Warning! transitive inverse functional role %s!\n"
                    (Owl2IO.str_of_ObjectProperty tr)
            ) si index.I.trans_roles;
    ) index.I.inv_funct_roles;
    
  t
;;

(* a strightforward printing of implication sets *)

let print t ont out =
  O.iter_record_ObjectProperty ( fun op _ ->
          Printf.fprintf out "%s: " (Owl2IO.str_of_ObjectProperty op);
          Printf.fprintf out "subproperties: %s; " (RS.str (find_subproperties t op));
          Printf.fprintf out "sub-transitive: %s\n" (RS.str (find_sub_trans t op));
    ) ont;
;;