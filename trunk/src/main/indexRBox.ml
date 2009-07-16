(* the necessary information for RBox reasoning from the ontology is kept  *)
(* in index                                                                *)

open OwlSyntax
open Consed.T
module O = Ontology
module H = ObjectProperty.Hashtbl
module S = ObjectProperty.Set
module M = ObjectProperty.Map
module RS = Brole.Set
module RM = Brole.Map
module RC = ObjectPropertyExpression.Constructor
module AC = ObjectPropertyAxiom.Constructor

type t = {
  (* assigns for every atomic role [a] the set of its subproperties *)
  subproperties : RS.t H.t;
  (*| assigns for every atomic role [a] a pair consisting of:                 *)
  (*| - a map from roles [r] to the set of role [S] so that [r o S => a]      *)
  (*| - a map from roles [r] to the set of role [S] so that [r o S => (inv a)]*)
  subcomps : ((RS.t RM.t) * (RS.t RM.t)) H.t;
  (* the set of transitive atomic roles *)
  mutable trans_roles : S.t;
  (* the set of functional roles *)
  mutable funct_roles : S.t;
  (* the set of inverse functional roles *)
  mutable inv_funct_roles : S.t;
}

let create i = {
  subproperties = H.create i;
  subcomps = H.create i;
  trans_roles = S.empty;
  funct_roles = S.empty;
  inv_funct_roles = S.empty;
}

let init ont =
  let index = create 127 in
  (* initialize told predecessors adding implications between roles if     *)
  (* [imp = true], or, otherwise, inverse implication to the index         *)
  let add_subrole r s imp =
    let add_subr a r =
      H.replace_f index.subproperties a
        ( fun () -> Brole.Set.singleton r )
        ( fun s -> Brole.Set.add r s )
    in
    match r.data, s.data with
    | RC.ObjectProperty a, RC.ObjectProperty b -> add_subr a (b, imp)
    | RC.ObjectProperty a, RC.InverseObjectProperty b -> add_subr a (b, not imp)
    | RC.InverseObjectProperty a, RC.ObjectProperty b -> add_subr a (b, not imp)
    | RC.InverseObjectProperty a, RC.InverseObjectProperty b -> add_subr a (b, imp)
  in
  
(*|  let add_to_map r1 r2 =                *)
(*|    RM.process r1 ( function            *)
(*|        | None -> Some (RS.singleton r2)*)
(*|        | Some s -> Some (RS.add r2 s)  *)
(*|      )                                 *)
(*|  in                                    *)
  
(*|  let add_composition r1 r2 s =                     *)
(*|    let r1 = Brole.to_elt r1 in                     *)
(*|    let r2 = Brole.to_elt r2 in                     *)
(*|    let (b, atb) = Brole.to_elt s in                *)
(*|    let add_to_maps ma mi =                         *)
(*|      if atb then                                   *)
(*|        add_to_map r1 r2 ma,                        *)
(*|        add_to_map (Brole.inv r2) (Brole.inv r1) mi *)
(*|      else                                          *)
(*|        add_to_map (Brole.inv r2) (Brole.inv r1) ma,*)
(*|        add_to_map r1 r2 mi                         *)
(*|    in                                              *)
(*|    H.replace_f index.subcomps b                    *)
(*|      ( fun () -> add_to_maps RM.empty RM.empty)    *)
(*|      ( fun (m1, m2) -> add_to_maps m1 m2)          *)
(*|  in                                                *)
  
  let add_trans_role r =
    match r.data with
    | RC.ObjectProperty a ->
        index.trans_roles <- S.add a index.trans_roles
    | RC.InverseObjectProperty a ->
        index.trans_roles <- S.add a index.trans_roles
  in
  
  let add_funct_role r =
    match r.data with
    | RC.ObjectProperty a ->
        index.funct_roles <- S.add a index.funct_roles        
    | RC.InverseObjectProperty a ->
        index.inv_funct_roles <- S.add a index.inv_funct_roles
  in
  
  let add_inv_funct_role r =
    match r.data with
    | RC.ObjectProperty a ->
        index.inv_funct_roles <- S.add a index.inv_funct_roles        
    | RC.InverseObjectProperty a ->
        index.funct_roles <- S.add a index.funct_roles
  in
  
  O.iter_record_ObjectPropertyAxiom (fun ax -> match ax.data with
          | AC.SubObjectPropertyOf ([r], s) ->
              add_subrole s r true
          | AC.EquivalentObjectProperties op_set ->
              let op_c = ObjectPropertyExpression.Set.choose op_set in
              ObjectPropertyExpression.Set.iter (fun op ->
                      if op <> op_c then (
                        add_subrole op op_c true;
                        add_subrole op_c op true
                      )) op_set
          | AC.InverseObjectProperties (r, s) ->
              add_subrole s r false;
              add_subrole r s false
          | AC.FunctionalObjectProperty r ->
              add_funct_role r
          | AC.InverseFunctionalObjectProperty r ->
              add_inv_funct_role r    
          | AC.TransitiveObjectProperty r ->
              add_trans_role r
          | _ -> ()
    ) ont;
  (*|  Gc.compact ();  (* <- slow but useful in the long run *)*)
  index
;;