(* the necessary information for RBox reasoning from the ontology is kept  *)
(* in index                                                                *)

open Owl
open Consed
module O = Ontology
module H = ObjectProperty.HMap
module S = ObjectProperty.Set
module M = ObjectProperty.Map
module RS = Brole.Set
module RM = Brole.Map
module RC = ObjectPropertyExpression_Constructor
module AC = ObjectPropertyAxiom_Constructor

(* information stored for every role [r] *)
type role_record = {
  (* the set of subproperties *)
  mutable subprop : RS.t;
(*|  (*| - a map from roles [r] to the set of role [S] so that [r o S => a]      *)*)
(*|  mutable subcomp : RS.t RM.t;                                                  *)
(*|  (*| - a map from roles [r] to the set of role [S] so that [r o S => (inv a)]*)*)
(*|  mutable subcompi : RS.t RM.t;                                                 *)
}

type t = {
  hrr : role_record H.t;
  (* the set of transitive atomic roles *)
  mutable trans_roles : S.t;
  (* the set of functional roles *)
  mutable funct_roles : S.t;
  (* the set of inverse functional roles *)
  mutable inv_funct_roles : S.t;
}

let create_role_record () = {
  subprop = RS.empty;
(*|  subcomp = RM.empty; *)
(*|  subcompi = RM.empty;*)
}

let create i = {
  hrr = H.create i;
  trans_roles = S.empty;
  funct_roles = S.empty;
  inv_funct_roles = S.empty;
}

let estimated_role_index_size ont =
  Polarity.Counter.get_pos (O.count_ObjectSomeValuesFrom ont)

let init ont =
  let index = create (estimated_role_index_size ont) in
  (* initialize told predecessors adding implications between roles if     *)
  (* [imp = true], or, otherwise, inverse implication to the index         *)
  let add_subrole r s imp =
    let add_subr a r =
      let rr = try H.find index.hrr a
        with Not_found ->
            let rr = create_role_record () in H.add index.hrr a rr; rr
      in rr.subprop <- Brole.Set.add r rr.subprop
    in
    match r.data, s.data with
    | RC.ObjectProperty a, RC.ObjectProperty b -> add_subr a (b, imp)
    | RC.ObjectProperty a, RC.ObjectInverseOf b -> add_subr a (b, not imp)
    | RC.ObjectInverseOf a, RC.ObjectProperty b -> add_subr a (b, not imp)
    | RC.ObjectInverseOf a, RC.ObjectInverseOf b -> add_subr a (b, imp)
  in
  
  let add_trans_role r =
    match r.data with
    | RC.ObjectProperty a ->
        index.trans_roles <- S.add a index.trans_roles
    | RC.ObjectInverseOf a ->
        index.trans_roles <- S.add a index.trans_roles
  in
  
  let add_funct_role r =
    match r.data with
    | RC.ObjectProperty a ->
        index.funct_roles <- S.add a index.funct_roles
    | RC.ObjectInverseOf a ->
        index.inv_funct_roles <- S.add a index.inv_funct_roles
  in
  
  let add_inv_funct_role r =
    match r.data with
    | RC.ObjectProperty a ->
        index.inv_funct_roles <- S.add a index.inv_funct_roles
    | RC.ObjectInverseOf a ->
        index.funct_roles <- S.add a index.funct_roles
  in
  
  O.iter_record_ObjectPropertyAxiom (fun ax -> match ax.data with
          | AC.SubObjectPropertyOf ([r], s) ->
              add_subrole s r true
          | AC.EquivalentObjectProperties op_lst ->
              begin match op_lst with
                | op_c :: op_rest ->
                    List.iter (fun op ->
                            add_subrole op op_c true;
                            add_subrole op_c op true
                      ) op_rest
                | _ -> invalid_arg "IndexRBox.add_inv_funct_role"
              end
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