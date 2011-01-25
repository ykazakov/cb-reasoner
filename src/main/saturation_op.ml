(** a module for computing and printing of the object property taxonomy *)

open Owl
module O = Ontology
module H = ObjectProperty.HMap
module S = ObjectProperty.Set
module M = ObjectProperty.Map
module I = Index_op
module RS = Brole.Set
module RM = Brole.Map

(* information kept for every object property [ar] *)
type impl_set = {
	(* the set of subproperties *)
	mutable subproperties : RS.t;
	(* the stack of unprocessed subproperties *)
	mutable stack_subproperties : Brole.t list ;
	(* the set of transitive subproperties *)
	mutable sub_trans : RS.t;
}

let empty ={
	subproperties = RS.empty;
	stack_subproperties = [];
	sub_trans = RS.empty;	
}

let init_is index a =
	let r = (a, true) in
	let s = RS.singleton r in
	if S.mem a index.I.trans_roles then
		{
			subproperties = s;
			stack_subproperties = [r];
			sub_trans = s;
		}
	else { empty with
			subproperties = s;
			stack_subproperties = [r];
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
    with Not_found -> RS.empty
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
	let add_subrole ((a, ata) as r) =
		if not (RS.mem r is.subproperties) then (
			is.subproperties <- RS.add r is.subproperties;
			is.stack_subproperties <- r :: is.stack_subproperties;
			if S.mem a index.I.trans_roles then
				is.sub_trans <- RS.add r is.sub_trans;
		);
	in
	
	let process_subrole (a, ata) =
		try	let rr = H.find index.I.hrr a in
			RS.iter add_subrole (if ata then rr.I.subprop else RS.inv rr.I.subprop);
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
		| [] -> () (* all stacks are empty; we are done with [is] *)
	
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

(* a strightforward printing of implication sets *)

let print t ont out =
	O.iter_record_ObjectProperty ( fun op _ ->
					Printf.fprintf out "%s: " (Owl_io.str_of_ObjectProperty op);
					Printf.fprintf out "subproperties: %s; " (RS.str (find_subproperties t op));
					Printf.fprintf out "sub-transitive: %s\n" (RS.str (find_sub_trans t op));
		) ont;
;;

let compute ont =
	let index = I.init ont in
	let t = create (O.total_ObjectPropertyIRI ont) in
	
	H.iter ( fun a _ ->
					let _ = cons t index a in
					process_implset_stack t index;
		) index.I.hrr;
	
	S.iter ( fun a ->
					let _ = cons t index a in
					process_implset_stack t index;
		) index.I.trans_roles;
	
	t.funct_roles <- index.I.funct_roles;
	t.inv_funct_roles <- index.I.inv_funct_roles;
	
	(*| ---- checking for transitive functional roles --- *)
	S.iter (fun ar ->
					let sa, si = find_subproperties t ar in
					S.iter2 (fun tr ->
									Printf.fprintf stderr "Warning! transitive functional role %s!\n"
										(Owl_io.str_of_ObjectProperty tr)
						) sa index.I.trans_roles;
					S.iter2 (fun tr ->
									Printf.fprintf stderr "Warning! transitive functional role %s!\n"
										(Owl_io.str_of_ObjectProperty tr)
						) si index.I.trans_roles;
		) index.I.funct_roles;
	
	S.iter (fun ar ->
					let sa, si = find_subproperties t ar in
					S.iter2 (fun tr ->
									Printf.fprintf stderr "Warning! transitive inverse functional role %s!\n"
										(Owl_io.str_of_ObjectProperty tr)
						) sa index.I.trans_roles;
					S.iter2 (fun tr ->
									Printf.fprintf stderr "Warning! transitive inverse functional role %s!\n"
										(Owl_io.str_of_ObjectProperty tr)
						) si index.I.trans_roles;
		) index.I.inv_funct_roles;
	
	(*|	print t ont stdout;*)
	
	t
;;