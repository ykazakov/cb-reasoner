(* index for quick computations *)
open Owl
open Consed
module O = Ontology
module OPE = ObjectPropertyExpression_Constructor
module CE = ClassExpression_Constructor

(* information stored a concept [C] *)
type concept_record = {
	(* a list of concepts [D] such that axioms [(implies C D)] or            *)
	(* [(equivalent C D)] occur in the ontology                              *)
	mutable c_impl : ClassExpression.Set.t;
	
	(* a map from concepts [D] to a set of negatively occurred binary        *)
	(* conjunctions [(and C D)] containing [D]                               *)
	c_conj : ClassExpression.Set.t ClassExpression.HMap.t;
	
	(* a map from an atomic role [r] to a set of positive and a set of       *)
	(* negative concepts [D] such that [C] implies [(all r D)]               *)
	mutable c_succ : (ClassExpression.Set.t * ClassExpression.Set.t) ObjectProperty.Map.t;
	
	(* a map from an atomic role [r] to a set of positive and a set of       *)
	(* negative concepts [D] such that [C] implies [(all (inv r) D)]         *)
	mutable c_succi : (ClassExpression.Set.t * ClassExpression.Set.t) ObjectProperty.Map.t;
}

(* information stored for an atomic role [r] *)
type role_record = {
	(* a map from concepts [C] to a list of positive and a list of negative  *)
	(* concepts [D] such that [C] implies [(all r D)]                        *)
	r_succ : (ClassExpression.Set.t * ClassExpression.Set.t) ClassExpression.HMap.t;
	
	(* a map from concepts [C] to a list of positive and a list of negative  *)
	(* concepts [D] such that [C] implies [(all (inv r) D)]                  *)
	
	r_succi : (ClassExpression.Set.t * ClassExpression.Set.t) ClassExpression.HMap.t;
	
	(* the set of atomic roles having a common functional supperrole with    *)
	(* [r]                                                                   *)
	mutable r_sibl : ObjectProperty.Set.t;
	
	(* the set of atomic roles whose inverse has a common functional         *)
	(* supperrole with r                                                     *)
	mutable r_isibl : ObjectProperty.Set.t;
	
	(* the set of atomic roles that have a common functional superrole with  *)
	(* inverse of r                                                          *)
	mutable r_sibli : ObjectProperty.Set.t;
	
	(* the set of atomic roles whose inverse has a common functional         *)
	(* superrole with the inverse of r                                       *)
	mutable r_isibli : ObjectProperty.Set.t;
}

let create_concept_record () = {
	c_impl = ClassExpression.Set.empty;
	c_conj = ClassExpression.HMap.create 1;
	c_succ = ObjectProperty.Map.empty;
	c_succi = ObjectProperty.Map.empty;
}

let create_role_record () = {
	r_succ = ClassExpression.HMap.create 1;
	r_succi = ClassExpression.HMap.create 1;
	r_sibl = ObjectProperty.Set.empty;
	r_isibl = ObjectProperty.Set.empty;
	r_sibli = ObjectProperty.Set.empty;
	r_isibli = ObjectProperty.Set.empty;
}

let empty_concept_record = create_concept_record ()
let empty_role_record = create_role_record ()

type t = {
	hcr : concept_record ClassExpression.HMap.t;
	hrr : role_record ObjectProperty.HMap.t;
}

(* required by the interface *)
let find_concept_record index c =
	try ClassExpression.HMap.find index.hcr c
	with Not_found -> empty_concept_record
;;
let find_role_record index r =
	try ObjectProperty.HMap.find index.hrr r
	with Not_found -> empty_role_record
;;

let cons_concept_record index c =
	try ClassExpression.HMap.find index c
	with Not_found ->
			let cr = create_concept_record () in
			ClassExpression.HMap.add index c cr;
			cr
;;

let cons_role_record index r =
	try ObjectProperty.HMap.find index r
	with Not_found ->
			let rr = create_role_record () in
			ObjectProperty.HMap.add index r rr;
			rr
;;

(* functions for adding bindings to records in concept and role indexes *)
let add_c_impl index c d =
	let cr = cons_concept_record index c in
	cr.c_impl <- ClassExpression.Set.add d cr.c_impl
;;

let add_c_conj index c d e =
	let cr = cons_concept_record index c in
	let s =
		try ClassExpression.HMap.find cr.c_conj d
		with Not_found -> ClassExpression.Set.empty
	in
	ClassExpression.HMap.replace cr.c_conj d
		(ClassExpression.Set.add e s)
;;

(* below [p] is a polarity: [true = positive], [false = negative] *)
let add_c_succ index c r d p =
	let cr = cons_concept_record index c in
	cr.c_succ <- ObjectProperty.Map.process r (function
			| None -> Some (
						if p then ClassExpression.Set.singleton d, ClassExpression.Set.empty
						else ClassExpression.Set.empty, ClassExpression.Set.singleton d
					)
			| Some (sp, sn) -> Some (
						if p then ClassExpression.Set.add d sp, sn
						else sp, ClassExpression.Set.add d sn
					)
		) cr.c_succ
;;

let add_c_succi index c r d p =
	let cr = cons_concept_record index c in
	cr.c_succi <- ObjectProperty.Map.process r (function
			| None -> Some (
						if p then ClassExpression.Set.singleton d, ClassExpression.Set.empty
						else ClassExpression.Set.empty, ClassExpression.Set.singleton d
					)
			| Some (sp, sn) -> Some (
						if p then ClassExpression.Set.add d sp, sn
						else sp, ClassExpression.Set.add d sn
					)
		) cr.c_succi
;;

let add_r_succ index r c d p =
	let rr = cons_role_record index r in
	let sp, sn = try ClassExpression.HMap.find rr.r_succ c
		with Not_found -> ClassExpression.Set.empty, ClassExpression.Set.empty
	in
	ClassExpression.HMap.replace rr.r_succ c
		(if p then ClassExpression.Set.add d sp, sn
			else sp, ClassExpression.Set.add d sn)
;;

let add_r_succi index r c d p =
	let rr = cons_role_record index r in
	let sp, sn = try ClassExpression.HMap.find rr.r_succi c
		with Not_found -> ClassExpression.Set.empty, ClassExpression.Set.empty
	in
	ClassExpression.HMap.replace rr.r_succi c
		(if p then ClassExpression.Set.add d sp, sn
			else sp, ClassExpression.Set.add d sn)
;;

let add_r_sibl index r s =
	let rr = cons_role_record index r in
	rr.r_sibl <- ObjectProperty.Set.add s rr.r_sibl
;;

let union_r_sibl index r ss =
	let rr = cons_role_record index r in
	rr.r_sibl <- ObjectProperty.Set.union ss rr.r_sibl
;;

let add_r_isibl index r s =
	let rr = cons_role_record index r in
	rr.r_isibl <- ObjectProperty.Set.add s rr.r_isibl
;;

let union_r_isibl index r ss =
	let rr = cons_role_record index r in
	rr.r_isibl <- ObjectProperty.Set.union ss rr.r_isibl
;;

let add_r_sibli index r s =
	let rr = cons_role_record index r in
	rr.r_sibli <- ObjectProperty.Set.add s rr.r_sibli
;;

let union_r_sibli index r ss =
	let rr = cons_role_record index r in
	rr.r_sibli <- ObjectProperty.Set.union ss rr.r_sibli
;;

let add_r_isibli index r s =
	let rr = cons_role_record index r in
	rr.r_isibli <- ObjectProperty.Set.add s rr.r_isibli
;;

let union_r_isibli index r ss =
	let rr = cons_role_record index r in
	rr.r_isibli <- ObjectProperty.Set.union ss rr.r_isibli
;;

let add_succ c_index r_index c r d p =
	add_c_succ c_index c r d p;
	add_r_succ r_index r c d p;
;;

let add_succi c_index r_index c r d p =
	add_c_succi c_index c r d p;
	add_r_succi r_index r c d p;
;;

let estimated_concept_index_size ont =
	O.total_SubClassOf ont + O.total_ObjectIntersectionOf ont

let estimated_role_index_size ont =
	Polarity.Counter.get_pos (O.count_ObjectSomeValuesFrom ont)

(* initialize the index from an ontology [ont] *)
let init ont =
	
	let concept_index = ClassExpression.HMap.create (estimated_concept_index_size ont) in
	let role_index = ObjectProperty.HMap.create (estimated_role_index_size ont) in
	
	let module A = ClassAxiom_Constructor in
	O.iter_record_ClassAxiom (fun ax -> match ax.data with
					| A.SubClassOf (ce1, ce2) ->
							add_c_impl concept_index ce1 ce2
					| A.EquivalentClasses ce_lst ->
							begin match ce_lst with
								| ce_c :: ce_rest ->
										List.iter (fun ce ->
														add_c_impl concept_index ce ce_c;
														add_c_impl concept_index ce_c ce
											) ce_rest
								| _ -> invalid_arg "indexTBox.init"
							end
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
									| OPE.ObjectInverseOf ar -> ar_ex := ObjectProperty.Set.add ar !ar_ex
								)
						| _ -> ()
		) ont;
	
	(* insert propagation rules for bottom into the index *)
	if O.has_positive_Nothing ont || O.has_positive_ComplementOf ont then (
		let bot = ClassExpression.cons
				(ClassExpression_Constructor.Class Class_Constructor.Nothing) in
		ObjectProperty.Set.iter ( fun ar ->
				(* [bot] implies [(all ar bot)] and [(all (inv ar) bot)] *)
						add_succ concept_index role_index bot ar bot true;
						add_succi concept_index role_index bot ar bot true;
			) !ar_ex;
	);
	
	let rt = Saturation_op.compute ont in
	
	(* iterating over relevant atomic subproperties of [h]: iterate [fs]     *)
	(* over relevant subproperties; iterate [fsi] over relevant inversed     *)
	(* subproperties                                                         *)
	let iter_relevant_sa_si h fs fsi =
		let h_sa, h_si =
			match h.data with
			| OPE.ObjectProperty ar -> Saturation_op.find_subproperties rt ar
			| OPE.ObjectInverseOf ar -> Brole.Set.inv (Saturation_op.find_subproperties rt ar)
		in
		ObjectProperty.Set.iter2 (fun ar -> fs ar) !ar_ex h_sa;
		ObjectProperty.Set.iter2 (fun ar -> fsi ar) !ar_ex h_si;
	in
	(* iterating over sub-transitive properties of [h]: iterate fst over     *)
	(* over transitive subproperties, and [fsti] over transitive inversed    *)
	(* subproperties.                                                        *)
	let iter_sta_sti h fst fsti =
		let h_sta, h_sti =
			match h.data with
			| OPE.ObjectProperty ar -> Saturation_op.find_sub_trans rt ar
			| OPE.ObjectInverseOf ar -> Brole.Set.inv (Saturation_op.find_sub_trans rt ar)
		in
		ObjectProperty.Set.iter (fun ar -> fst ar) h_sta;
		ObjectProperty.Set.iter (fun ar -> fsti ar) h_sti;
	in
	
	O.iter_record_ComplexClassExpression (fun c p ->
					match c.data with
					| CE.ObjectIntersectionOf (c1, c2) | CE.ObjectIntersectionOfrec (c1, c2)
					when Polarity.Counter.get_neg p > 0 ->
							add_c_conj concept_index c1 c2 c;
							add_c_conj concept_index c2 c1 c;
					| CE.ObjectUnionOf (c1, c2) | CE.ObjectUnionOfrec (c1, c2)
					when Polarity.Counter.get_neg p > 0 ->
							add_c_impl concept_index c1 c;
							add_c_impl concept_index c2 c;
					| CE.ObjectComplementOf d when Polarity.Counter.get_pos p > 0 ->
							let bot = ClassExpression.cons
									(ClassExpression_Constructor.Class Class_Constructor.Nothing) in
							add_c_conj concept_index c d bot;
							add_c_conj concept_index d c bot;
					| CE.ObjectSomeValuesFrom (h, d) when Polarity.Counter.get_neg p > 0 ->
							iter_relevant_sa_si h
								(*| if [r] implies [h] then [d] implies [(all (inv r) c)] *)
								(fun r -> add_succi concept_index role_index d r c false)
								(*| if [r] implies [(inv h)] then [d] implies [(all r c)] *)
								(fun r -> add_succ concept_index role_index d r c false);
							iter_sta_sti h
								(fun t ->
									(*| if [t] subtr [h] then [ObjectSomeValuesFrom(t,d)] implies [c] *)
											let opt = ObjectPropertyExpression.cons (ObjectPropertyExpression_Constructor.ObjectProperty t) in
											let ctd = ClassExpression.cons (ClassExpression_Constructor.ObjectSomeValuesFrom (opt, d)) in
											if not (ClassExpression.equal c ctd) then add_c_impl concept_index ctd c;
											let st, sit = (Saturation_op.find_subproperties rt t) in
											(* if [r] implies [t] then *)
											(*| [ObjectSomeValuesFrom(t,d)] implies [(all (inv r) ObjectSomeValuesFrom(t,d))] and *)
											(*| [d] implies [(all (inv r) ObjectSomeValuesFrom(t,d))] *)
											ObjectProperty.Set.iter2 (fun r ->
															add_succi concept_index role_index ctd r ctd false;
															add_succi concept_index role_index d r ctd false;
												) !ar_ex st;
											(* if [(inv r)] implies [t] then *)
											(*| [ObjectSomeValuesFrom(t,d)] implies [(all r ObjectSomeValuesFrom(t,d))] and *)
											(*| [d] implies [(all r ObjectSomeValuesFrom(t,d))] *)
											let st, sit = (Saturation_op.find_subproperties rt t) in
											ObjectProperty.Set.iter2 (fun r ->
															add_succ concept_index role_index ctd r ctd false;
															add_succ concept_index role_index d r ctd false;
												) !ar_ex sit;
								)
								(fun t ->
									(*| if [t] subtr [(inv h)] then [ObjectSomeValuesFrom((inv t),d)] implies [c] *)
											let opt = ObjectPropertyExpression.cons (ObjectPropertyExpression_Constructor.ObjectInverseOf t) in
											let ctd = ClassExpression.cons (ClassExpression_Constructor.ObjectSomeValuesFrom (opt, d)) in
											let st, sit = (Saturation_op.find_subproperties rt t) in
											if not (ClassExpression.equal c ctd) then add_c_impl concept_index ctd c;
											(* if [r] implies [t] then *)
											(*| [ObjectSomeValuesFrom((inv t),d)] implies [(all r ObjectSomeValuesFrom((inv t),d))] and *)
											(*| [d] implies [(all r ObjectSomeValuesFrom((inv t),d))] *)
											ObjectProperty.Set.iter2 (fun r ->
															add_succ concept_index role_index ctd r ctd false;
															add_succ concept_index role_index d r ctd false;
												) !ar_ex st;
											(* if [(inv r)] implies [t] then *)
											(*| [ObjectSomeValuesFrom((inv t),d)] implies [(all (inv r) ObjectSomeValuesFrom(t,d))] and *)
											(*| [d] implies [(all (inv r) ObjectSomeValuesFrom((inv t),d))] *)
											let st, sit = (Saturation_op.find_subproperties rt t) in
											ObjectProperty.Set.iter2 (fun r ->
															add_succi concept_index role_index ctd r ctd false;
															add_succi concept_index role_index d r ctd false;
												) !ar_ex sit;
								);
					| CE.ObjectAllValuesFrom (h, d) when Polarity.Counter.get_pos p > 0 ->
							iter_relevant_sa_si h
								(*| if [r] implies [h] then [c] implies [(all r d)] *)
								(fun r -> add_succ concept_index role_index c r d true)
								(*| if [r] implies [(inv h)] then [c] implies [(all (inv r) d)] *)
								(fun r -> add_succi concept_index role_index c r d true);
							iter_sta_sti h
								(fun t ->
									(*| if [t] subtr [h] then [c] implies [ObjectAllValuesFrom(t,d)] *)
											let opt = ObjectPropertyExpression.cons (ObjectPropertyExpression_Constructor.ObjectProperty t) in
											let ctd = ClassExpression.cons (ClassExpression_Constructor.ObjectAllValuesFrom (opt, d)) in
											if not (ClassExpression.equal c ctd) then add_c_impl concept_index c ctd;
											let st, sit = (Saturation_op.find_subproperties rt t) in
											(* if [r] implies [t] then *)
											(*| [ObjectAllValuesFrom(t,d)] implies [(all r ObjectAllValuesFrom(t,d))] and *)
											(*| [ObjectAllValuesFrom(t,d)] implies [(all r d)] *)
											ObjectProperty.Set.iter2 (fun r ->
															add_succ concept_index role_index ctd r ctd true;
															add_succ concept_index role_index ctd r d true;
												) !ar_ex st;
											(* if [(inv r)] implies [t] then *)
											(*| [ObjectAllValuesFrom(t,d)] implies [(all (inv r) ObjectAllValuesFrom(t,d))] and *)
											(*| [ObjectAllValuesFrom(t,d)] implies [(all (inv r) d)] *)
											ObjectProperty.Set.iter2 (fun r ->
															add_succi concept_index role_index ctd r ctd true;
															add_succi concept_index role_index ctd r d true;
												) !ar_ex st;
								)
								(fun t ->
									(*| if [t] subtr [(inv h)] then [c] implies [ObjectAllValuesFrom((inv t),d)] *)
											let opt = ObjectPropertyExpression.cons (ObjectPropertyExpression_Constructor.ObjectInverseOf t) in
											let ctd = ClassExpression.cons (ClassExpression_Constructor.ObjectAllValuesFrom (opt, d)) in
											if not (ClassExpression.equal c ctd) then add_c_impl concept_index c ctd;
											let st, sit = (Saturation_op.find_subproperties rt t) in
											(* if [r] implies [t] then *)
											(*| [ObjectAllValuesFrom((inv t),d)] implies [(all (inv r) ObjectAllValuesFrom((inv t),d))] and *)
											(*| [ObjectAllValuesFrom(t,d)] implies [(all (inv r) d)] *)
											ObjectProperty.Set.iter2 (fun r ->
															add_succi concept_index role_index ctd r ctd true;
															add_succi concept_index role_index ctd r d true;
												) !ar_ex st;
											(* if [(inv r)] implies [t] then *)
											(*| [ObjectAllValuesFrom((inv t),d)] implies [(all r ObjectAllValuesFrom((inv t),d))] and *)
											(*| [ObjectAllValuesFrom((inv t),d)] implies [(all r d)] *)
											ObjectProperty.Set.iter2 (fun r ->
															add_succ concept_index role_index ctd r ctd true;
															add_succ concept_index role_index ctd r d true;
												) !ar_ex st;
								);
					| _ -> (); (**! to be extended for other constructors *)
		) ont;
	
	(* computing functional superroles and siblings *)
	(*|  let m = ref ObjectProperty.Map.empty in*)
	ObjectProperty.Set.iter ( fun af ->
					let sibl_f = ref ObjectProperty.Set.empty in
					let isibl_f = ref ObjectProperty.Set.empty in
					let af_sa, af_si = Saturation_op.find_subproperties rt af in
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
		) (Saturation_op.find_funct_roles rt);
	
	ObjectProperty.Set.iter ( fun af ->
					let sibl_f = ref ObjectProperty.Set.empty in
					let isibl_f = ref ObjectProperty.Set.empty in
					let af_sa, af_si = Saturation_op.find_subproperties rt af in
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
		) (Saturation_op.find_inv_funct_roles rt);
	(*|  Gc.compact ();  (* <- slow but useful in the long run *)*)
	{
		hcr = concept_index;
		hrr = role_index;
	}
;;

let print_statistics index =
	Printf.printf "Concept index contains:\n";
	Printf.printf "-----------------------\n";
	Printf.printf "\tConcept records: \t %n\n" (ClassExpression.HMap.length index.hcr);
	Printf.printf "\tRole records: \t %n\n" (ObjectProperty.HMap.length index.hrr)
;;