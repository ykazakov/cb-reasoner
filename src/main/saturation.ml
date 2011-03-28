(** reasoning algorithm is implemented here                                 *)

open Owl
open Consed
module O = Ontology
module I = Index
module PT = Progress_tracker

(* The main units of our saturation-based algorithm are implication sets.  *)
(* An implication set is an implication between a set of concepts [root]   *)
(* and the set of [implied] concepts (both sets are interpreted as a       *)
(* conjunction). We treat negatively and positive occurrences of the same  *)
(* concepts in ontology as different concepts. For example, in the axiom:  *)

(*| [(implies (some r B) (some r (some r B))]                              *)

(* two occurrences of the concept [(some r B)] are treated as different.   *)
(* They will be replaced by different concepts if the standard structural  *)
(* transformation is applied. The first one is negative and it can be used *)
(* to trigger inference rules---if we have obtained such concept in the    *)
(* inference then we should add the concept in the right-hand-side of the  *)
(* axiom as a conclusion. The second one is positive---if we obtain such   *)
(* concept then we should add an [r]-successor with [B]. Every positive    *)
(* concept implicitly implies its negative variant but not vice versa.     *)
(* Therefore, the positive [(some r B)] will also trigger the rule of the  *)
(* axiom, but the negative one will not be used for creating successors.   *)
(* To deal with different types of concepts, the [root] of the implication *)
(* sets is represented as a pair of concept sets---the first set for       *)
(* positive, and the second set for negative concepts. The concepts for    *)
(* which inference rules are yet to be applied are stored in two stacks    *)
(* [stack_pos] for positive concepts and [stack_neg] for negative          *)
(* concepts. All concepts in [implied] are thought to be negative.         *)

module IS = Intset

type impl_set = {
	(* the root of the implication set *)
	core_pos : ClassExpression.Set.t;
	core_neg : ClassExpression.Set.t;
	(* concepts implied by the core *)
	implied : ClassExpression.HSet.t;
	(* stack of unprocessed positive and negative concepts; invariant: every *)
	(* concept in [implied_new_pos] and in [implied_new_neg] already occurs  *)
	(* in [implied].                                                         *)
	mutable implied_new_pos : ClassExpression.t list;
	mutable implied_new_neg : ClassExpression.t list;
	(* [children] maps every role [r] the set of child roots existentially   *)
	(* "required" by the current implication set, and which can be later     *)
	(* "affected" by the concepts in the implication set.                    *)
	mutable children : edge list ObjectProperty.Map.t;
	mutable childreni : edge list ObjectProperty.Map.t;
	(* [parents] maps every role [r] to the set of child roots that          *)
	(* existentially require the current implication set and later can be    *)
	(* "affected" by the concepts in the implication set.                    *)
	mutable parents : IS.t ObjectProperty.Map.t;
	mutable parentsi : IS.t ObjectProperty.Map.t;
}
(* edge stores the relationship between parents and children *)
and edge = {
	mutable id : int;
	is_parent : impl_set;
	mutable rt_child_new_pos : ClassExpression.Set.t; (* keep     *)
	mutable rt_child_new_neg : ClassExpression.Set.t; (* disjoint *)
	mutable is_child : impl_set;
	mutable roles : ObjectProperty.Set.t;
	mutable rolesi : ObjectProperty.Set.t;
	mutable roles_new : ObjectProperty.t list;
	mutable rolesi_new : ObjectProperty.t list;
}

let ed_get_rt_child_new_pos ed =
	ClassExpression.Set.filter (fun c -> not (ClassExpression.HSet.mem ed.is_child.implied c)) ed.rt_child_new_pos
let ed_get_rt_child_new_neg ed =
	ClassExpression.Set.filter (fun c -> not (ClassExpression.HSet.mem ed.is_child.implied c)) ed.rt_child_new_neg

(* Hash set of impication sets *)
module HI = Hashsetlp.Make (struct
		type t = ClassExpression.Set.t * ClassExpression.Set.t
		type elt = impl_set
		let equal (cp1, cn1) (cp2, cn2) =
			ClassExpression.Set.equal cp1 cp2 && ClassExpression.Set.equal cn1 cn2
		let hash (cp, cn) = (ClassExpression.Set.hash cp) + (ClassExpression.Set.hash cn)
		let key is = is.core_pos, is.core_neg
	end)

(* Hash set of edges *)
module HE = Hashsetlp.Make (struct
		type t = int
		type elt = edge
		let equal = (==)
		let hash i = i
		let key ed = ed.id
	end)

let is_processed_is is =
	is.implied_new_pos == [] && is.implied_new_neg == []

let is_processed_ed_roles ed =
	ed.roles_new == [] && ed.rolesi_new == []
;;

let is_processed_ed_root ed =
	ClassExpression.Set.is_empty ed.rt_child_new_pos &&
	ClassExpression.Set.is_empty ed.rt_child_new_neg
;;

(* The main datastructure for performing saturation under inference rules  *)
(* consists of two fileds. The filed [rt_to_is] is a mapping from roots to *)
(* the implication sets for them. Th field [stack_is] stores the           *)
(* implication sets for which the inferences should be applied. The field  *)
(* [stack_ed] stores edges for which inferences should be applied.         *)
(* Invariant: [is] is not processed if and only if [is] is in [stack_is];  *)
(* [ed] is not processed if and only if [ed] is in [stack_ed].             *)

type t = {
	rt_to_is : HI.t;
	mutable edges : HE.t;
	mutable edge_count : int;
	(* the stack of and the current unprocessed implication set. Invariants: *)
	(* contains [is] iff [is_processed_is is = false]; [current_is = None]   *)
	(* implies [stack_is = []]                                               *)
	mutable current_is : impl_set option;
	mutable stack_is : impl_set list;
	(* the stack of and the current edge whose roles are unprocessed.        *)
	(* Invariants: contains [ed] iff [is_processed_ed_roles ed = false];     *)
	(* [current_ed_roles = None] implies [stack_ed_roles = []]               *)
	mutable current_ed_roles : edge option;
	mutable stack_ed_roles : edge list;
	(* the stack of and the current edge whose roots are unprocessed.        *)
	(* Invariants: contains [ed] iff [is_processed_ed_root ed = false];      *)
	(* [current_ed_root = None] implies [stack_ed_root = []]                 *)
	mutable current_ed_root : edge option;
	mutable stack_ed_root : edge list;
	(* the top concept, if there is any *)
	mutable top : ClassExpression.t option;
}

let t_create i = {
	rt_to_is = HI.create i;
	edges = HE.create i;
	edge_count = 0;
	current_is = None;
	stack_is = [];
	current_ed_roles = None;
	stack_ed_roles = [];
	current_ed_root = None;
	stack_ed_root = [];
	top = None;
}
;;

(* required by the interface *)
let find_option_top t = t.top
;;

(* required by the interface *)
let find_implied t c =
	let is = HI.find t.rt_to_is ((ClassExpression.Set.singleton c), ClassExpression.Set.empty)
	in is.implied
;;

(**=================== functions that do NOT modify the datastructures =================*)

(* iterating over sibling roles *)

let iter_isibling_parents f is role_record = function
	| true ->
			ObjectProperty.Map.iter_s (fun ar -> f ar true) is.parents role_record.I.r_isibl;
			ObjectProperty.Map.iter_s (fun ar -> f ar false) is.parentsi role_record.I.r_sibl;
	| false ->
			ObjectProperty.Map.iter_s (fun ar -> f ar true) is.parents role_record.I.r_isibli;
			ObjectProperty.Map.iter_s (fun ar -> f ar false) is.parentsi role_record.I.r_sibli;
;;

let iter_sibling_children f is role_record = function
	| true ->
			ObjectProperty.Map.iter_s (fun ar -> f ar true) is.children role_record.I.r_sibl;
			ObjectProperty.Map.iter_s (fun ar -> f ar false) is.childreni role_record.I.r_isibl;
	| false ->
			ObjectProperty.Map.iter_s (fun ar -> f ar true) is.children role_record.I.r_sibli;
			ObjectProperty.Map.iter_s (fun ar -> f ar false) is.childreni role_record.I.r_isibli;
;;

(* iterating [f] over edges [ed] that are child edges of [is] for some     *)
(* sibling role of the role for [role_record,role_dir]                     *)
let iter_sibling_child_edges_r_aux f index is role_record role_dir accu =
	iter_sibling_children (fun _ _ ed_list ->
					match ed_list with
					| [ed] -> if not (IS.mem ed.id !accu) then (
								accu := IS.add ed.id !accu; f ed)
					| _ -> failwith "Functional child is not unique!\n"
		) is role_record role_dir
;;

let iter_sibling_child_edges_r f index is role_record role_dir =
	iter_sibling_child_edges_r_aux f index is role_record role_dir (ref IS.empty)

(* iterating [f] over edges [ed] that are child edges of [is] for some     *)
(* sibling role of a role in [roles, rolesi]                               *)
let iter_sibling_child_edges_ed f index is roles rolesi =
	let accu = ref IS.empty in
	let iter_roles f =
		ObjectProperty.Set.iter (f true) roles;
		ObjectProperty.Set.iter (f false) rolesi;
	in
	iter_roles (fun ar_dir ar ->
					let ar_rr = I.find_role_record index ar in
					iter_sibling_child_edges_r_aux f index is ar_rr ar_dir accu
		)
;;

(* iterating [f] over child edges of [is] without repetitions *)
let iter_child_edges f t is =
	(* here we remember processed edges *)
	let accu = ref IS.empty in
	let iter_edges f =
		ObjectProperty.Map.iter (fun _ -> List.iter f) is.children;
		ObjectProperty.Map.iter (fun _ -> List.iter f) is.childreni;
	in iter_edges (fun ed ->
					if not (IS.mem ed.id !accu) then (
						accu := IS.add ed.id !accu;
						f ed
					))
;;

(* iterating [f] over edges [ed] that are parent edges of [is] for some    *)
(* inversed sibling role of a role in [roles, rolesi]                      *)
let iter_isibling_parent_edges f t index is roles rolesi =
	(* here we remember processed edges *)
	let accu = ref IS.empty in
	(* we merge all edges containing some sibling role into one *)
	let iter_roles f =
		ObjectProperty.Set.iter (fun ar -> f ar true) roles;
		ObjectProperty.Set.iter (fun ar -> f ar false) rolesi;
	in
	iter_roles (fun ar ar_dir ->
					let ar_rr = I.find_role_record index ar in
					iter_isibling_parents (fun _ _ ed_set ->
									IS.iter (fun ed_id ->
													if not (IS.mem ed_id !accu) then (
														accu := IS.add ed_id !accu;
														let ed = HE.find t.edges ed_id in f ed
													)
										) ed_set (* IS.iter *)
						) is ar_rr ar_dir
		) (* iter_roles *)
;;

let role_is_functional role_record = function
	| true -> not (ObjectProperty.Set.is_empty role_record.I.r_sibl)
	| false -> not (ObjectProperty.Set.is_empty role_record.I.r_isibli)
;;

(**============ checking datastructure invariants =================*)
(**================ ( useful for debugging ) ======================*)
(* checking consistensy of implication sets *)
let check_implication_sets t =
	HI.iter (fun is ->
			(* all elements of [is.root] should be in [is.implied] *)
			(*|          assert (ClassExpression.Set.is_subset is.core_pos is.implied);*)
			(*|          assert (ClassExpression.Set.is_subset is.core_neg is.implied);*)
					assert (ClassExpression.Set.for_all (ClassExpression.HSet.mem is.implied) is.core_pos);
					assert (ClassExpression.Set.for_all (ClassExpression.HSet.mem is.implied) is.core_neg);
					(* every parent edge [ed] of [is] for a role [r = (ar, ar_dir)]  *)
					(* sould: 1. have a child implication set [ed.is_child_op]       *)
					(* assigned; 2. this child implication set shoud coninside with  *)
					(* [is], and 3. the role [r = (ar, ar_dir)] should belong to the *)
					(* role set of [ed].                                             *)
					ObjectProperty.Map.iter (fun ar ed_id_set ->
									IS.iter (fun ed_id ->
													let ed = HE.find t.edges ed_id in
													assert (is == ed.is_child);
													assert (ObjectProperty.Set.mem ar ed.roles);
										) ed_id_set;
						) is.parents;
					ObjectProperty.Map.iter (fun ar ed_id_set ->
									IS.iter (fun ed_id ->
													let ed = HE.find t.edges ed_id in
													assert (is == ed.is_child);
													assert (ObjectProperty.Set.mem ar ed.rolesi);
										) ed_id_set;
						) is.parentsi;
					(* every child edge [ed] of [is] for a role [r = (ar, ar_dir)]   *)
					(* should 1. have as a parent implication set [ed.is_parent] the *)
					(* implication set [is], and 2. the role [r = (ar, ar_dir)]      *)
					(* should belong to the role set of [ed].                        *)
					ObjectProperty.Map.iter (fun ar ed_lst ->
									List.iter (fun ed ->
													assert (ed.is_parent == is);
													assert (ObjectProperty.Set.mem ar ed.roles);
										) ed_lst;
						) is.children;
					ObjectProperty.Map.iter (fun ar ed_lst ->
									List.iter (fun ed ->
													assert (ed.is_parent == is);
													assert (ObjectProperty.Set.mem ar ed.rolesi);
										) ed_lst;
						) is.childreni;
		) t.rt_to_is;
;;

(* checking consistensy of edges *)
let check_edges t index =
	HE.iter (fun ed ->
			(* every edge [ed] in the hashtable should not be delted *)
					assert (ed.id <> 0);
					(* finding roles that are already processed *)
					let processed_roles = List.fold_left (
								fun s ar -> ObjectProperty.Set.remove ar s
							) ed.roles ed.roles_new in
					let processed_rolesi = List.fold_left (
								fun s ar -> ObjectProperty.Set.remove ar s
							) ed.rolesi ed.rolesi_new in
					(* the edge [ed] should be a child in the parent implication set *)
					(* [ed.is_parent] for every processed role [r = (ar, ar_dir)] of *)
					(* the edge.                                                     *)
					ObjectProperty.Set.iter (fun ar ->
									try assert (List.memq ed (ObjectProperty.Map.find ar ed.is_parent.children));
									with Not_found -> assert false;
						) processed_roles;
					ObjectProperty.Set.iter (fun ar ->
									try assert (List.memq ed (ObjectProperty.Map.find ar ed.is_parent.childreni));
									with Not_found -> assert false;
						) processed_rolesi;
					(* for every processed role: 1. the edge [ed] should be added as *)
					(* a parent in the child implication set for this role, and 2.   *)
					(* there is no other sibling child edges of the parent           *)
					(* implication set                                               *)
					ObjectProperty.Set.iter (fun ar ->
									assert (IS.mem ed.id (ObjectProperty.Map.find ar ed.is_child.parents));
									iter_sibling_child_edges_r (fun sed -> assert (sed == ed);
										) index ed.is_parent (I.find_role_record index ar) true;
						) processed_roles;
					ObjectProperty.Set.iter (fun ar ->
									assert (IS.mem ed.id (ObjectProperty.Map.find ar ed.is_child.parentsi));
									iter_sibling_child_edges_r (fun sed -> assert (sed == ed);
										) index ed.is_parent (I.find_role_record index ar) false;
						) processed_rolesi;
					(* for all sibling child edges the root concepts have been       *)
					(* merged to the implied concepts                                *)
					iter_sibling_child_edges_ed (fun sed ->
									assert (ClassExpression.Set.for_all (ClassExpression.HSet.mem ed.is_parent.implied) sed.is_child.core_pos);
									assert (ClassExpression.Set.for_all (ClassExpression.HSet.mem ed.is_parent.implied) sed.is_child.core_neg);
									assert (ClassExpression.Set.for_all (ClassExpression.HSet.mem ed.is_parent.implied) sed.rt_child_new_pos);
									assert (ClassExpression.Set.for_all (ClassExpression.HSet.mem ed.is_parent.implied) sed.rt_child_new_neg);
									assert (ObjectProperty.Set.is_subset
											(List.fold_left (fun s ar -> ObjectProperty.Set.remove ar s) sed.roles sed.roles_new)
											ed.rolesi);
									assert (ObjectProperty.Set.is_subset
											(List.fold_left (fun s ar -> ObjectProperty.Set.remove ar s) sed.rolesi sed.rolesi_new)
											ed.roles);
						) index ed.is_child processed_rolesi processed_roles;
		) t.edges;
;;

(* check that all implication sets and edges are processed *)
let check_processed_t t =
	HI.iter (fun is -> assert (is_processed_is is)) t.rt_to_is;
	HE.iter (fun ed ->
					assert (is_processed_ed_roles ed);
					assert (is_processed_ed_root ed);
		) t.edges
;;

(* live statistics *)
let root_count = ref 0
let max_root_size = ref 0
let total_root_size = ref 0
let total_implied = ref 0
let total_children = ref 0
let count_stack_is = ref 0
let count_stack_ed_roles = ref 0
let count_stack_ed_roots = ref 0
let update_interval = 0.05
let last_update_time = ref 0.0

let print_live_statistics () =
	if !root_count > 0 then (
		Printf.fprintf stderr "   cores:%6n size mx/av:%3n/%5.2f implied av:%6.2f children av:%6.2f stacks (is/roles/cores):%4n/%4n/%5n"
			!root_count
			!max_root_size
			(float_of_int !total_root_size /. float_of_int !root_count)
			(float_of_int !total_implied /. float_of_int !root_count)
			(float_of_int !total_children /. float_of_int !root_count)
			!count_stack_is
			!count_stack_ed_roles
			!count_stack_ed_roots
		;
	);
	Printf.fprintf stderr "\n";
;;

(**================== functions that DO modify the datastructures ====================*)

(* remebering to process new concepts of [is] *)
let to_process_is t pt_lst is =
	List.iter (fun pt -> pt.PT.back ()) pt_lst;
	incr count_stack_is;
	match t.current_is with
	| None -> t.current_is <- Some is
	| Some _ -> t.stack_is <- is :: t.stack_is
;;

(* remebering to process the new roles of [ed] *)
let to_process_ed_roles t ed =
	incr count_stack_ed_roles;
	match t.current_ed_roles with
	| None -> t.current_ed_roles <- Some ed
	| Some _ -> t.stack_ed_roles <- ed :: t.stack_ed_roles
;;

(* removes the child implication sets of edge [ed] and references from it  *)
(* to the parent edge [ed]                                                 *)
let ed_clear_child_is t ed =
	ObjectProperty.Map.iter_s (fun ar ed_id_set ->
					let ed_id_set_new = IS.remove ed.id ed_id_set in
					ed.is_child.parents <- if IS.is_empty ed_id_set_new
					then ObjectProperty.Map.remove ar ed.is_child.parents
					else ObjectProperty.Map.replace ar ed_id_set_new ed.is_child.parents
		) ed.is_child.parents ed.roles;
	ObjectProperty.Map.iter_s (fun ar ed_id_set ->
					let ed_id_set_new = IS.remove ed.id ed_id_set in
					ed.is_child.parentsi <- if IS.is_empty ed_id_set_new
					then ObjectProperty.Map.remove ar ed.is_child.parentsi
					else ObjectProperty.Map.replace ar ed_id_set_new ed.is_child.parentsi
		) ed.is_child.parentsi ed.rolesi;
;;

(* remebering to assign the implication set to the child root of [ed] *)
let to_process_ed_root t ed =
	(*|  ed_clear_child_is t ed;*)
	incr count_stack_ed_roots;
	match t.current_ed_root with
	| None -> t.current_ed_root <- Some ed
	| Some _ -> t.stack_ed_root <- ed :: t.stack_ed_root

(* completely remove all references to [ed] *)
let purge_edge t ed =
	(* remove references to [ed] from [ed.is_child] *)
	ed_clear_child_is t ed;
	(* removes references to [ed] from [ed.is_parent] *)
	let rec rm_lst e = function
		| [] -> []
		| hd :: tl -> if hd == e then tl else hd :: (rm_lst e tl)
	in
	ObjectProperty.Map.iter_s (fun ar ed_lst ->
					let ed_lst_new = rm_lst ed ed_lst in
					ed.is_parent.children <- if ed_lst_new == []
					then ObjectProperty.Map.remove ar ed.is_parent.children
					else ObjectProperty.Map.replace ar ed_lst_new ed.is_parent.children
		) ed.is_parent.children ed.roles;
	ObjectProperty.Map.iter_s (fun ar ed_lst ->
					let ed_lst_new = rm_lst ed ed_lst in
					ed.is_parent.childreni <- if ed_lst_new == []
					then ObjectProperty.Map.remove ar ed.is_parent.childreni
					else ObjectProperty.Map.replace ar ed_lst_new ed.is_parent.childreni
		) ed.is_parent.childreni ed.roles;
	(* removing from the hash table *)
	decr total_children;
	HE.remove t.edges ed.id;
	ed.id <- 0; (** to remember not to process this edge in the stacks! *)
;;

let ed_is_deleted ed = ed.id == 0
;;

(* the function for creating / finding an implication set for the root     *)
(* [rt]                                                                    *)

(* add positive concept [c] to the stack of set [is] *)
let is_add_implied_pos t pt_lst is c =
	if not (ClassExpression.HSet.mem is.implied c) then begin
		ClassExpression.HSet.add is.implied c;
		if is_processed_is is then to_process_is t pt_lst is;
		is.implied_new_pos <- c :: is.implied_new_pos;
		incr total_implied;
	end
;;

let is_union_implied_pos t pt_lst is s =
	ClassExpression.Set.iter (fun c -> is_add_implied_pos t pt_lst is c) s;
;;

(* add negative concept [c] to the stack of [is] *)
let is_add_implied_neg t pt_lst is c =
	if not (ClassExpression.HSet.mem is.implied c) then	begin
		ClassExpression.HSet.add is.implied c;
		if is_processed_is is then to_process_is t pt_lst is;
		is.implied_new_neg <- c :: is.implied_new_neg;
		incr total_implied;
	end
;;
let is_union_implied_neg t pt_lst is s =
	ClassExpression.Set.iter (is_add_implied_neg t pt_lst is) s
;;

(* adding new roles to edges *)

(* adds a role [r = (ar, ar_dir)] to the edge [ed] *)
let ed_add_role t ed ar = function
	| true -> if not (ObjectProperty.Set.mem ar ed.roles) then (
				if is_processed_ed_roles ed then to_process_ed_roles t ed;
				ed.roles <- ObjectProperty.Set.add ar ed.roles;
				ed.roles_new <- ar :: ed.roles_new;
			)
	| false -> if not (ObjectProperty.Set.mem ar ed.rolesi) then (
				if is_processed_ed_roles ed then to_process_ed_roles t ed;
				ed.rolesi <- ObjectProperty.Set.add ar ed.rolesi;
				ed.rolesi_new <- ar :: ed.rolesi_new;
			)

let ed_union_roles t ed roles rolesi =
	ObjectProperty.Set.iter (fun ar -> ed_add_role t ed ar true) roles;
	ObjectProperty.Set.iter (fun ar -> ed_add_role t ed ar false) rolesi;
;;

let init_is t pt_lst core_pos core_neg =
	let is = {
		core_pos = core_pos;
		core_neg = core_neg;
		implied = ClassExpression.HSet.create (2 * (
						ClassExpression.Set.cardinal core_pos +
						ClassExpression.Set.cardinal core_neg
					));
		implied_new_pos = [];
		implied_new_neg = [];
		children = ObjectProperty.Map.empty;
		childreni = ObjectProperty.Map.empty;
		parents = ObjectProperty.Map.empty;
		parentsi = ObjectProperty.Map.empty;
	} in
	is_union_implied_neg t pt_lst is core_neg;
	is_union_implied_pos t pt_lst is core_pos;
	is
;;

let rt_assign_is t pt_lst core_pos core_neg =
	(* --- some statistics comment out --- *)
	(*|  incr root_count;                                     *)
	(*|  let root_size =                                      *)
	(*|    ClassExpression.Set.cardinal core_pos +            *)
	(*|    ClassExpression.Set.cardinal core_neg              *)
	(*|  in                                                   *)
	(*|  if root_size > !max_root_size then (                 *)
	(*|    max_root_size := root_size;                        *)
	(*|  );                                                   *)
	(*|  total_root_size := root_size + !total_root_size;     *)
	(*|  let time = Unix.gettimeofday () in                   *)
	(*|  if time > !last_update_time +. update_interval then ( *)
	(*|    last_update_time := time;                          *)
	(*|    Printf.fprintf stderr "\n";                        *)
	(*|    print_live_statistics ();                          *)
	(*|    Printf.fprintf stderr "\027[2A\027[31C";           *)
	(*|  );                                                   *)
	(* printing large roots to find out what is happening *)
	(*|  if root_size = !max_root_size then (                                                                 *)
	(*|    ClassExpression.Set.iter (fun c -> Printf.fprintf stderr "%s " (Krss.str_of_concept c)) (R.pos rt);*)
	(*|    Printf.fprintf stderr "> ";                                                                        *)
	(*|    ClassExpression.Set.iter (fun c -> Printf.fprintf stderr "%s " (Krss.str_of_concept c)) (R.neg rt);*)
	(*|    Printf.fprintf stderr "\n";                                                                        *)
	(*|  );                                                                                                   *)
	(* --- statistics ends --- *)
	let is = init_is t pt_lst core_pos core_neg in
	begin match t.top with
		| Some c -> is_add_implied_neg t pt_lst is c;
		| _ -> ();
	end;
	HI.add t.rt_to_is is;
	List.iter (fun pt -> pt.PT.incr_max ()) pt_lst;
	List.iter (fun pt -> pt.PT.step ()) pt_lst;
	is
;;

let rt_cons t pt_lst core_pos core_neg =
	try HI.find t.rt_to_is (core_pos, core_neg)
	with Not_found ->
			let is = rt_assign_is t pt_lst core_pos core_neg
			in is
;;

let empty_is t pt_lst =
	rt_cons t pt_lst ClassExpression.Set.empty ClassExpression.Set.empty

let init_fresh_edge t pt_lst is c =
	incr total_children;
	t.edge_count <- succ t.edge_count;
	let ed =
		{ id = t.edge_count;
			is_parent = is;
			rt_child_new_pos = ClassExpression.Set.empty;
			rt_child_new_neg = ClassExpression.Set.empty;
			is_child = rt_cons t pt_lst
					(ClassExpression.Set.singleton c) ClassExpression.Set.empty;
			roles = ObjectProperty.Set.empty;
			rolesi = ObjectProperty.Set.empty;
			roles_new = [];
			rolesi_new = [];
		} in
	HE.add t.edges ed;
	ed
;;

(** applying the backward rules when a role or a concept is added *)
(*|----- Inference Rule:------|*)
(*| [ch -> a, (pt r ch), a -> (all (inv r) b) ==> pt -> b] *)

(* when a concept with [concept_record] is added to an implication set     *)
(* [ch_is]                                                                 *)
let is_propagate_backward t pt_lst ch_is concept_record =
	let iter_parents f =
		ObjectProperty.Map.iter2 f ch_is.parents concept_record.I.c_succi;
		ObjectProperty.Map.iter2 f ch_is.parentsi concept_record.I.c_succ;
	in
	iter_parents ( fun _ ed_id_set (c_pos, c_neg) ->
					IS.iter (fun ed_id ->
									let is = (HE.find t.edges ed_id).is_parent in
									is_union_implied_pos t pt_lst is c_pos;
									is_union_implied_neg t pt_lst is c_neg;
						) ed_id_set
		);
;;

(* iterating over common keys of hashset and hasmap *)
let iter_hm_hs f h s =
	let l = ref [] in
	if ClassExpression.HMap.length h > ClassExpression.HSet.length s then
		ClassExpression.HSet.iter (fun x ->
						try let y = ClassExpression.HMap.find h x in
							l := (x, y) :: !l
						with Not_found -> ()
			) s
	else
		ClassExpression.HMap.iter (fun x y ->
						if ClassExpression.HSet.mem s x then
							l := (x, y) :: !l
			) h;
	List.iter (fun (x, y) -> f x y) !l;
;;

(* when a role with [role_record] and [role_dir] is added to the ede [ed] *)
let ed_propagate_backward_r t pt_lst ed role_record role_dir =
	iter_hm_hs ( fun _ (c_pos, c_neg) ->
					is_union_implied_pos t pt_lst ed.is_parent c_pos;
					is_union_implied_neg t pt_lst ed.is_parent c_neg;
		)
		(if role_dir then role_record.I.r_succi else role_record.I.r_succ)
		ed.is_child.implied;
;;

(* when a concept with [concept_record] is added to the root of the edge   *)
(* [ed]                                                                    *)
let ed_propagate_backward_c t pt_lst ed concept_record =
	let iter_roles f =
		ObjectProperty.Map.iter_s f concept_record.I.c_succi ed.roles ;
		ObjectProperty.Map.iter_s f concept_record.I.c_succ ed.rolesi;
	in
	iter_roles ( fun _ (c_pos, c_neg) ->
					is_union_implied_pos t pt_lst ed.is_parent c_pos;
					is_union_implied_neg t pt_lst ed.is_parent c_neg;
		);
;;

(* extending the root of the child implication set of [ed] with positive   *)
(* concepts [c_pos] and negative concepts [c_neg]                          *)
let ed_extend_root t pt_lst index ed c_pos c_neg =
	let implied = ed.is_child.implied in
	(*|  let c_pos = ClassExpression.Set.diff c_pos implied in*)
	let c_pos = ClassExpression.Set.filter (fun c -> not (ClassExpression.HSet.mem implied c)) c_pos in
	let c_pos = ClassExpression.Set.diff c_pos c_neg in
	let c_pos = ClassExpression.Set.diff c_pos ed.is_child.core_pos in
	let c_pos = ClassExpression.Set.diff c_pos ed.is_child.core_neg in
	let c_pos = ClassExpression.Set.diff c_pos ed.rt_child_new_pos in
	let c_pos = ClassExpression.Set.diff c_pos ed.rt_child_new_neg in
	(*|  let c_neg = ClassExpression.Set.diff c_neg implied in*)
	let c_neg = ClassExpression.Set.filter (fun c -> not (ClassExpression.HSet.mem implied c)) c_neg in
	let c_neg = ClassExpression.Set.diff c_neg ed.is_child.core_neg in
	let c_neg = ClassExpression.Set.diff c_neg ed.rt_child_new_neg in
	if not (ClassExpression.Set.is_empty c_pos && ClassExpression.Set.is_empty c_neg) then (
		if is_processed_ed_root ed then to_process_ed_root t ed;
		ed.rt_child_new_pos <- ClassExpression.Set.union c_pos ed.rt_child_new_pos;
		ed.rt_child_new_neg <- ClassExpression.Set.union c_neg ed.rt_child_new_neg;
		(*|    ed.is_child_op <- RT (R.extend c_pos c_neg ch_rt);*)
		(* inserting new concepts to sibling parents *)
		iter_isibling_parent_edges (fun sed ->
						is_union_implied_neg t pt_lst sed.is_parent c_neg;
						is_union_implied_pos t pt_lst sed.is_parent c_pos;
			) t index ed.is_parent ed.roles ed.rolesi;
		(* applying the backward rules *)
		let iter_concepts f =
			(ClassExpression.Set.iter f c_pos; ClassExpression.Set.iter f c_neg)
		in iter_concepts (fun c ->
						let c_cr = I.find_concept_record index c in
						ed_propagate_backward_c t pt_lst ed c_cr
			); (* iter_concepts *)
	)
;;

(** applying the forward rules when a role or a concept is added *)
(*|----- Inference Rule:------|*)
(*| [pt -> a, (pt r ch), a -> (all r b) ==> ch -> b] *)

let is_propagate_forward t pt_lst index pt_is concept_record =
	let iter_children f =
		ObjectProperty.Map.iter2 f pt_is.children concept_record.I.c_succ;
		ObjectProperty.Map.iter2 f pt_is.childreni concept_record.I.c_succi;
	in
	iter_children ( fun _ ed_id_list (c_pos, c_neg) ->
					List.iter (fun ed ->
									ed_extend_root t pt_lst index ed c_pos c_neg
						) ed_id_list
		);
;;

let ed_propagate_forward_r t pt_lst index ed role_record role_dir =
	iter_hm_hs ( fun _ (c_pos, c_neg) ->
					ed_extend_root t pt_lst index ed c_pos c_neg
		) (if role_dir then role_record.I.r_succ else role_record.I.r_succi)
		ed.is_parent.implied
;;

(**===================== processing of stacks ==========================**)

(* finding the next implication set to be processed *)
let pop_stack_is t pt_lst =
	(* incrementing the progress bar *)
	List.iter (fun pt -> pt.PT.step ()) pt_lst;
	decr count_stack_is;
	match t.stack_is with
	| is :: rest ->
			t.stack_is <- rest;
			t.current_is <- Some is;
	| [] ->	t.current_is <- None
;;

(* finding the next edge to be processed for roles *)
let pop_stack_ed_roles t =
	decr count_stack_ed_roles;
	match t.stack_ed_roles with
	| ed :: rest ->
			t.stack_ed_roles <- rest;
			t.current_ed_roles <- Some ed;
	| [] -> t.current_ed_roles <- None
;;

(* finding the next edge to be processed for root *)
let pop_stack_ed_root t =
	decr count_stack_ed_roots;
	match t.stack_ed_root with
	| ed :: rest ->
			t.stack_ed_root <- rest;
			t.current_ed_root <- Some ed;
	| [] -> t.current_ed_root <- None
;;

(* === a function for processing positive implied concepts *)
let process_is_implied_new_pos t pt_lst index is ce =
	let module C = ClassExpression_Constructor in
	match ce.data with
	| C.Class c ->
			begin match c with
				| Class_Constructor.Nothing ->
				(* removing everything from [is] except for [ce = bottom] *)
						ClassExpression.HSet.clear is.implied;
						ClassExpression.HSet.add is.implied ce;
						incr total_implied;
						is.implied_new_pos <- [];
						is.implied_new_neg <- [ce];
						(* removing all edges *)
						ObjectProperty.Map.iter (fun ar ed_lst ->
										List.iter (fun ed -> if HE.mem t.edges ed.id
														then purge_edge t ed) ed_lst;
							) is.children;
						ObjectProperty.Map.iter (fun ar ed_lst ->
										List.iter (fun ed -> if HE.mem t.edges ed.id
														then purge_edge t ed) ed_lst;
							) is.childreni;
						is.children <- ObjectProperty.Map.empty;
						is.childreni <- ObjectProperty.Map.empty;
				| _ -> ()
			end
	| C.ObjectIntersectionOf (c1, c2) | C.ObjectIntersectionOfrec (c1, c2) ->
			is_add_implied_pos t pt_lst is c1;
			is_add_implied_pos t pt_lst is c2;
	| C.ObjectSomeValuesFrom (ope, ce) ->
			let ar, ar_dir = Brole.to_elt ope in
			(* create a fresh edge for the successor *)
			let ed = init_fresh_edge t pt_lst is ce in
			(* add role [op] to the edge *)
			ed_add_role t ed ar ar_dir;
	(* add [ce] to the root of [ed] *)
	| _ -> ()   (** to be extended for new constructors *)
;;

(* === a function for processing negative implied concepts *)

let process_is_implied_new_neg t pt_lst index is c =
	let c_cr = I.find_concept_record index c in
	(* checking concept implications *)
	is_union_implied_pos t pt_lst is c_cr.I.c_impl;
	(* checking inferences producing conjunctions *)
	iter_hm_hs (fun _ s -> is_union_implied_neg t pt_lst is s)
		c_cr.I.c_conj is.implied;
	(* apply propagation rules for roles *)
	is_propagate_backward t pt_lst is c_cr;
	is_propagate_forward t pt_lst index is c_cr;
;;

let process_is t pt_lst index is =
	match is.implied_new_pos with
	| c :: rest ->
			is.implied_new_pos <- rest;
			is.implied_new_neg <- c :: is.implied_new_neg;
			process_is_implied_new_pos t pt_lst index is c;  (* tail recursive! *)
	| [] -> match is.implied_new_neg with
			| c :: rest ->
					is.implied_new_neg <- rest;
					if is_processed_is is then pop_stack_is t pt_lst;
					process_is_implied_new_neg t pt_lst index is c;  (* tail recursive! *)
			| [] -> failwith "[is] was already processed!"
;;

(* processes a new role [ar,ar_dir] of the edge [ed] *)
let process_edge_new_role t pt_lst index ed ar ar_dir =
	let ar_rr = I.find_role_record index ar in
	(* merging sibling edges of [ar, ar_dir] into [ed] *)
	iter_sibling_child_edges_r (fun sed ->
					if sed.id != ed.id then (
						(* add concepts in the root of [sed] to the root of [ed] *)
						ed_extend_root t pt_lst index ed sed.is_child.core_pos sed.is_child.core_neg;
						ed_extend_root t pt_lst index ed (ed_get_rt_child_new_pos sed) (ed_get_rt_child_new_neg sed);
						(* ading roles, removing children *)
						ObjectProperty.Set.iter (fun ar -> ed_add_role t ed ar true) sed.roles;
						ObjectProperty.Set.iter (fun ar -> ed_add_role t ed ar false) sed.rolesi;
						(* removing [sed] *)
						purge_edge t sed;
					) (* close if *)
		) index ed.is_parent ar_rr ar_dir;
	(* assigning the child of [ed.is_parent] *)
	if role_is_functional ar_rr ar_dir then
		if ar_dir then ed.is_parent.children <- ObjectProperty.Map.add ar [ed] ed.is_parent.children
		else ed.is_parent.childreni <- ObjectProperty.Map.add ar [ed] ed.is_parent.childreni
	else if ar_dir then
		ed.is_parent.children <- ObjectProperty.Map.process ar (function
				| None -> Some [ed] | Some ed_lst -> Some (ed :: ed_lst)
			) ed.is_parent.children
	else
		ed.is_parent.childreni <- ObjectProperty.Map.process ar (function
				| None -> Some [ed] | Some ed_lst -> Some (ed :: ed_lst)
			) ed.is_parent.childreni;
	(* assign the parent to the child is if there any *)
	if ar_dir then
		ed.is_child.parents <- ObjectProperty.Map.process ar (function
				| None -> Some (IS.singleton ed.id) | Some s -> Some (IS.add ed.id s)
			) ed.is_child.parents
	else ed.is_child.parentsi <- ObjectProperty.Map.process ar (function
				| None -> Some (IS.singleton ed.id) | Some s -> Some (IS.add ed.id s)
			) ed.is_child.parentsi;
	(* extend the implied set with the roots of children *)
	iter_sibling_child_edges_r (fun sed ->
			(* inserting new concepts *)
					is_union_implied_pos t pt_lst ed.is_parent sed.is_child.core_pos;
					is_union_implied_neg t pt_lst ed.is_parent sed.is_child.core_neg;
					is_union_implied_pos t pt_lst ed.is_parent (ed_get_rt_child_new_pos sed);
					is_union_implied_neg t pt_lst ed.is_parent (ed_get_rt_child_new_neg sed);
					(* inserting new roles *)
					ed_union_roles t ed sed.rolesi sed.roles
		) index ed.is_child ar_rr (not ar_dir);
	(* apply forward and backward rules for the new edge *)
	ed_propagate_backward_r t pt_lst ed ar_rr ar_dir;
	ed_propagate_forward_r t pt_lst index ed ar_rr ar_dir;
	(* extend the sibling parents with new roles *)
	iter_isibling_parent_edges (fun sed ->
			(* inserting new concepts *)
					is_union_implied_pos t pt_lst sed.is_parent ed.is_child.core_pos;
					is_union_implied_neg t pt_lst sed.is_parent ed.is_child.core_neg;
					is_union_implied_pos t pt_lst sed.is_parent (ed_get_rt_child_new_pos ed);
					is_union_implied_neg t pt_lst sed.is_parent (ed_get_rt_child_new_neg ed);
					(* inserting new roles *)
					ed_union_roles t sed ed.rolesi ed.roles
		) t index ed.is_parent ed.roles ed.rolesi;
;;

let process_edge_new_roles t pt_lst index ed =
	
	match ed.roles_new with
	| ar :: rest ->
			ed.roles_new <- rest;
			if is_processed_ed_roles ed then pop_stack_ed_roles t;
			process_edge_new_role t pt_lst index ed ar true;
	| [] ->
			match ed.rolesi_new with
			| ar :: rest ->
					ed.rolesi_new <- rest;
					if is_processed_ed_roles ed then pop_stack_ed_roles t;
					process_edge_new_role t pt_lst index ed ar false;
			| [] -> failwith "the roles of [ed] were already processed!"
;;

(* process the root [ed.rt_child] of the edge [ed] by assigning an         *)
(* implication set to [ed.is_child_op]                                     *)
let process_edge_root t pt_lst index ed =
	(* finding the next edge to be processed *)
	pop_stack_ed_root t;
	ed_clear_child_is t ed;
	(* we need to find a suitable implication set for the child root *)
	let new_core_pos = ClassExpression.Set.union ed.is_child.core_pos
			(ed_get_rt_child_new_pos ed) in
	let new_core_neg = ClassExpression.Set.union ed.is_child.core_neg
			(ed_get_rt_child_new_neg ed) in
	let new_core_pos = ClassExpression.Set.diff new_core_pos new_core_neg in
	let ch_is = try HI.find t.rt_to_is (new_core_pos, new_core_neg)
		with Not_found -> rt_assign_is t pt_lst new_core_pos new_core_neg
	in
	ed.is_child <- ch_is;
	ed.rt_child_new_pos <- ClassExpression.Set.empty;
	ed.rt_child_new_neg <- ClassExpression.Set.empty;
	(* it remains to extend the parents of [ch_is] and apply the backward    *)
	(* propagation rules                                                     *)
	let iter_roles f =
		ObjectProperty.Set.iter (fun ar -> f ar true) ed.roles;
		ObjectProperty.Set.iter (fun ar -> f ar false) ed.rolesi;
	in
	iter_roles (fun ar ar_dir ->
			(* creating parent edges for [ed.id] in [ch_is] *)
					if ar_dir then ch_is.parents <- ObjectProperty.Map.process ar (function
								| None -> Some (IS.singleton ed.id) | Some s -> Some (IS.add ed.id s)
							) ch_is.parents
					else ch_is.parentsi <- ObjectProperty.Map.process ar (function
								| None -> Some (IS.singleton ed.id) | Some s -> Some (IS.add ed.id s)
							) ch_is.parentsi;
					let ar_rr = I.find_role_record index ar in
					ed_propagate_backward_r t pt_lst ed ar_rr ar_dir;
		);
	(* extending roles and concepts from sibling child edges *)
	iter_sibling_child_edges_ed (fun sed ->
			(* inserting new concepts *)
					is_union_implied_pos t pt_lst ed.is_parent sed.is_child.core_pos;
					is_union_implied_neg t pt_lst ed.is_parent sed.is_child.core_neg;
					is_union_implied_pos t pt_lst ed.is_parent sed.rt_child_new_pos;
					is_union_implied_neg t pt_lst ed.is_parent sed.rt_child_new_neg;
					(* inserting new roles *)
					ed_union_roles t ed sed.rolesi sed.roles
		) index ch_is ed.rolesi ed.roles;
;;

(** sanity checking: periodically check datastructure invariants *)
(* current check attempt *)
let check_attempt = ref 0
(* perform checking every [check_interval] attempts *)
let check_interval = 100

let check_integrity t pt_lst index =
	incr check_attempt;
	if !check_attempt = check_interval then begin
		check_attempt := 0;
		check_implication_sets t;
		check_edges t index;
	end
;;

(* the main function for recursively processing the datastructure *)
let rec process_t t pt_lst index =
	(* precedence on currents: [t.current_is] > [t.current_ed_roles] >       *)
	(* [t.current_ed_root]                                                   *)
	(** for sanity checking uncomment the following *)
	(*|  check_integrity t index;*)
	if !count_stack_is > !count_stack_ed_roles then
		match t.current_is with
		| Some is ->
				assert (not (is_processed_is is));
				process_is t pt_lst index is;
				process_t t pt_lst index (* tail recursive! *)
		| None -> invalid_arg "process_t"
	else if !count_stack_ed_roles > 0 then
		match t.current_ed_roles with
		| Some ed ->
				assert (not (is_processed_ed_roles ed));
				if ed_is_deleted ed then
					pop_stack_ed_roles t
				else process_edge_new_roles t pt_lst index ed;
				process_t t pt_lst index (* tail recursive! *)
		| None -> invalid_arg "process_t"
	else
		match t.current_ed_root with
		| Some ed ->
				assert (not (is_processed_ed_root ed));
				if ed_is_deleted ed then
					pop_stack_ed_root t
				else process_edge_root t pt_lst index ed;
				process_t t pt_lst index (* tail recursive! *)
		| None -> () (* done! *)
;;

(**======================= the main function =================================*)

let compute ?(message = "Saturating ontology...") pt_lst ont =
	let max = O.count_Class ont in	
	List.iter (fun pt -> pt.PT.start message max) pt_lst;
	let t = t_create (O.count_Class ont) in
	let index = I.init ont in
	(* Gc.compact (); *)
	
	if O.has_negative_Thing ont then (
		(* negative top means there could be a global inconsistency *)
		List.iter (fun pt -> pt.PT.incr_max ()) pt_lst;
		let top = ClassExpression.cons
				(ClassExpression_Constructor.Class Class_Constructor.Thing) in
		t.top <- Some top;
		let is = rt_cons t pt_lst (ClassExpression.Set.singleton top) ClassExpression.Set.empty
		in process_is t pt_lst index is;
		(*|    is.required <- true;*)
		process_t t pt_lst index;
		List.iter (fun pt -> pt.PT.step ()) pt_lst;
	);
	O.iter_record_Class ( fun ac _ ->
					let c = ClassExpression.cons (ClassExpression_Constructor.Class ac) in
					let _ = rt_cons t pt_lst (ClassExpression.Set.singleton c) ClassExpression.Set.empty in
					(*|          is.required <- true;*)
					process_t t pt_lst index;
					(* incrementing the progress bar *)
					List.iter (fun pt -> pt.PT.step ()) pt_lst;
		) ont;
	List.iter (fun pt -> pt.PT.finish ()) pt_lst;
	(*| print_live_statistics ();*)
	(*|  HI.print_stats ();*)
	(*|  HE.print_stats ();*)
	(* Gc.compact (); *) (* <- slow but useful in the long run *)
	(*|  print_t t;  *)
	(** for sanity checking : uncomment the following *)
	(*|  check_implication_sets t;*)
	(*|  check_edges t index;     *)
	(*|  check_processed_t t;     *)
	t
;;