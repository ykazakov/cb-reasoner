(** computation and printing of the concept taxonomy *)

open Owl
open Consed
module O = Ontology
module R = Saturation
module H = ClassExpression.HMap
module C = Class_Constructor
module CE = ClassExpression_Constructor
module F = Format
module HS = ClassExpression.HSet
module CS = ClassExpression.Set
module PT = Progress_tracker

(* we will use atomic concept sets for sorting of atomic concepts *)
module S = O.OrderedClass.Set
module M = O.OrderedClass.Map

(* there is some kind of bug of native compiler when two empty arraysq are *)
(* not physically equal                                                    *)

let empty_array = [||]

(* nodes *)
module Node = struct
	type t = {
		(* equivalent classes *)
		mutable classes : ClassExpression.t array;
		(* direct subclasses *)
		mutable child_nodes : t array;
		(* direct superclasses *)
		mutable parent_nodes : t array;
		(* auxiliary counter for subclasses *)
		mutable child_nodes_count : int;
	}
	let create () = {
		classes = empty_array;
		child_nodes = empty_array;
		parent_nodes = empty_array;
		child_nodes_count = 0;
	}
	let get_classes node = node.classes
	let get_child_nodes node = node.child_nodes
	let get_parent_nodes node = node.parent_nodes
end

(* taxonomy type *)
type t = {
	(* array of all nodes *)
	mutable nodes : Node.t array;
	(* assignment of classes to nodes *)
	map : Node.t H.t;
}

let find_node t a = H.find t.map a

(* find node if it exists and create new if it doesnt *)
let get_node t a =
	try H.find t.map a
	with Not_found ->
			let node = Node.create () in
			H.add t.map a node;
			node

let get_nodes t = t.nodes

let set_node t a node =
	H.replace t.map a node

(* initialize taxonomy with the number of classes + top + bottom *)
let init ont = {
	nodes = empty_array;
	map = H.create (O.count_Class ont + 2)
}

(* lexicographic comparison of classes *)
let lex_compare ce de = match ce.data, de.data with
	| CE.Class c, CE.Class d ->
			String.compare (Class.str_of c) (Class.str_of d)
	| _ -> invalid_arg "lex_compare"

(*| [iter_a_eq_di pt_lst f cimp ont] iterates [f] over all tuples *)
(*| [a] [a_eq_lst] [a_eq_cnt] [a_di_lst] [a_di_cnt]      *)
(*| where:                                                                  *)
(*| - [a] is the representative class (minimal element) in its *)
(*| equivalent class *)
(*| - [a_eq_lst] is the sorted list of other equivalent classes to [a] *)
(*| - [a_eq_cnt] is the length of [a_eq_lst] *)
(*| - [a_di_lst] is the sorted list of directly implied class representatives *)
(*| (except for [a = owl:Nothing]) *)
(*| - [a_di_cnt] is the length of [a_di_lst] *)
(* all minimal elements and order of elements in the lists are according *)
(* to the lexicographical ordering; iteration over tuples starts with [a] = [top],*)
(* finishes with [a] = [bot] (if ontology is consistent) and otherwise in the*)
(* lexicographical ordering for [a] *)
let iter_a_eq_di pt_lst f cimp ont =
	let top = ClassExpression.cons (CE.Class C.Thing) in
	let bot = ClassExpression.cons (CE.Class C.Nothing) in
	(* concepts equivalent to bottom are collected here *)
	let bot_equiv = ref [] in
	let bot_equiv_cnt = ref 0 in
	(* concepts equivalent to top are collected here *)
	let top_equiv = ref [] in
	let top_equiv_cnt = ref 0 in
	let top_implied =
		match R.find_option_top cimp with
		| None -> HS.create 0
		| Some top -> R.find_implied cimp top
	in
	HS.iter ( fun de -> if de != top then
						match de.data with
						| CE.Class d ->
								begin match d with
									| C.IRI _ ->
											top_equiv := de :: !top_equiv;
											incr top_equiv_cnt;
									| C.Nothing ->
											bot_equiv := top :: !bot_equiv;
											incr bot_equiv_cnt;
									| C.Thing -> ()
								end
						| _ -> ()
		) top_implied;
	(* if [!bot_equiv] is not empty, it contains [top] *)
	if !bot_equiv != [] then begin
		(* in this case the ontology is inconsistent *)
		bot_equiv := [];
		bot_equiv_cnt := 0;
		O.iter_record_Class ( fun c _ ->
						begin match c with
							| C.Thing -> ()
							| C.Nothing -> ()
							| C.IRI _ ->
									let ce = ClassExpression.cons (CE.Class c) in
									bot_equiv := ce :: !bot_equiv;
									incr bot_equiv_cnt;
						end
			) ont;
		bot_equiv := bot :: !bot_equiv;
		incr bot_equiv_cnt;
		f top (List.rev !bot_equiv) !bot_equiv_cnt [] 0;
		PT.finish pt_lst;
	end else begin
		(* apply [f] for [top] *)
		top_equiv := List.sort lex_compare !top_equiv;
		f top !top_equiv !top_equiv_cnt [] 0;
		PT.step pt_lst;
		(* process classes *)
		O.iter_record_Class ( fun c _ ->
						begin match c with
							| C.Thing -> ()
							| C.Nothing -> ()
							| C.IRI _ ->
									let ce = ClassExpression.cons (CE.Class c) in
									if not (HS.mem top_implied ce) then begin
										(* list of all implied classes for [ce] with their     *)
										(* implications                                        *)
										let ce_impl_impl_lst = ref [] in
										(* computing the list of equivalent classes for [ce] *)
										let ce_equiv_lst = ref [] in
										let ce_equiv_cnt = ref 0 in
										HS.iter ( fun de -> if de != ce then
															match de.data with
															| CE.Class d ->
																	begin match d with
																		| C.IRI _ ->
																				if not (HS.mem top_implied de) then
																					let de_implied = R.find_implied cimp de in
																					if HS.mem de_implied ce then begin
																						ce_equiv_lst := de :: !ce_equiv_lst;
																						incr ce_equiv_cnt;
																					end else
																						ce_impl_impl_lst :=
																						(de, de_implied) :: !ce_impl_impl_lst
																		| C.Nothing ->
																				bot_equiv := ce :: !bot_equiv
																		| C.Thing -> ()
																	end
															| _ -> () (* collect only classes *)
											)	(R.find_implied cimp ce);
										(* check if [ce] is equivalent to [bot] *)
										match !bot_equiv with
										| de :: _ when de == ce -> ()
										| _ ->
										(* sort equivalencies *)
												ce_equiv_lst := List.sort lex_compare !ce_equiv_lst;
												(* check if [ce] is smaller than other elements in *)
												(* the equivalence class                           *)
												match !ce_equiv_lst with
												| de :: _ when (lex_compare ce de > 0) -> ()
												| _ ->
												(* to compute indirectly implied concepts modulo   *)
												(* equivalences it is important to process implied *)
												(* concpets in the alphabetical order              *)
														ce_impl_impl_lst := List.sort
															(fun (ce, _) (de, _) -> lex_compare ce de)
															!ce_impl_impl_lst;
														let indirect = ref CS.empty in
														let rec process accu = function
															| [] -> accu
															| (de, de_implied) :: tl ->
																	if CS.mem de !indirect then process accu tl
																	else begin
																		let check dde =
																			if not (CS.mem dde !indirect) &&
																			HS.mem de_implied dde then
																				indirect := CS.add dde !indirect
																		in
																		List.iter check accu;
																		List.iter (fun (dde, _) -> check dde) tl;
																		process (de :: accu) tl
																	end
														in
														let ce_impl_lst = ref (process [] !ce_impl_impl_lst) in
														(* remove the inderect elements while          *)
														(* reversing and counting                      *)
														let ce_impl_cnt = ref 0 in
														let rec filter accu = function
															| [] -> accu
															| ce :: tl ->
																	if CS.mem ce !indirect then filter accu tl
																	else begin
																		incr ce_impl_cnt;
																		filter (ce :: accu) tl
																	end
														in
														ce_impl_lst := filter [] !ce_impl_lst;
														f ce !ce_equiv_lst !ce_equiv_cnt !ce_impl_lst !ce_impl_cnt;
									end;
						end;
						(* incrementing the progress bar *)
						PT.step pt_lst;
			) ont;
		(* the elements in [bot_equiv] are in the reverse order *)
		bot_equiv := List.rev !bot_equiv;
		f bot !bot_equiv !bot_equiv_cnt [] 0;
		PT.step pt_lst;
		(* done *)
	end;
	PT.finish pt_lst;
;;

(* ===================== computing taxonomy ======================= *)

let compute ?(message = "Computing taxonomy...") pt_lst cimp ont =
	PT.start pt_lst message (O.count_Class ont + 2);
	let t = init ont in
	(* list and counts of nodes *)
	let node_list = ref [] in
	let node_count = ref 0 in
	let top = ClassExpression.cons (CE.Class C.Thing) in
	let bot = ClassExpression.cons (CE.Class C.Nothing) in
	(* phase 1 : creating nodes with superclasses; counting subclasses *)
	iter_a_eq_di pt_lst ( fun a a_eq_lst a_eq_cnt a_di_lst a_di_cnt ->
					let node = get_node t a in
					node_list := node :: !node_list;
					incr node_count;
					(* creating an array of equivalent classes including [a] and     *)
					(* assigning node to them in the taxonomy                        *)
					let classes = Array.create (succ a_eq_cnt) a in
					(* if [a = owl:Nothing] it should be at the end, and otherwise   *)
					(* in the beginning                                              *)
					let i = ref (if a == bot then 0 else 1) in
					List.iter (fun b ->
									classes.(!i) <- b;
									set_node t b node;
									incr i;
						) a_eq_lst;
					node.Node.classes <- classes;
					(* creating an array of superclasses from directly implied       *)
					(* concepts; if there are none its superclass is [top] unless    *)
					(* [a] is [top] or [bot]                                         *)
					let sups, sups_count =
						if (a_di_cnt = 0) && (a != top) && (a != bot) then [top], 1
						else a_di_lst, a_di_cnt
					in
					let parent_nodes = Array.create sups_count node in (* the elements will be later overwritten *)
					let i = ref 0 in
					List.iter (fun b ->
									let b_node = get_node t b in
									(* incrementing the counter for subclasses in [b_node]   *)
									(* becase [a] is a subclass of [b]                       *)
									b_node.Node.child_nodes_count <- succ b_node.Node.child_nodes_count;
									parent_nodes.(!i) <- b_node;
									incr i;
						) sups;
					assert (!i = Array.length parent_nodes);
					node.Node.parent_nodes <- parent_nodes;
		) cimp ont;
	(* phase 2 : computing direct subclasses and the array of nodes and also *)
	(* computing the parents of the bottom node                              *)
	let bot_node = get_node t bot in
	let nodes = Array.create !node_count bot_node in
	let bot_supnode_list = ref [] in
	let bot_supnode_count = ref 0 in
	List.iter (fun node ->
			(* nodes are coming in the reverse alphabetical order *)
					decr node_count;
					nodes.(!node_count) <- node;
					Array.iter (fun node_sup ->
									if node_sup.Node.child_nodes == empty_array then
										node_sup.Node.child_nodes <- Array.create node_sup.Node.child_nodes_count node;
									(* add [node] as a subclass of [node_sup] from the end   *)
									(* of the array because [!node_list] is processed in the *)
									(* reversed alphabetical order                           *)
									let i = pred node_sup.Node.child_nodes_count in
									node_sup.Node.child_nodes.(i) <- node;
									node_sup.Node.child_nodes_count <- i;
						) node.Node.parent_nodes;
					(* checking if the node is a parent of the bot node *)
					if (node.Node.child_nodes == empty_array) && (node.Node.child_nodes_count = 0) then begin
						bot_supnode_list := node :: !bot_supnode_list;
						incr bot_supnode_count;
						node.Node.child_nodes <- [| bot_node |]
					end;
		) !node_list;
	t.nodes <- nodes;
	(* phase 3: assigning parents of the bottom node *)
	let bot_parent_nodes = Array.create !bot_supnode_count bot_node in
	let i = ref 0 in
	List.iter (fun node ->
			(* nodes are coming in the alphabetical order *)
					bot_parent_nodes.(!i) <- node;
					incr i;
		) !bot_supnode_list;
	bot_node.Node.parent_nodes <- bot_parent_nodes;
	t