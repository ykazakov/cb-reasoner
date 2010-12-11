(** computation and printing of the concept taxonomy *)

open Owl2
open Consed.T
module O = Ontology
module R = ReasonerTBox
module H = Class.HMap
module C = Class_Constructor
module CE = ClassExpression_Constructor
module PB = ProgressBar
module F = Format
module HS = ClassExpression.HSet
module CS = ClassExpression.Set

(* we will use atomic concept sets for sorting of atomic concepts *)
module S = O.ClassSet
module M = O.ClassMap

(* taxonomy is stored as a hastable from classes to triples consisting of  *)
(* the set of equivalent classes, direct superclasses, and direct          *)
(* subclasses                                                              *)
type t = (S.t * S.t * S.t) H.t

(* lexicographic comparison of classes *)
let lex_compare ce de = match ce.data, de.data with
	| CE.Class c, CE.Class d ->
			String.compare (Class.str_of c) (Class.str_of d)
	| _ -> invalid_arg "lex_compare"

let init ont = H.create (O.total_ClassIRI ont)

(* Iterating over triples [a], [a_eq], [a_di] where [a] is a minimal       *)
(* element in its equvalent class [a_eq] and [a_di] are directly implied   *)
(* by [a] which are also minimal elements in their equivalent classes; All *)
(* minimal elements are according to the lexicographic ordering.           *)
let iter_a_eq_di f t ont =
	let top = O.cons_ClassExpression ont (CE.Class Class_Constructor.Thing) in
	let bot = O.cons_ClassExpression ont (CE.Class Class_Constructor.Nothing) in
	(* concepts equivalent to bottom are collected here *)
	let bot_equiv = ref [] in
	(* concepts equivalent to top are collected here *)
	let top_equiv = ref [] in
	let top_implied =
		match R.find_option_top t with
		| None -> HS.create 0
		| Some top -> R.find_implied t top
	in
	HS.iter ( fun de -> if de != top then
						match de.data with
						| CE.Class d ->
								begin match d with
									| Class_Constructor.IRI _ ->
											top_equiv := de :: !top_equiv
									| Class_Constructor.Nothing ->
											bot_equiv := top :: !bot_equiv
									| Class_Constructor.Thing -> ()
								end
						| _ -> ()
		) top_implied;
	if !bot_equiv != [] then begin
		(* then the ontology is inconsistent *)
		f top [bot] [];
		PB.finish ();
	end else begin
		(* apply [f] for [top] *)
		top_equiv := List.sort lex_compare !top_equiv;
		f top !top_equiv [];
		PB.step ();
		(* process classes *)
		O.iter_record_Class ( fun c _ ->
						begin match c with
							| C.Thing -> ()
							| C.Nothing -> ()
							| C.IRI _ ->
									let ce = O.cons_ClassExpression ont (CE.Class c) in
									if not (HS.mem top_implied ce) then begin
										(* list of all implied classes for [ce] with their     *)
										(* implications                                        *)
										let ce_impl_impl_lst = ref [] in
										(* computing the list of equivalent classes for [ce] *)
										let ce_equiv_lst = ref [] in
										HS.iter ( fun de -> if de != ce then
															match de.data with
															| CE.Class d ->
																	begin match d with
																		| Class_Constructor.IRI _ ->
																				if not (HS.mem top_implied de) then
																					let de_implied = R.find_implied t de in
																					if HS.mem de_implied ce then begin
																						ce_equiv_lst := de :: !ce_equiv_lst;
																					end else
																						ce_impl_impl_lst :=
																						(de, de_implied) :: !ce_impl_impl_lst
																		| Class_Constructor.Nothing ->
																				bot_equiv := ce :: !bot_equiv
																		| Class_Constructor.Thing -> ()
																	end
															| _ -> () (* collect only classes *)
											)	(R.find_implied t ce);
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
														(* reversing                                   *)
														let rec filter accu = function
															| [] -> accu
															| ce :: tl ->
																	if CS.mem ce !indirect then filter accu tl
																	else filter (ce :: accu) tl
														in
														ce_impl_lst := filter [] !ce_impl_lst;
														f ce !ce_equiv_lst !ce_impl_lst;
									end;
						end;
						(* incrementing the progress bar *)
						PB.step ();
			) ont;
		(* the elements in [bot_equiv] are in the reverse order *)
		bot_equiv := List.rev !bot_equiv;
		f bot !bot_equiv [];
		PB.step ();
		(* done *)
	end;
	PB.finish ();
;;

(* ================== formatting and printing ===================== *)

(** printing in functional style OWL 2 syntax *)

(* printing taxonomy *)
let print_fowl t ont out =
	let f = F.formatter_of_out_channel out in
	let print_declaration ce = match ce.data with
		| CE.Class c ->
				if c != C.Thing && c != C.Nothing then begin
					F.fprintf f "@;@[<hv 2>Declaration(@,@[<hv 2>Class(@,";
					Owl2IO.fprint_Class f c;
					F.fprintf f "@;<0 -2>)@]@;<0 -2>)@]";
				end
		| _ -> ()
	in
	let print_equivalent ce s =
		print_declaration ce;
		List.iter print_declaration s;
		if not (s = []) then begin
			F.fprintf f "@;@[<hv 2>EquivalentClasses(@,";
			Owl2IO.fprint_ClassExpression f ce;
			List.iter (fun be ->
							F.fprintf f "@ ";
							Owl2IO.fprint_ClassExpression f be;
				) s;
			F.fprintf f "@;<0 -2>)@]";
		end
	in
	PB.init (O.total_ClassIRI ont + 2);
	let tm = Unix.gmtime (Unix.gettimeofday ()) in
	F.fprintf f "@[<v 2>Ontology(<http://code.google.com/p/cb-reasoner/%n/%n/%n/taxonomy>"
		(1900 + tm.Unix.tm_year) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday;
	iter_a_eq_di ( fun a equiv_a dimpl_a ->
					print_equivalent a equiv_a;
					(* printing directly implied concepts *)
					List.iter (fun be ->
									F.fprintf f "@;@[<hv 2>SubClassOf(@,";
									Owl2IO.fprint_ClassExpression f a;
									F.fprintf f "@ ";
									Owl2IO.fprint_ClassExpression f be;
									F.fprintf f "@;<0 -2>)@]";
						) dimpl_a;
		) t ont;
	F.fprintf f "@;<0 -2>)@]%!";
;;

let print_statistics t ont out =
	let n = ref 0 in
	let _ =
		iter_a_eq_di ( fun a equiv_a dimpl_a ->
				(* counting equivalent concepts *)
						List.iter (fun _ ->	incr n) equiv_a;
						(* counting directly implied concepts *)
						List.iter (fun _ -> incr n ) dimpl_a;
			) t ont in
	Printf.fprintf out "%n concept names have totally %n equivalent or directly implied concepts, %f everage\n"
		(O.total_ClassIRI ont) !n
		((float_of_int !n) /. (float_of_int (O.total_ClassIRI ont)) )
