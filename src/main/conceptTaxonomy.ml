(** computation and printing of the concept taxonomy *)

open Owl2
open Consed.T
module O = Ontology
module R = ReasonerTBox
module H = Class.Hashtbl
module C = Class.Constructor
module CE = ClassExpression.Constructor
module PB = ProgressBar

(* we will use atomic concept sets for sorting of atomic concepts *)

module OrderedClass =
struct
  type t = Class.t
  let precedence = function
    | C.IRI _ -> 1
    | C.Thing -> 2
    | C.Nothing -> 3
  let compare c1 c2 =
    let c = Pervasives.compare (precedence c1.data) (precedence c2.data) in
    if c <> 0 then c
    else match c1.data, c2.data with
      | C.IRI iri1, C.IRI iri2 ->
          compare iri1 iri2
      | C.Thing, C.Thing -> 0
      | C.Nothing, C.Nothing -> 0
      | _ -> invalid_arg "OrderedClass.compare"
  let str = Owl2IO.str_of_Class
end

module S = Set.Make (OrderedClass)
module M = Map.Make (OrderedClass)

(* taxonomy is stored as a hastable from classes to triples consisting of  *)
(* the set of equivalent classes, direct superclasses, and direct          *)
(* subclasses                                                              *)
type t = (S.t * S.t * S.t) H.t

let init ont = H.create (O.total_ClassIRI ont)

(* Iterating over triples [a], [a_eq], [a_di] where [a] is a minimal       *)
(* element in its equvalent class [a_eq] and [a_di] are directly implied   *)
(* by [a] which are also minimal elements in their equivalent classes; All *)
(* minimal elements are according to the lexicographic ordering.           *)
let iter_a_eq_di f t ont top_equiv =
  let bot_equiv = ref S.empty in
  O.iter_record_Class ( fun c _ ->
          begin match c.data with
            | C.IRI _ ->
                let ce = ClassExpression.cons (CE.Class c) in
                (* first collect all implied classes *)
                let implied = ref ClassExpression.Set.empty in
                let implied_sorted = ref S.empty in
                (* computing implied classes *)
                ClassExpression.Set.iter ( fun de ->
                        if de != ce then
                          match de.data with
                          | CE.Class d ->
                              begin match d.data with
                                | Class.Constructor.IRI _ ->
                                    if not (S.mem d top_equiv) then (
                                      implied := ClassExpression.Set.add de !implied;
                                      implied_sorted := S.add d !implied_sorted;
                                    )
                                | Class.Constructor.Nothing ->
                                    bot_equiv := S.add c !bot_equiv
                                | Class.Constructor.Thing -> ()
                              end
                          | _ -> () (* collect only classes *)
                  )	(R.find_implied t ce);
                (* computing equivalent and filtering out non-directly     *)
                (* implied concepts; it is important to process implied in *)
                (* the sorted order for corectness                         *)
                let equiv = ref (S.singleton c) in
                S.iter ( fun d ->
                        let de = ClassExpression.cons (CE.Class d) in
                        if d != c && (ClassExpression.Set.mem de !implied) then
                          (
                            let de_implied = R.find_implied t de in
                            if ClassExpression.Set.mem ce de_implied then (
                              implied := ClassExpression.Set.remove de !implied;
                              match de.data with
                              | CE.Class d -> equiv := S.add d !equiv;
                              | _ -> ()
                            ) else implied := ClassExpression.Set.diff !implied
                                (ClassExpression.Set.remove de de_implied)
                          )
                  ) !implied_sorted;
                (* we apply [f] only if [c] is the minimal element in the  *)
                (* equivalent class which does not contan [top] or         *)
                (* [bottom]                                                *)
                if (S.min_elt !equiv == c) &&
                not (S.mem c top_equiv) && not (S.mem c !bot_equiv) then (
                  (* sort [!implied] into [!dimpl] *)
                  let dimpl = ref S.empty in
                  ClassExpression.Set.iter (fun de ->
                          match de.data with
                          | CE.Class d -> dimpl := S.add d !dimpl
                          | _ -> ()
                    ) !implied;
                  f c (S.remove c !equiv) !dimpl;
                ); (* close if *)
            | C.Thing -> ()
            | C.Nothing -> ()
          end;  
          (* incrementing the progress bar *)
          PB.step ();        
    ) ont;
  !bot_equiv
;;

let find_top_equiv t =
  match R.find_option_top t with
  | None -> S.empty;
  | Some top ->
      let top_equiv = ref S.empty in
      ClassExpression.Set.iter ( fun de ->
              if de != top then
                match de.data with
                | CE.Class d ->
                    top_equiv := S.add d !top_equiv
                | _ -> ()
        )	(R.find_implied t top);
      !top_equiv
;;

(* ===================== computing taxonomy ======================= *)

let compute t ont =
  PB.init (O.total_ClassIRI ont);
  let taxonomy = init ont in
  let find_record a =
    (* retuns the triple for a; if there is no, returns the empty triple *)
    try H.find taxonomy a
    with Not_found -> (S.empty, S.empty, S.empty)
  in
  let add_sub a b =
    (* add b as a child for a *)
    let (eqv, sup, sub) = find_record a in
    H.replace taxonomy a (eqv, sup, S.add b sub)
  in
  let top = Class.cons Class.Constructor.Thing in
  let bot = Class.cons Class.Constructor.Nothing in
  let top_eqv = find_top_equiv t in
  (* check if the ontology is consistent *)
  if S.mem bot top_eqv then begin
    H.add taxonomy top (S.singleton bot, S.empty, S.empty);
    (* setting progress bar to max *)
    PB.set_max ();
  end
  else begin
    let top_sub = ref S.empty in
    (* populating taxonomy record *)
    let bot_eqv =
      iter_a_eq_di ( fun a eqv sup ->
              S.iter (fun b -> add_sub b a) sup;
              let (_, _, sub) = find_record a in
              H.replace taxonomy a (eqv, sup, sub);
              if S.is_empty sup then
                top_sub := S.add a !top_sub;            
        ) t ont top_eqv in
    (* saving the record for top *)
    H.add taxonomy top (top_eqv, S.empty,
        if S.is_empty !top_sub then S.singleton bot else !top_sub);
    let bot_sup = ref S.empty in    
    (* saving the record for named classes *)
    O.iter_record_Class ( fun a _ ->            
            let (eqv, sup, sub) = find_record a in            
            if not (S.is_empty eqv) then begin              
              H.replace taxonomy a (
                  eqv,
                  (if S.is_empty sup then S.singleton top else sup),
                  (if S.is_empty sub then S.singleton bot else sub)
                );
              if S.is_empty sub then bot_sup := S.add a !bot_sup;              
            end;                        
      ) ont;    
    (* saving the recored for bottom *)
    H.add taxonomy bot (
        bot_eqv,
        (if S.is_empty !bot_sup then S.singleton top else !bot_sup),
        S.empty
      );
  end; (* close: else *)
  taxonomy
;;

(* [find_iter tax c] returns a triple [iter_eqv], [iter_sup], [iter_sub]   *)
(* of functions that iterate over respectively the set of equivalent       *)
(* classes, direct superclasses and direct subclasses                      *)

let find_iter t a =
  let (eqv, sup, sub) =
    try H.find t a
    with Not_found -> S.empty, S.empty, S.empty
  in
  (fun f -> S.iter f eqv),
  (fun f -> S.iter f sup),
  (fun f -> S.iter f sub)
;;

(* ================== formatting and printing ===================== *)

(** printing in lisp format *)

(* in lisp format, for every concept name [a] we print a triple            *)
(* ([equiv],[parents],[children]) where [equiv] consists of all concept    *)
(* names equivalent to [a], [parents] consists of all concept names        *)
(* directly implied by [a], and [children] consists of all concept names   *)
(* which directly imply [a]. A particular form how this triples are        *)
(* printed may vary and specified by the function below. The concept names *)
(* in all sets are printed in the lexicoraphical order.                    *)

let print_lisp_triple out iter_equiv iter_parents iter_children =
  let module F = Format in
  F.set_formatter_out_channel out;
  F.print_string "(taxonomy";
  F.open_box 1;
  (* equvalent *)
  let first = ref true in
  iter_equiv (fun c ->
          F.print_space ();
          if !first then first := false else
            (F.print_string "="; F.print_space (););
          F.open_box 1;
          F.print_string (Owl2IO.str_of_Class c);
          F.close_box ();
    );
  F.print_newline ();
  (* printing parents *)
  F.print_string "       parents: (";
  F.open_box 0;
  let first = ref true in
  iter_parents (fun c ->
          if !first then first := false else F.print_space ();
          F.open_box 1;
          F.print_string (Owl2IO.str_of_Class c);
          F.close_box ();
    );
  F.close_box ();
  F.print_string ")";
  F.print_newline ();
  (* printing directly implied concepts *)
  F.print_string "       children: (";
  F.open_box 0;
  let first = ref true in
  iter_children (fun c ->
          if !first then first := false else F.print_space ();
          F.open_box 1;
          F.print_string (Owl2IO.str_of_Class c);
          F.close_box ();
    );
  F.close_box ();
  F.print_string ")";
  F.close_box ();
  F.print_string ")";
  F.print_newline ();
;;

(* The imlication set for a concept name contains only the information     *)
(* about the equivalent concepts and directly implied concepts (parents).  *)
(* The children are therefore need to be computed separately. There is a   *)
(* trade-off between how fast the triples can be printed and how much      *)
(* memory is required for their computation. We provide two functions for  *)
(* printing the list triples for all concept names. The first uses one     *)
(* pass over implication sets and stores the triples for every concept     *)
(* name in a separate hash table; the second computes and stores only the  *)
(* children concept names during the first pass and during the second pass *)
(* the equivalent and parent concept names are computed on the fly.        *)
(* Therefore, the first function is faster but requires more memory for    *)
(* the hash table than the second one. I do not know how to have the best  *)
(* of both. The triples are printed only for the minimal in the            *)
(* lexicoraphic ordering element in every equivalence class of concept     *)
(* names and ordered also according to the lexicographic ordering on       *)
(* concept names.                                                          *)

let print_lisp_fast t ont out =
  PB.init (2 * O.total_ClassIRI ont);
  let tax_record = H.create (O.total_ClassIRI ont) in
  let find_record a =
    (* retuns the triple for a; if there is no, returns the empty triple *)
    try H.find tax_record a
    with Not_found -> (S.empty, S.empty, S.empty)
  in
  let add_child a b =
    (* add b as a child for a *)
    let (equiv, parents, children) = find_record a in
    H.replace tax_record a
      (equiv, parents, S.add b children)
  in
  let top = Class.cons Class.Constructor.Thing in
  let bot = Class.cons Class.Constructor.Nothing in
  let top_equiv = find_top_equiv t in
  (* check if the ontology is consistent *)
  if S.mem bot top_equiv then (
    print_lisp_triple out
      (fun f ->
            f top;
            O.iter_record_Class (fun c _ -> f c) ont;
            f bot;
      ) (fun f -> ()) (fun f -> ());
    (* setting progress bar to max *)
    PB.set_max ();
  )
  else (
    let top_children = ref S.empty in
    (* populating taxonomy record *)
    let bot_equiv =
      iter_a_eq_di ( fun a equiv parents ->
              S.iter (fun b -> add_child b a) parents;
              let (_, _, children) = find_record a in
              H.replace tax_record a
                (equiv, parents, children);
              if S.is_empty parents then
                top_children := S.add a !top_children;
        ) t ont top_equiv in
    (* printing top *)
    print_lisp_triple out
      (fun f -> f top; S.iter f top_equiv)
      (fun f -> S.iter f S.empty)
      (fun f -> S.iter f (if S.is_empty !top_children then S.singleton bot else !top_children));
    let bot_parents = ref S.empty in
    (* printing atomic concepts *)
    O.iter_record_Class ( fun a _ ->
            let (equiv, parents, children) = find_record a in
            if not (S.is_empty equiv) then (
              print_lisp_triple out
                (fun f -> S.iter f equiv)
                (fun f -> S.iter f (if S.is_empty parents then S.singleton top else parents) )
                (fun f -> S.iter f (if S.is_empty children then S.singleton bot else children));
              if S.is_empty children then
                bot_parents := S.add a !bot_parents;
            ); (* close: if *)
            (* incrementing the progress bar *)
            PB.step ();
      ) ont;
    (* printing bottom *)
    print_lisp_triple out
      (fun f -> f bot; S.iter f bot_equiv)
      (fun f -> S.iter f (if S.is_empty !bot_parents then S.singleton top else !bot_parents))
      (fun f -> S.iter f S.empty);
  ) (* close: else *)
;;

(* slower but more memory efficient *)
let print_lisp_slow t ont out =
  PB.init (2 * (O.total_ClassIRI ont));
  let empty = S.empty in
  let tax_record = H.create (O.total_ClassIRI ont) in
  let find_record a =
    (* retuns the parents of a *)
    try H.find tax_record a
    with Not_found -> empty
  in
  let add_child a b =
    (* add b as a parent for a *)
    let children = find_record a in
    H.replace tax_record a (S.add b children)
  in
  let top = Class.cons C.Thing in
  let bot = Class.cons C.Nothing in
  let top_equiv = find_top_equiv t in
  (* check if the ontology is consistent *)
  if S.mem bot top_equiv then (
    print_lisp_triple out
      (fun f ->
            f top;
            O.iter_record_Class (fun c _ -> f c) ont;
            f bot;
      ) (fun f -> ()) (fun f -> ());
    (* setting progress bar to max *)
    PB.set_max ();
  )
  else (
    let top_children = ref S.empty in
    (* populating taxonomy record *)
    let bot_equiv =
      iter_a_eq_di ( fun a _ parents ->
              S.iter (fun b -> add_child b a) parents;
              if S.is_empty parents then top_children := S.add a !top_children;
        ) t ont top_equiv in
    (* printing top *)
    print_lisp_triple out
      (fun f -> f top; S.iter f top_equiv)
      (fun f -> S.iter f S.empty)
      (fun f -> S.iter f (if S.is_empty !top_children then S.singleton bot else !top_children));
    let bot_parents = ref S.empty in
    (* printing *)
    let _ =
      iter_a_eq_di ( fun c equiv parents ->
              let children = find_record c in
              if not (S.is_empty equiv) then (
                print_lisp_triple out
                  (fun f -> S.iter f equiv)
                  (fun f -> S.iter f (if S.is_empty parents then S.singleton top else parents) )
                  (fun f -> S.iter f (if S.is_empty children then S.singleton bot else children));
                if S.is_empty children then
                  bot_parents := S.add c !bot_parents;
              ) (* close: if *)
        ) t ont top_equiv in
    (* printing bottom *)
    print_lisp_triple out
      (fun f -> f bot; S.iter f bot_equiv)
      (fun f -> S.iter f (if S.is_empty !bot_parents then S.singleton top else !bot_parents))
      (fun f -> S.iter f S.empty);
  ) (* close: else *)
;;

let print_lisp = print_lisp_fast

(** printing in krss format *)

(* in krss format, for every class of equivalent concept names with the    *)
(* smallest in the lexicoraphical ordering concept [a] we print            *)
(* [(equivalent a b)] for all [b] different from [a] in the equivalence    *)
(* class for [a], and [(implies a b)] for all [b] directly implied by [a]. *)
(* The axioms are first lexicographically sorted by [a], then by the type  *)
(* of the axiom (equivalent > implies), then by [b].                       *)

let print_krss t ont out =
  PB.init (O.total_ClassIRI ont);
  Printf.fprintf out ";; This concept taxionomy contains direct implications between classes:\n";
  Printf.fprintf out ";;\n";
  Printf.fprintf out ";;   (implies c1 c2)     means that c1 is a direct sub-class of c2\n";
  Printf.fprintf out ";;   (equivalent c1 c2)  means that c1 and c2 are equivalent classes\n";
  Printf.fprintf out ";;\n";
  Printf.fprintf out ";;---------------------------------------------------------------------\n";
  let bot = Class.cons C.Nothing in
  let top_equiv = find_top_equiv t in
  (* check if the ontology is consistent *)
  if S.mem bot top_equiv then (
    Printf.fprintf out "(equivalent %s %s)\n"
      (Owl2IO.str_of_Class (Class.cons C.Thing) ) (Owl2IO.str_of_Class (Class.cons C.Nothing) );
    (* setting progress bar to max *)
    PB.set_max ();
  ) else (
    S.iter (fun c -> Printf.fprintf out "(equivalent %s %s)\n"
              (Owl2IO.str_of_Class (Class.cons C.Thing) ) (Owl2IO.str_of_Class c)
      ) top_equiv;
    let bot_equiv =
      iter_a_eq_di ( fun a equiv_a dimpl_a ->
              S.iter (fun b ->
                      if a != b then Printf.fprintf out "(equivalent %s %s)\n" (Owl2IO.str_of_Class a) (Owl2IO.str_of_Class b)
                ) equiv_a;
              (* printing directly implied concepts *)
              S.iter (fun b ->
                      Printf.fprintf out "(implies %s %s)\n" (Owl2IO.str_of_Class a) (Owl2IO.str_of_Class b)
                ) dimpl_a;
        ) t ont top_equiv;
    in
    S.iter (fun c -> Printf.fprintf out "(equivalent %s %s)\n"
              (Owl2IO.str_of_Class (Class.cons C.Nothing)) (Owl2IO.str_of_Class c)
      ) bot_equiv
  ) (* close: else *)
;;

let print_fowl t ont out =
  let print_equivalent a s =
    if not (S.is_empty s) then begin
      Printf.fprintf out "EquivalentClasses(%s" (Owl2IO.str_of_Class a);
      S.iter (fun b -> Printf.fprintf out " %s" (Owl2IO.str_of_Class b)) s;
      Printf.fprintf out ")\n";
    end
  in
  PB.init (O.total_ClassIRI ont);
  Printf.fprintf out "Ontology(taxonomy\n";
  let bot = Class.cons C.Nothing in
  let top_equiv = find_top_equiv t in
  (* check if the ontology is consistent *)
  if S.mem bot top_equiv then begin
    Printf.fprintf out "EquivalentClasses(%s %s)\n"
      (Owl2IO.str_of_Class (Class.cons C.Thing) ) (Owl2IO.str_of_Class (Class.cons C.Nothing) );
    (* setting progress bar to max *)
    PB.set_max ();
  end
  else begin
    print_equivalent (Class.cons C.Thing) top_equiv;
    let bot_equiv =
      iter_a_eq_di ( fun a equiv_a dimpl_a ->
              print_equivalent a equiv_a;
              (* printing directly implied concepts *)
              S.iter (fun b ->
                      Printf.fprintf out "SubClassOf(%s %s)\n" (Owl2IO.str_of_Class a) (Owl2IO.str_of_Class b)
                ) dimpl_a;
        ) t ont top_equiv;
    in
    print_equivalent (Class.cons C.Nothing) bot_equiv;
  end;
  Printf.fprintf out ")";
;;

let print_statistics t ont out =
  let n = ref 0 in
  let _ =
    iter_a_eq_di ( fun a equiv_a dimpl_a ->
        (* counting equivalent concepts *)
            S.iter (fun _ ->	incr n) equiv_a;
            (* counting directly implied concepts *)
            S.iter (fun _ -> incr n ) dimpl_a;
      ) t ont in
  Printf.fprintf out "%n concept names have totally %n equivalent or directly implied concepts, %f everage\n"
    (O.total_ClassIRI ont) !n
    ((float_of_int !n) /. (float_of_int (O.total_ClassIRI ont)) )
