(* pp: -parser Camlp4OCamlRevisedParser -parser Camlp4QuotationCommon -parser Camlp4OCamlRevisedQuotationExpander -parser Camlp4GrammarParser -parser Camlp4OCamlParser *)

module Id = struct
  let name = "pa_typeext"
  let version = "1.0"
end

type t = | Foo of (String.t * String.t) * String.t

open Camlp4

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax
	
	(* datatypes for structures *)
	type t_path = string list                 (* dot separated type module names *)
	and t_variant = string * t_expr list
	and t_expr =
		| ProductType of t_expr list
		| PolymorphicType of t_expr list * t_path 
		| VariantType of t_variant list

  (* variable holding the current structure *)
  let mt : t_expr ref = ref (VariantType [])

  (** auxiliary functions *)

  (* create list of [n] variables with prefix [p] *)
  let rec create_vars p n accu =
    if n = 0 then accu else create_vars p (pred n) ((p ^ string_of_int n) :: accu)
  let create_vars p n = create_vars p n []
  (* create the comma-separated pattern from list of variables [vars] *)
  let create_vars_patt _loc vars =
      let l = List.map (fun lid -> <:patt< $lid:lid$ >>) vars in
      match l with
      | [] -> invalid_arg "create_vars_patt"
      | [p] -> p
      | p :: tl -> <:patt< ($p$, $Ast.paCom_of_list tl$) >>
	(* printing module identifier *)
	let rec print_path _loc = function
		| [] -> invalid_arg "print_module_t_constr"
		| [uid] -> <:ident< $uid:uid$ >>
		| uid :: tl -> <:ident< $uid:uid$.$print_path _loc tl$>>
	
	(** printing *)
	
	(* printing of type *)    
	let type_t_path _loc mp = <:ctyp< $id:print_path _loc mp$.t >>		
	let rec type_t_and _loc t = 
      Ast.tyAnd_of_list (List.map (type_t_expr _loc) t)
	and type_t_sta _loc t =
		  Ast.tySta_of_list (List.map (type_t_expr _loc) t)
	and type_t_variant _loc (uid, t_list) =  match t_list with
      | [] -> <:ctyp< $uid:uid$ >>
      | _  -> <:ctyp< $uid:uid$ of $type_t_and _loc t_list$ >>
  and type_t_expr _loc = function
		| ProductType t_list -> <:ctyp< ( $tup:type_t_sta _loc t_list$ ) >>			
		| PolymorphicType (t_lst, mp) ->			
			let rec appl accu = function
				| [] -> accu
				| t :: tl -> appl <:ctyp< $accu$ $type_t_expr _loc t$ >> tl
			in appl (type_t_path _loc mp) t_lst
		| VariantType v_list -> 
			<:ctyp< [ $Ast.tyOr_of_list (List.map (type_t_variant _loc) v_list)$ ] >>
  let str_item_type _loc t = 
      <:str_item< type t = $type_t_expr _loc t$ >>
  let sig_item_type _loc t = 
      <:sig_item< type t = $type_t_expr _loc t$ >>

  (* printing of the hash function *)
  let hash_t_path _loc mp = <:expr< $id:print_path _loc mp$.hash >>
	let rec hash_pairs _loc hf = function
	  | [] -> <:expr< [] >>
	  | (t, v) :: tl ->
		  <:expr< [ $hash_t_expr_v _loc hf t v$ :: $hash_pairs _loc hf tl$] >>
  and hash_t_variant _loc hf n (uid, t_list) =
	      let l = List.length t_list in
	      if l = 0 then <:match_case< $uid:uid$ -> $lid:hf$ [$`int:n$] >>
	      else (* if l > 0 *)
		    let vars = create_vars "t" l in
		    let pairs = List.map2 (fun m v -> (m, v)) t_list vars in
		    let patt_vars = create_vars_patt _loc vars in
		    let expr = hash_pairs _loc hf pairs in
		    <:match_case< $uid:uid$ $patt_vars$ -> $lid:hf$ [$`int:n$ :: $expr$] >>
  and hash_t_expr _loc hf = function
		| ProductType t_list ->
			  let l = List.length t_list in
				if l = 0 then invalid_arg "hash_t_expr"
			  else
					let vars = create_vars "t" l in
			    let pairs = List.map2 (fun m v -> (m, v)) t_list vars in
		      let patt_vars = create_vars_patt _loc vars in
		      let expr = hash_pairs _loc hf pairs in
			    <:expr< fun $patt_vars$ -> $lid:hf$ $expr$ >>
		| PolymorphicType (t_lst, mp) ->
			  let rec appl accu = function
				 | [] -> accu
				 | t :: tl -> appl <:expr< $accu$ $hash_t_expr _loc hf t$ >> tl
			  in
			  appl <:expr< $hash_t_path _loc mp$ >> t_lst
		| VariantType v_list ->
	      let n = ref 0 in
		    let gen () = incr n; !n in
				let f = Ast.mcOr_of_list
				  (List.map (fun case -> hash_t_variant _loc hf (gen ()) case) v_list)
				in <:expr< fun [$f$] >>
	and hash_t_expr_v _loc hf t v = <:expr< $hash_t_expr _loc hf t$ $lid:v$ >>
  let str_item_hash _loc hf t =
    <:str_item< value hash = $hash_t_expr _loc hf t$ >>
	let sig_item_hash _loc =
    <:sig_item< value hash : t -> int >>		
		
  (* printing of the equality function *)
	let equal_t_path _loc mp =
		<:expr< $id:print_path _loc mp$.equal >>  
  let rec equal_triples _loc = function
    | [] -> invalid_arg "equal_triples"
    | [ (t, v1, v2) ] -> equal_t_expr_v _loc t v1 v2
    | (t, v1, v2) :: tl ->			
			<:expr< $equal_t_expr_v _loc t v1 v2$ && $equal_triples _loc tl$ >>
  and equal_t_variant _loc (uid, t_list) =
		    let l = List.length t_list in
        if l = 0 then <:match_case< ($uid:uid$, $uid:uid$) -> True >>
        else (* if l > 0 *)
          let vars1 = create_vars "s" l in
          let vars2 = create_vars "t" l in
          let tripes = List.map2 (fun uid (v1, v2) -> (uid, v1, v2)) t_list 
                        (List.map2 (fun v1 v2 -> (v1, v2)) vars1 vars2)
          in
          let patt_vars1 = create_vars_patt _loc vars1 in
          let patt_vars2 = create_vars_patt _loc vars2 in
          let expr_equal = equal_triples _loc tripes in
          <:match_case< ($uid:uid$ $patt_vars1$, $uid:uid$ $patt_vars2$) -> $expr_equal$ >>
  and equal_t_expr _loc = function
		| ProductType t_list ->
			  let l = List.length t_list in
				if l = 0 then invalid_arg "equal_t_expr"
			  else 
					let vars1 = create_vars "s" l in
          let vars2 = create_vars "t" l in
          let tripes = List.map2 (fun uid (v1, v2) -> (uid, v1, v2)) t_list 
                        (List.map2 (fun v1 v2 -> (v1, v2)) vars1 vars2)
          in
          let patt_vars1 = create_vars_patt _loc vars1 in
          let patt_vars2 = create_vars_patt _loc vars2 in
          let expr_equal = equal_triples _loc tripes in
          <:expr< fun $patt_vars1$ $patt_vars2$ -> $expr_equal$ >>			
		| PolymorphicType (t_lst, mp) ->
			  let rec appl accu = function
				 | [] -> accu
				 | t :: tl -> appl <:expr< $accu$ $equal_t_expr _loc t$ >> tl 
			  in  
			  appl <:expr< $equal_t_path _loc mp$ >> t_lst
		| VariantType v_list -> 
        let cases = Ast.mcOr_of_list (List.map (equal_t_variant _loc) v_list) in
        (* determining whether the cases are exhaustive *)
				let f = match v_list with
          | [_] -> cases
          | _ -> <:match_case< $cases$ | _ -> False >>
				in <:expr< fun s t -> match (s, t) with [$f$] >>
	and equal_t_expr_v _loc t v1 v2 = <:expr< $equal_t_expr _loc t$ $lid:v1$ $lid:v2$ >>		
  let str_item_equal _loc t =
    <:str_item< value equal = $equal_t_expr _loc t$ >>
	let sig_item_equal _loc =
    <:sig_item< value equal : t -> t -> bool >>	
		
	(* printing of the compare function *)
  let compare_t_path _loc mp =
		<:expr< $id:print_path _loc mp$.compare >>
	let rec compare_triples _loc = function
    | [] -> invalid_arg "compare_triples"
    | [ (t, v1, v2) ] -> compare_t_expr_v _loc t v1 v2
    | (t, v1, v2) :: tl ->
			<:expr< let c = $compare_t_expr_v _loc t v1 v2$ in if c <> 0 then c else $compare_triples _loc tl$ >>
	and compare_t_variant _loc last (uid, t_list) = 
		    let l = List.length t_list in
        if l = 0 then
			    let case = <:match_case< ($uid:uid$, $uid:uid$) -> 0 >>
			    in if last then case else <:match_case< $case$ | ($uid:uid$, _) -> -1 | (_, $uid:uid$) -> 1 >>
        else (* if l > 0 *)
		      let vars1 = create_vars "s" l in
          let vars2 = create_vars "t" l in
          let tripes = List.map2 (fun uid (v1, v2) -> (uid, v1, v2)) t_list 
                        (List.map2 (fun v1 v2 -> (v1, v2)) vars1 vars2)
          in
          let patt_vars1 = create_vars_patt _loc vars1 in
          let patt_vars2 = create_vars_patt _loc vars2 in
          let expr_compare = compare_triples _loc tripes in
		      let case = <:match_case< ($uid:uid$ $patt_vars1$, $uid:uid$ $patt_vars2$) -> $expr_compare$ >> in		
		      if last then case else <:match_case< $case$ | ($uid:uid$ _, _) -> -1 | (_, $uid:uid$ _) -> 1 >>
	and compare_t_expr _loc = function
		| ProductType t_list ->
			  let l = List.length t_list in
				if l = 0 then invalid_arg "hash_t_expr"
			  else 
					let vars1 = create_vars "s" l in
          let vars2 = create_vars "t" l in
          let tripes = List.map2 (fun uid (v1, v2) -> (uid, v1, v2)) t_list 
                        (List.map2 (fun v1 v2 -> (v1, v2)) vars1 vars2)
          in
          let patt_vars1 = create_vars_patt _loc vars1 in
          let patt_vars2 = create_vars_patt _loc vars2 in
          let expr_compare = compare_triples _loc tripes in
		      <:expr< fun $patt_vars1$ $patt_vars2$ -> $expr_compare$ >> 
		| PolymorphicType (t_lst, mp) ->
			  let rec appl accu = function
				 | [] -> accu
				 | t :: tl -> appl <:expr< $accu$ $compare_t_expr _loc t$ >> tl 
			  in  
			  appl <:expr< $compare_t_path _loc mp$ >> t_lst 
		| VariantType v_list -> 
			  let rec cases = function
					| [] -> invalid_arg "compare_t_expr"
					| [c] -> [compare_t_variant _loc true c]
					| c :: tl -> (compare_t_variant _loc false c) :: cases tl
				in 
				let cases = Ast.mcOr_of_list (cases v_list)
				in <:expr< fun s t -> match (s, t) with [$cases$] >>
	and compare_t_expr_v _loc t v1 v2 = <:expr< $compare_t_expr _loc t$ $lid:v1$ $lid:v2$ >>
  let str_item_compare _loc t =
    <:str_item< value compare = $compare_t_expr _loc t$ >>	
	let sig_item_compare _loc =
    <:sig_item< value compare : t -> t -> int >>	
		
	(** parsing *)
  
	let t_top = Gram.Entry.mk "t_top"
  let t_expr = Gram.Entry.mk "t_expr"
	let t_variant = Gram.Entry.mk "t_variant"
	let t_star = Gram.Entry.mk "t_star"	
	let t_path = Gram.Entry.mk "t_path"		

  EXTEND Gram 
    str_item: LEVEL "top"
      [ [ "save"; t = t_top -> mt := t; <:str_item< >> 
        | "make_type" -> str_item_type _loc !mt
				| "make_hash"; hf = a_LIDENT -> str_item_hash _loc hf !mt
        | "make_equal" -> str_item_equal _loc !mt
				| "make_compare" -> str_item_compare _loc !mt
      ] ]
    ;
	  sig_item: LEVEL "top"
      [ [ "save"; m = t_top -> mt := m; <:sig_item< >> 
        | "make_type" -> sig_item_type _loc !mt
				| "make_ohtype" -> <:sig_item< $sig_item_type _loc !mt$; $sig_item_hash _loc$; $sig_item_equal _loc$; $sig_item_compare _loc$ >>      
      ] ]
    ;
		t_top:
		  [ [ OPT "|"; t = LIST1 t_variant SEP "|" -> VariantType t
			  | t = t_expr -> t
			] ]
		;
		t_variant:
      [ [ n = a_UIDENT; "of"; t = LIST1 t_expr SEP "*" -> (n, t)
        | n = a_UIDENT -> (n, [])
      ] ]
    ;
    t_expr:
      [	"star"
        [ t = SELF; "*"; tl = t_star -> ProductType (t :: tl) ]
			|	"typ1"
			  [ t = SELF; mp = t_path -> PolymorphicType ([t], mp) ]
			| "simple"
			  [ t = t_path -> PolymorphicType ([], t)
				| "("; t = SELF; ","; tl = LIST1 SELF SEP ","; ")"; mp = t_path -> PolymorphicType (t :: tl, mp)
				| "("; t = t_expr; ")" -> t
				]								
			]      
    ;
		t_star:
		  [ [ t1 = t_expr LEVEL "typ1"; "*"; t2 = SELF -> t1 :: t2
        | t = t_expr LEVEL "typ1" -> [t] 
			] ]
		;
		t_path:
      [ [ t = LIST1 a_UIDENT SEP "." -> t 
      ] ]
    ;    
  END

end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()