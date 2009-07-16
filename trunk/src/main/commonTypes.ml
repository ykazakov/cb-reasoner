(** modules and interfaces for various ordered structures **)

module type OrderedType = sig
	type t
	val compare : t -> t -> int
end

module type HashedType = sig
	type t
	val equal : t -> t -> bool
	val hash : t -> int
end

module type OrderedHashedType = sig
	include HashedType
	val compare : t -> t -> int
end

module type OrderedHashedTypeStr = sig
	include OrderedHashedType
	val str : t -> string
end

(** functor for constructing various ordered structures **)

module OrderedList =
struct
	module type S = OrderedHashedTypeStr
	module Make (O: OrderedHashedTypeStr): S with type t = O.t list =
	struct
		
		type t = O.t list
		
		(* !! we do not distinguish between lists that have the same elements  *)
		(* !!                                                                  *)
		
		let sort l = List.fast_sort O.compare l
		
		let rec compare l1 l2 =
			match (sort l1), (sort l2) with
			| [], [] -> 0
			| _:: _, [] -> 1
			| [], _:: _ -> - 1
			| h1:: t1, h2:: t2 ->
					let comp = O.compare h1 h2 in
					if comp = 0 then compare t1 t2 else comp
		
		let rec equal l1 l2 =
			match (sort l1), (sort l2) with
			| [], [] -> true
			| h1:: t1, h2:: t2 ->
					let eq = O.equal h1 h2 in
					if eq then equal t1 t2 else false
			| _ -> false
		
		type hash_type = int list
		
		let rec hash_struct l : hash_type =
			match sort l with
			| [] -> []
			| h:: t -> (O.hash h) :: (hash_struct t)
		
		let hash l = Hashtbl.hash_param 100 100 (hash_struct l)
		
		let str l =
			let rec str_aux l =
				match l with
				| [] -> ""
				| [el] -> O.str el
				| el :: l -> O.str el ^ "; " ^ str_aux l
			in "[" ^ str_aux l ^ "]"
		
	end
end

module OrderedPair =
struct
	module type S = OrderedHashedTypeStr
	module Make (O1: OrderedHashedTypeStr) (O2: OrderedHashedTypeStr): S with type t = O1.t * O2.t =
	struct
		type t = O1.t * O2.t
		
		let compare p1 p2 =
			match p1, p2 with
			| (e11, e12), (e21, e22) ->
					let comp = O1.compare e11 e21 in
					if comp = 0 then O2.compare e12 e22 else comp
		
		let equal p1 p2 =
			match p1, p2 with
			| (e11, e12), (e21, e22) ->
					let eq = O1.equal e11 e21 in
					if eq then O2.equal e12 e22 else false
		
		type hash_type = int * int
		
		let hash_struct p =
			match p with
			| (el1, el2) -> (O1.hash el1, O2.hash el2)
		
		let hash p = Hashtbl.hash_param 100 100 (hash_struct p)
		
		let str p =
			match p with
			| (el1, el2) ->	"(" ^ O1.str el1 ^ ", " ^ O2.str el2 ^ ")"
	end
end