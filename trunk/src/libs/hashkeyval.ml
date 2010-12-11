(* Adapted from the module hashtbl.ml in the standard library *)

open CommonTypes

(* Hash tables *)

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

let hash x = hash_param 10 100 x

(* We do dynamic hashing, and resize the table and rehash the elements     *)
(* when buckets become too long.                                           *)

type ('a, 'b) t =
	{ mutable size: int;                        (* number of elements *)
		mutable data: ('a, 'b) bucketlist array } (* the buckets *)

and ('a, 'b) bucketlist =
		Empty
	| Cons of ('a * 'b) * ('a, 'b) bucketlist

let create initial_size =
	let s = min (max 1 initial_size) Sys.max_array_length in
	{ size = 0; data = Array.make s Empty }

let clear h =
	for i = 0 to Array.length h.data - 1 do
		if h.data.(i) != Empty then h.data.(i) <- Empty
	done;
	h.size <- 0

let copy h =
	{ size = h.size;
		data = Array.copy h.data }

let length h = h.size

let resize hashfun tbl =
	let odata = tbl.data in
	let osize = Array.length odata in
	let nsize = min (2 * osize + 1) Sys.max_array_length in
	if nsize <> osize then begin
		let ndata = Array.create nsize Empty in
		let rec insert_bucket = function
				Empty -> ()
			| Cons( (key, _) as el, rest) ->
					insert_bucket rest; (* preserve original order of elements *)
					let nidx = (hashfun key) mod nsize in
					ndata.(nidx) <- Cons(el, ndata.(nidx)) in
		for i = 0 to osize - 1 do
			insert_bucket odata.(i)
		done;
		tbl.data <- ndata;
	end

let rec find_rec eq key = function
		Empty ->
			raise Not_found
	| Cons( (key1, _) as el1, rest) ->
			if eq key key1 then el1 else find_rec eq key rest

let find_fast eq key data =
	match data with
		Empty -> raise Not_found
	| Cons( (key1, v1) as el1, rest1) ->
			if eq key key1 then el1 else
				match rest1 with
					Empty -> raise Not_found
				| Cons( (key2, v2) as el2, rest2) ->
						if eq key key2 then el2 else
							match rest2 with
								Empty -> raise Not_found
							| Cons( (key3, v3) as el3, rest3) ->
									if eq key key3 then el3 else find_rec eq key data

(* [fresh ()] is a function that creates a default binding for key if      *)
(* there are no such in the hashtable                                      *)

let cons_par hashfun eq h key fresh =
	let i = (hashfun key) mod (Array.length h.data) in
	try
		find_fast eq key h.data.(i)
	with Not_found ->
			let el = key, fresh () in
			h.data.(i) <- Cons(el, h.data.(i));
			h.size <- succ h.size;
			if h.size > Array.length h.data lsl 1 then resize hashfun h;
			el

let cons h key fresh = cons_par hash (fun x y -> compare x y = 0) h key fresh

let iter f h =
	let rec do_bucket = function
			Empty ->
				()
		| Cons(el, rest) ->
				f el; do_bucket rest in
	let d = h.data in
	for i = 0 to Array.length d - 1 do
		do_bucket d.(i)
	done

(* Functorial interface *)

module type S =
sig
	type key		
	type 'a t
	val create : int -> 'a t
	val length : 'a t -> int
	val cons : 'a t -> key -> (unit -> 'a) -> (key * 'a)
	val iter : ( (key * 'a) -> unit ) -> 'a t -> unit
end

module Make(H: HashedType): (S with type key = H.t) =
struct
	type ('a, 'b) _t = ('a, 'b) t
	type key = H.t	
	type 'a t = (key, 'a) _t
	let create = create
	let length = length
	let safehash key = (H.hash key) land max_int
	let cons h key fresh = cons_par safehash H.equal h key fresh
	let iter = iter
end
