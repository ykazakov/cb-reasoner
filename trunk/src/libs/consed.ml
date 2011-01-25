(* Weak hash maps using the module Weak from the standard library *)

type 'a consed = {
	data : 'a;
	tag : int;
}

let compare x y = (x.tag - y.tag)
let equal x y = (x == y)
let hash x = x.tag

(* Functorial interface *)

module type HashedType = sig
	type t
	val equal: t -> t -> bool
	val hash: t -> int
end

module type S = sig
	type key
	type t = key consed
	val cons: key -> t
	val compare : t -> t -> int
	val equal : t -> t -> bool
	val hash : t -> int
end

module Make (H: HashedType) : (S with type key = H.t) =
struct
	type key = H.t
	type t = H.t consed
	module HT = struct
		type t = H.t consed
		let equal x y = H.equal x.data y.data
		let hash x = H.hash x.data
	end
	module WH = Weak.Make (HT)
	let h = WH.create (127)
	let tag = ref 0
	let cons x =
		let y = {
			data = x;
			tag = !tag;
		} in
		let z = WH.merge h y in
		if (z == y) then incr tag;
		z
	let compare = compare
	let equal = equal
	let hash = hash
end

(* Old implementation based on custom weak hashtables - doesn't work well  *)
(* for some reason                                                         *)

(*|type 'a t =                                                                            *)
(*|	{ mutable size: int;                (* number of elements *)                         *)
(*|		mutable content: 'a Weak.t;       (* the content *)                                *)
(*|		mutable hashes: int array;        (* hashes *)                                     *)
(*|		mutable index: int array;         (* indices *)                                    *)
(*|		mutable recycle: int list;        (* list of free indicies *)                      *)
(*|		mutable rover: int;               (* for cleaning up *)                            *)
(*|	}                                                                                    *)
(*|                                                                                       *)
(*|let create initial_size =                                                              *)
(*|	let sw = min (max 1 initial_size) (Sys.max_array_length - 1) in                      *)
(*|	let s = min (max 1 (4 * initial_size / 3)) Sys.max_array_length in                   *)
(*|	{ size = 0;                                                                          *)
(*|		content = Weak.create sw;                                                          *)
(*|		hashes = Array.make s (- 1); (* the value by which we recognize unoccupied entry *)*)
(*|		index = Array.make s 0;                                                            *)
(*|		recycle = [];                                                                      *)
(*|		rover = 0;                                                                         *)
(*|	}                                                                                    *)
(*|                                                                                       *)
(*|let resize h =                                                                         *)
(*|	let osize = Array.length h.index in                                                  *)
(*|	let oindex = h.index in                                                              *)
(*|	let ohashes = h.hashes in                                                            *)
(*|	let nsize = min ((8 * h.size) / 3 + 1) Sys.max_array_length in                       *)
(*|	if nsize = osize then                                                                *)
(*|		failwith "consed: hash set cannot grow more"                                       *)
(*|	else begin                                                                           *)
(*|		let nindex = Array.create nsize 0 in                                               *)
(*|		let nhashes = Array.create nsize (- 1) in                                          *)
(*|		let rec insert idx i =                                                             *)
(*|			if nhashes.(i) == (- 1) then begin                                               *)
(*|				Array.blit ohashes idx nhashes i 1;                                            *)
(*|				Array.blit oindex idx nindex i 1;                                              *)
(*|			end                                                                              *)
(*|			else insert idx ((i + 1) mod nsize)                                              *)
(*|		in                                                                                 *)
(*|		for i = 0 to osize - 1 do                                                          *)
(*|			if ohashes.(i) != (- 1) then insert i (ohashes.(i) mod nsize)                    *)
(*|		done;                                                                              *)
(*|		h.hashes <- nhashes;                                                               *)
(*|		h.index <- nindex;                                                                 *)
(*|		h.rover <- h.rover mod nsize;                                                      *)
(*|		let ncontent = Weak.create (min (2 * h.size + 1) (Sys.max_array_length - 1)) in    *)
(*|		Weak.blit h.content 0 ncontent 0 h.size;                                           *)
(*|		h.content <- ncontent;                                                             *)
(*|	end                                                                                  *)
(*|                                                                                       *)
(*|let sweep h =                                                                          *)
(*|	let size = Array.length h.index in                                                   *)
(*|	let rec find_free i =                                                                *)
(*|		if i = h.rover || h.hashes.(i) == (- 1) then i                                     *)
(*|		else find_free ((i + 1) mod size)                                                  *)
(*|	in                                                                                   *)
(*|	let rec move () =                                                                    *)
(*|		h.rover <- ((h.rover + 1) mod size);                                               *)
(*|		if h.hashes.(h.rover) != (- 1) then                                                *)
(*|			if Weak.check h.content h.index.(h.rover) then                                   *)
(*|				let i = find_free (h.hashes.(h.rover) mod size) in                             *)
(*|				if i <> h.rover then begin                                                     *)
(*|					(* moving *)                                                                 *)
(*|					Array.blit h.hashes h.rover h.hashes i 1;                                    *)
(*|					Array.blit h.index h.rover h.index i 1;                                      *)
(*|					h.hashes.(h.rover) <- (- 1);                                                 *)
(*|					move ();                                                                     *)
(*|				end else move ()                                                               *)
(*|			else begin                                                                       *)
(*|				(* deleting *)                                                                 *)
(*|				h.hashes.(h.rover) <- (- 1);                                                   *)
(*|				h.size <- pred h.size;                                                         *)
(*|				h.recycle <- h.index.(h.rover) :: h.recycle;                                   *)
(*|				move ()                                                                        *)
(*|			end                                                                              *)
(*|	in                                                                                   *)
(*|	h.rover <- ((h.rover + 1) mod size);                                                 *)
(*|	if h.hashes.(h.rover) != (- 1) &&                                                    *)
(*|	not (Weak.check h.content h.index.(h.rover)) then                                    *)
(*|		begin                                                                              *)
(*|			(* deleting *)                                                                   *)
(*|			h.hashes.(h.rover) <- (- 1);                                                     *)
(*|			h.size <- pred h.size;                                                           *)
(*|			h.recycle <- h.index.(h.rover) :: h.recycle;                                     *)
(*|			move ()                                                                          *)
(*|		end                                                                                *)
(*|                                                                                       *)
(*|module Make(H: HashedType): (S with type key = H.t) =                                  *)
(*|struct                                                                                 *)
(*|  type key = H.t                                                                       *)
(*|                                                                                       *)
(*|  let safehash key = (H.hash key) land max_int                                         *)
(*|                                                                                       *)
(*|  let (h : H.t consed t) = create 31                                                   *)
(*|                                                                                       *)
(*|  let cons key =                                                                       *)
(*|    let hash = safehash key in                                                         *)
(*|    let size = Array.length h.index in                                                 *)
(*|    let rec cons_rec i =                                                               *)
(*|      if h.hashes.(i) == (- 1) then begin                                              *)
(*|        let idx = begin match h.recycle with                                           *)
(*|            | [] -> h.size                                                             *)
(*|            | hd :: tail -> h.recycle <- tail; hd                                      *)
(*|          end in                                                                       *)
(*|        let key_consed = {                                                             *)
(*|          data = key;                                                                  *)
(*|          tag = idx                                                                    *)
(*|        } in                                                                           *)
(*|        Weak.set h.content idx (Some key_consed);                                      *)
(*|        h.hashes.(i) <- hash;                                                          *)
(*|        h.index.(i) <- idx;                                                            *)
(*|        h.size <- succ h.size;                                                         *)
(*|        let l = Weak.length h.content in                                               *)
(*|        if h.size = l then resize h;                                                   *)
(*|        sweep h;                                                                       *)
(*|        key_consed                                                                     *)
(*|      end                                                                              *)
(*|      else if h.hashes.(i) == hash then                                                *)
(*|        match Weak.get h.content h.index.(i) with                                      *)
(*|        | Some key_consed when H.equal key key_consed.data -> key_consed               *)
(*|        | _ -> cons_rec ((i + 1) mod size)                                             *)
(*|      else cons_rec ((i + 1) mod size)                                                 *)
(*|    in                                                                                 *)
(*|    cons_rec (hash mod size)                                                           *)
(*|end                                                                                    *)
