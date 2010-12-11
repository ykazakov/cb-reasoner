(* hash sets using array and linear probing for resolving hash collision   *)
(* see [1] p.526                                                           *)
(*| [1] Donald E. Knuth, The Art of Computer Programming, Volume 3,        *)
(*| Sorting and Searching, Second Edition                                  *)

(* We do dynamic hashing, and resize when the array becomes too long. *)

type 'a hashset =
	{ mutable size: int;          (* number of elements; *)
		(* if negative the hashset is locked for writing *)
		mutable data: 'a array; }   (* the elements *)

(* Unique blank element of any type. Works by magic! Must be allocated in  *)
(* heap.                                                                   *)
let blank = Obj.magic (Obj.repr [0]);;

let length h = abs h.size

let create initial_size =
	let s = min (max 1 initial_size) Sys.max_array_length in
	{ size = 0; data = Array.create s blank }

let clear h =
	let base = Array.length h.data in
	for i = 0 to base - 1 do
		if h.data.(i) != blank then h.data.(i) <- blank;
	done;
	
	Array.fill h.data 0 (Array.length h.data) blank;
	h.size <- 0

let copy h =
	{ size = length h;
		data = Array.copy h.data; }

(* next probing element modulo [base] *)
let next_probe base i = pred (if i == 0 then base else i)

(* test if [k] lies cyclically in the interval [i;j[ *)
let between k i j =
	if i < j then i <= k && k < j
	else i <= k || k < j

(* until [threshold] elements we resize only when the hashtable is full *)
let threshold = 100

(* check when hashtable is oversized / undersized *)
let oversized base size =
	size = base || size >= max threshold ((75 * base) / 100)
let undersized base size =
	size < (base / 3)

let iter f h =
	h.size <- - length h; (* locking for writes *)
	let base = Array.length h.data in
	for i = 0 to base - 1 do
		let e = h.data.(i) in
		if e != blank then f e
	done;
	h.size <- length h (* releasing locks *)

let fold f h init =
	h.size <- - length h; (* locking for writes *)
	let base = Array.length h.data in
	let accu = ref init in
	for i = 0 to base - 1 do
		let e = h.data.(i) in
		if e != blank then accu := f e !accu
	done;
	h.size <- length h; (* releasing locks *)
	!accu

(* Functorial interface *)

module type HashedType =
sig
	type t
	val equal: t -> t -> bool
	val hash: t -> int
	type elt
	val key: elt -> t
end

module type S =
sig
	type key
	type elt
	type t
	exception Locked
	val create: int -> t
	val clear: t -> unit
	val copy: t -> t
	val add: t -> elt -> unit
	val replace: t -> elt -> unit
	val remove: t -> key -> unit
	val mem : t -> key -> bool
	val find : t -> key -> elt
	val find_all : t -> key -> elt list
	val iter: (elt -> unit) -> t -> unit
	val fold: (elt -> 'b -> 'b) -> t -> 'b -> 'b
	val length: t -> int
	val equal: t -> t -> bool
	val hash: t -> int
	val iter2: (elt -> unit) -> t -> t -> unit
(*|  val print_stats: unit -> unit*)
end

module Make(H: HashedType): (S with type key = H.t and type elt = H.elt and type t = H.elt hashset) =
struct
	type key = H.t
	type elt = H.elt
	type t = elt hashset
	exception Locked
	let create = create
	let clear = clear
	let copy = copy
	let iter = iter
	let fold = fold
	let length = length
	
	(* we use a multiplier to avoid clustering *)
	let index base hash = (hash * 113) land max_int mod base
	
	(*|  let probe_count = ref 0                                             *)
	(*|  let seek_count = ref 0                                              *)
	(*|  let print_stats () =                                                *)
	(*|    Printf.fprintf stderr "probe count / seek count: %n / %n = %f.3\n"*)
	(*|      !probe_count                                                    *)
	(*|      !seek_count                                                     *)
	(*|      ((float_of_int !probe_count) /. (float_of_int !seek_count))     *)
	
	let rec re_insert ndata nbase elt i =
		if ndata.(i) == blank then ndata.(i) <- elt
		else re_insert ndata nbase elt (next_probe nbase i)
	
	let resize h =
		let odata = h.data in
		let obase = Array.length odata in
		let nbase = (min (max 1 (2 * h.size)) Sys.max_array_length) in
		if nbase = obase then
			failwith "consed: hash set cannot grow more"
		else begin
			let ndata = Array.create nbase blank in
			(* take special care to preserve the order for elements with the     *)
			(* same hashes                                                       *)
			let revisit = ref [] in
			for i = 1 to obase do
				let elt = odata.(obase - i) in
				if elt != blank then begin
					let hash = H.hash (H.key elt) in
					let oidx = index obase hash in
					let nidx = index nbase hash in
					if oidx >= obase - i then re_insert ndata nbase elt nidx
					else revisit := (nidx, elt) :: !revisit;
				end;
			done;
			List.iter (fun (nidx, elt) -> re_insert ndata nbase elt nidx) (List.rev !revisit);
			h.data <- ndata;
		end
	
	let rec mem_rec h base key i =
		let e = h.data.(i) in
		if e == blank then false
		else if H.equal (H.key e) key then true
		else mem_rec h base key (next_probe base i)
	
	let mem h key =
		let base = Array.length h.data in
		mem_rec h base key (index base (H.hash key))
	
	let rec find_rec h base key i =
		let e = h.data.(i) in
		if e == blank then raise Not_found
		else if H.equal (H.key e) key then e
		else find_rec h base key (next_probe base i)
	
	let find h key =
		let base = Array.length h.data in
		find_rec h base key (index base (H.hash key))
	
	let rec find_all_rec h base key i =
		let e = h.data.(i) in
		if e == blank then []
		else if H.equal (H.key e) key then
			e :: find_all_rec h base key (next_probe base i)
		else find_all_rec h base key (next_probe base i)
	
	let find_all h key =
		let base = Array.length h.data in
		find_all_rec h base key (index base (H.hash key))
	
	let rec add_rec h base elt i =
		let e = h.data.(i) in
		h.data.(i) <- elt;
		if e == blank then (
			h.size <- succ h.size;
			if oversized base h.size then resize h
		) else add_rec h base e (next_probe base i)
	
	let add h elt =
		if h.size < 0 then raise Locked;
		let key = H.key elt in
		let base = Array.length h.data in
		add_rec h base elt (index base (H.hash key))
	
	let rec replace_rec h base key elt i =
		let e = h.data.(i) in
		if e == blank then (
			h.data.(i) <- elt;
			h.size <- succ h.size;
			if oversized base h.size then resize h
		) else if H.equal (H.key e) key then
			h.data.(i) <- elt
		else replace_rec h base key elt (next_probe base i)
	
	let replace h elt =
		if h.size < 0 then raise Locked;
		let key = H.key elt in
		let base = Array.length h.data in
		replace_rec h base key elt (index base (H.hash key))
	
	let rec shift h base j i =
		let e = h.data.(i) in
		if e == blank || i = j then h.data.(j) <- blank
		else let k = index base (H.hash (H.key e)) in
			if between k i j then
				shift h base j (next_probe base i)
			else begin
				h.data.(j) <- e;
				shift h base i (next_probe base i)
			end
	
	let rec remove_rec h base key i =
		let e = h.data.(i) in
		if e == blank then ()
		else if H.equal (H.key e) key then begin
			shift h base i (next_probe base i);
			h.size <- pred h.size;
			if undersized base h.size then resize h
		end
		else remove_rec h base key (next_probe base i)
	
	let remove h key =
		if h.size < 0 then raise Locked;
		let base = Array.length h.data in
		remove_rec h base key (index base (H.hash key))
	
	(*|  let equal h1 h2 =                               *)
	(*|    length h1 == length h2 &&                     *)
	(*|    fold (fun e b -> b & mem h2 (H.key e)) h1 true*)
	
	let equal h1 h2 =
		(length h1) == (length h2) &&
		try
			iter (fun e -> if not (mem h2 (H.key e)) then raise Not_found) h1;
			true
		with Not_found -> false
	
	let hash h =
		fold (fun e h -> h + H.hash (H.key e)) h 0
	
	let iter2 f h1 h2 =
		if length h1 > length h2 then
			iter (fun e -> if mem h1 (H.key e) then f e) h2
		else
			iter (fun e -> if mem h2 (H.key e) then f e) h1
	
end
