(* hash sets based on hashtbl in the standard library *)

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

let hash x = hash_param 10 100 x

(* We do dynamic hashing, and resize the table and rehash the elements     *)
(* when buckets become too long.                                           *)

type 'a t =
  { mutable size: int;                  (* number of elements *)
    mutable data: 'a bucketlist array } (* the buckets *)

and 'a bucketlist =
    Empty
  | Cons of 'a * 'a bucketlist

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
      | Cons(key, rest) ->
          let nidx = (hashfun key) mod nsize in
          ndata.(nidx) <- Cons(key, ndata.(nidx));
          insert_bucket rest (* tail recursive *)
    in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    tbl.data <- ndata;
  end

let remove h key =
  let rec remove_bucket = function
      Empty ->
        Empty
    | Cons(k, next) ->
        if compare k key = 0
        then begin h.size <- pred h.size; next end
        else Cons(k, remove_bucket next) in
  let i = (hash key) mod (Array.length h.data) in
  h.data.(i) <- remove_bucket h.data.(i)

let rec mem_rec key = function
    Empty -> false
  | Cons(k, rest) -> compare key k = 0 || mem_rec key rest

let mem h key =
  match h.data.((hash key) mod (Array.length h.data)) with
    Empty -> false
  | Cons(k1, rest1) -> compare key k1 = 0 ||
      match rest1 with
        Empty -> false
      | Cons(k2, rest2) -> compare key k2 = 0 ||
          match rest2 with
            Empty -> false
          | Cons(k3, rest3) -> compare key k3 = 0 ||
              mem_rec key rest3

let rec find_rec key = function
    Empty -> raise Not_found
  | Cons(k, rest) -> if compare key k = 0 then k else find_rec key rest

let find h key =
  match h.data.((hash key) mod (Array.length h.data)) with
    Empty -> raise Not_found
  | Cons(k1, rest1) -> if compare key k1 = 0 then k1 else
        match rest1 with
          Empty -> raise Not_found
        | Cons(k2, rest2) -> if compare key k2 = 0 then k2 else
              match rest2 with
                Empty -> raise Not_found
              | Cons(k3, rest3) -> if compare key k3 = 0 then k3 else
                    find_rec key rest3

let add h key =
  let rec mem_bucket_rec = function
      Empty -> false
    | Cons(k, next) -> compare k key = 0 || mem_bucket_rec next
  in
  let mem_bucket = function
      Empty -> false
    | Cons(k1, rest1) -> compare key k1 = 0 ||
        match rest1 with
          Empty -> false
        | Cons(k2, rest2) -> compare key k2 = 0 ||
            match rest2 with
              Empty -> false
            | Cons(k3, rest3) ->
                compare key k3 = 0 || mem_bucket_rec rest3
  in
  let i = (hash key) mod (Array.length h.data) in
  let l = h.data.(i) in
  if not (mem_bucket l) then begin
    h.data.(i) <- Cons(key, l);
    h.size <- succ h.size;
    if h.size > Array.length h.data lsl 1 then resize hash h
  end

let iter f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(k, rest) ->
        f k; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons(k, rest) ->
        do_bucket rest (f k accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu

(* Functorial interface *)

module type HashedType =
sig
  type t
  type v
  val equal: t -> t -> bool
  val hash: t -> int
  val key: v -> t
end

module type S =
sig
  type key
  type elt
  type t
  val create: int -> t
  val clear: t -> unit
  val copy: t -> t
  val add: t -> elt -> unit
  val remove: t -> key -> unit
  val mem : t -> key -> bool
  val find : t -> key -> elt
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val length: t -> int
end

module Make(H: HashedType): (S with type key = H.t and type elt = H.v) =
struct
  type key = H.t
  type elt = H.v
  type hashtbl = elt t
  type t = hashtbl
  let create = create
  let clear = clear
  let copy = copy
  
  let safehash key = (H.hash key) land max_int
  
  let remove h key =
    let rec remove_bucket = function
        Empty ->
          Empty
      | Cons(elt, next) ->
          if H.equal (H.key elt) key
          then begin h.size <- pred h.size; next end
          else Cons(elt, remove_bucket next) in
    let i = (safehash key) mod (Array.length h.data) in
    h.data.(i) <- remove_bucket h.data.(i)
  
  let rec mem_bucket_rec key = function
      Empty -> false
    | Cons(elt, rest) -> H.equal key (H.key elt) ||
        mem_bucket_rec key rest
  
  let mem_bucket key = function
      Empty -> false
    | Cons(elt1, rest1) ->
        H.equal key (H.key elt1) ||
        match rest1 with
          Empty -> false
        | Cons(elt2, rest2) ->
            H.equal key (H.key elt2) ||
            match rest2 with
              Empty -> false
            | Cons(elt3, rest3) ->
                H.equal key (H.key elt3) || mem_bucket_rec key rest3
  
  let mem h key =
    mem_bucket key h.data.((safehash key) mod (Array.length h.data))
  
  let rec find_rec key = function
      Empty -> raise Not_found
    | Cons(elt, rest) -> if H.equal key (H.key elt) then elt else
          find_rec key rest
  
  let find h key =
    match h.data.((safehash key) mod (Array.length h.data)) with
      Empty -> raise Not_found
    | Cons(elt1, rest1) ->
        if H.equal key (H.key elt1) then elt1 else
          match rest1 with
            Empty -> raise Not_found
          | Cons(elt2, rest2) ->
              if H.equal key (H.key elt2) then elt2 else
                match rest2 with
                  Empty -> raise Not_found
                | Cons(elt3, rest3) ->
                    if H.equal key (H.key elt3) then elt3 else
                      find_rec key rest3
  
  let add h elt =
    let key = H.key elt in
    let i = (safehash key) mod (Array.length h.data) in
    let l = h.data.(i) in
    if not (mem_bucket key l) then begin
      h.data.(i) <- Cons(elt,l);
      h.size <- succ h.size;
      if h.size > Array.length h.data lsl 1 then resize (fun elt -> safehash (H.key elt)) h
    end
  let iter = iter
  let fold = fold
  let length = length
end
