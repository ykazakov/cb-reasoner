(* hash sets on consed values based on hashtbl in the standard library *)

open Consed

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
    h.data.(i) <- Empty
  done;
  h.size <- 0

let copy h =
  { size = h.size;
    data = Array.copy h.data }

let cardinal h = h.size

let safehash key = key.tag land max_int

let resize tbl =
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = min (3 * osize + 1) Sys.max_array_length in
  if nsize <> osize then begin
    let ndata = Array.create nsize Empty in
    let rec insert_bucket = function
        Empty -> ()
      | Cons(key, rest) ->
          let nidx = (safehash key) mod nsize in
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
        if k == key
        then begin h.size <- pred h.size; next end
        else Cons(k, remove_bucket next) in
  let i = (safehash key) mod (Array.length h.data) in
  h.data.(i) <- remove_bucket h.data.(i)

let rec mem_rec key = function
    Empty -> false
  | Cons(k, rest) -> key == k || mem_rec key rest

let mem h key =
  match h.data.((safehash key) mod (Array.length h.data)) with
    Empty -> false
  | Cons(k1, rest1) -> key == k1 ||
      match rest1 with
        Empty -> false
      | Cons(k2, rest2) -> key == k2 ||
          match rest2 with
            Empty -> false
          | Cons(k3, rest3) -> key == k3 ||
              mem_rec key rest3

let rec find_rec key = function
    Empty -> raise Not_found
  | Cons(k, rest) -> if key == k then k else find_rec key rest

let find h key =
  match h.data.((safehash key) mod (Array.length h.data)) with
    Empty -> raise Not_found
  | Cons(k1, rest1) -> if key == k1 then k1 else
        match rest1 with
          Empty -> raise Not_found
        | Cons(k2, rest2) -> if key == k2 then k2 else
              match rest2 with
                Empty -> raise Not_found
              | Cons(k3, rest3) -> if key == k3 then k3 else
                    find_rec key rest3

let add h key =
  let rec mem_bucket_rec = function
      Empty -> false
    | Cons(k, next) -> k == key || mem_bucket_rec next
  in
  let mem_bucket = function
      Empty -> false
    | Cons(k1, rest1) -> key == k1 ||
        match rest1 with
          Empty -> false
        | Cons(k2, rest2) -> key == k2 ||
            match rest2 with
              Empty -> false
            | Cons(k3, rest3) -> key == k3 ||
                mem_bucket_rec rest3
  in
  let i = (safehash key) mod (Array.length h.data) in
  let l = h.data.(i) in
  if not (mem_bucket l) then begin
    h.data.(i) <- Cons(key, l);
    h.size <- succ h.size;
    if h.size > Array.length h.data lsl 1 then resize h
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

let choose h =
  let rec choose_rec i =
    if i = h.size then raise Not_found
    else match h.data.(i) with
      | Empty -> choose_rec (i + 1)
      | Cons (k, _) -> k
  in choose_rec 0

let iter2 f h1 h2 =
  if cardinal h1 > cardinal h2 then
    iter (fun elt -> if mem h1 elt then f elt) h2
  else
    iter (fun elt -> if mem h2 elt then f elt) h1

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

module type Type = sig
  type t
end

module type S =
sig
  type elt
  type 'a _t = 'a t
  type t = elt _t
  val create: int -> t
  val singleton: int -> elt -> t
  val clear: t -> unit
  val copy: t -> t
  val add: t -> elt -> unit
  val remove: t -> elt -> unit
  val mem : t -> elt -> bool  
  val iter: (elt -> unit) -> t -> unit
  val iter2: (elt -> unit) -> t -> t -> unit
  val fold: (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val cardinal: t -> int
  val equal: t -> t -> bool
  val hash: t -> int
end

module Make(T: Type): (S with type elt = T.t consed) =
struct
  type elt = T.t consed
  type 'a _t = 'a t
  type t = elt _t
  let create = create
  let singleton i elt =
    let h = create i in add h elt; h
  let clear = clear
  let copy = copy
  let add = add
  let remove = remove
  let mem = mem  
  let iter = iter
  let iter2 = iter2
  let fold = fold
  let cardinal = cardinal
  let equal h1 h2 =
    cardinal h1 = cardinal h2 &&
    fold (fun elt b -> b && mem h2 elt) h1 true
  let hash h =
    fold (fun elt h -> h + elt.tag) h 0
end