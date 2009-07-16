(* hash maps on consed values based on hashtbl in the standard library *)

open Consed.T

(* We do dynamic hashing, and resize the table and rehash the elements     *)
(* when buckets become too long.                                           *)

type ('a, 'b) t =
  { mutable size: int;                        (* number of elements *)
    mutable data: ('a, 'b) bucketlist array } (* the buckets *)

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist

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

let length h = h.size

let safehash key = key.tag land max_int

let resize tbl =
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = min (2 * osize + 1) Sys.max_array_length in
  if nsize <> osize then begin
    let ndata = Array.create nsize Empty in
    let rec insert_bucket = function
        Empty -> ()
      | Cons(key, data, rest) ->
          let nidx = (safehash key) mod nsize in
          ndata.(nidx) <- Cons(key, data, ndata.(nidx));
          insert_bucket rest; (* tail recursive *)
    in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    tbl.data <- ndata;
  end

(*|let add h key info =                                 *)
(*|  let i = (safehash key) mod (Array.length h.data) in*)
(*|  let bucket = Cons(key, info, h.data.(i)) in        *)
(*|  h.data.(i) <- bucket;                              *)
(*|  h.size <- succ h.size;                             *)
(*|  if h.size > Array.length h.data lsl 1 then resize h*)

let remove h key =
  let rec remove_bucket = function
      Empty ->
        Empty
    | Cons(k, i, next) ->
        if k == key
        then begin h.size <- pred h.size; next end
        else Cons(k, i, remove_bucket next) in
  let i = (safehash key) mod (Array.length h.data) in
  h.data.(i) <- remove_bucket h.data.(i)

let rec find_rec key = function
    Empty ->
      raise Not_found
  | Cons(k, d, rest) ->
      if key == k then d else find_rec key rest

let find h key =
  match h.data.((safehash key) mod (Array.length h.data)) with
    Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
      if key == k1 then d1 else
        match rest1 with
          Empty -> raise Not_found
        | Cons(k2, d2, rest2) ->
            if key == k2 then d2 else
              match rest2 with
                Empty -> raise Not_found
              | Cons(k3, d3, rest3) ->
                  if key == k3 then d3 else find_rec key rest3

exception Break

let replace h key info =
  let rec replace_bucket = function
      Empty -> raise Break
    | Cons(k, i, next) ->
        if k == key
        then Cons(k, info, next)
        else Cons(k, i, replace_bucket next) in
  let i = (safehash key) mod (Array.length h.data) in
  let l = h.data.(i) in
  try
    h.data.(i) <- replace_bucket l
  with Break ->
      h.data.(i) <- Cons(key, info, l);
      h.size <- succ h.size;
      if h.size > Array.length h.data lsl 1 then resize h

let replace_f h key fresh repl =
  let rec replace_bucket = function
      Empty -> raise Break
    | Cons(k, i, next) ->
        if k == key
        then Cons(k, repl i, next)
        else Cons(k, i, replace_bucket next) in
  let i = (safehash key) mod (Array.length h.data) in
  let l = h.data.(i) in
  try
    h.data.(i) <- replace_bucket l
  with Break ->
      h.data.(i) <- Cons(key, fresh (), l);
      h.size <- succ h.size;
      if h.size > Array.length h.data lsl 1 then resize h

let mem h key =
  let rec mem_in_bucket = function
    | Empty ->
        false
    | Cons(k, d, rest) ->
        k == key || mem_in_bucket rest in
  mem_in_bucket h.data.((safehash key) mod (Array.length h.data))

let iter f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(k, d, rest) ->
        f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let iter2 f h1 h2 =
  if length h1 > length h2 then
    iter (fun k y ->
            try let x = find h1 k in f k x y
            with Not_found -> ()
      ) h2
  else
    iter (fun k x ->
            try let y = find h2 k in f k x y
            with Not_found -> ()
      ) h1

module S = Chashset

let iter_s f h s =
  if length h > S.cardinal s then
    S.iter (fun k ->
            try let x = find h k in f k x
            with Not_found -> ()
      ) s
  else iter (fun k x -> if S.mem s k then f k x) h

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons(k, d, rest) ->
        do_bucket rest (f k d accu) in
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
  type key
  type ('a, 'b) _t = ('a, 'b) t
  type 'a t = (key, 'a) _t
  val create: int -> 'a t
  val clear: 'a t -> unit
  val copy: 'a t -> 'a t
  val add: 'a t -> key -> 'a -> unit
  val remove: 'a t -> key -> unit
  val find: 'a t -> key -> 'a
  val replace : 'a t -> key -> 'a -> unit
  val replace_f : 'a t -> key -> (unit -> 'a) -> ('a -> 'a) -> unit
  val mem : 'a t -> key -> bool
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val iter2: (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
  module Set : Chashset.S with type elt = key
  val iter_s: (key -> 'a -> unit) -> 'a t -> Set.t -> unit         
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length: 'a t -> int
end

module Make(T: Type): (S with type key = T.t consed) =
struct
  type key = T.t consed
  type ('a, 'b) _t = ('a, 'b) t
  type 'a t = (key, 'a) _t
  let create = create
  let clear = clear
  let copy = copy
  let add = replace
  let remove = remove
  let find = find
  let replace = replace
  let replace_f = replace_f
(*|  let replace_f h key fresh repl =                  *)
(*|    try let x = find h key in replace h key (repl x)*)
(*|    with Not_found -> replace h key (fresh ())      *)
  let mem = mem
  let iter = iter
  let iter2 = iter2
  module Set = Chashset.Make (T)
  let iter_s = iter_s
  let fold = fold
  let length = length
end
