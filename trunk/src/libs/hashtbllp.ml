(* hash tables using array and linear probing for resolving hash collision *)
(* see [1] p.526                                                           *)
(*| [1] Donald E. Knuth, The Art of Computer Programming, Volume 3,        *)
(*| Sorting and Searching, Second Edition                                  *)

(* We do dynamic hashing, and resize when the array becomes too long. *)

type ('a, 'b) hashtbl =
  { mutable size: int;       (* the number of elements *)
    mutable keys: 'a array;  (* keys *)
    mutable data: 'b array } (* values *)

(* Unique blank element of any type. Works by magic! Must be allocated in  *)
(* heap.                                                                   *)
let blank = Obj.magic (Obj.repr [0]);;

let create initial_size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { size = 0; keys = Array.create s blank; data = Array.create s blank }

let clear h =
  Array.fill h.keys 0 (Array.length h.keys) blank;
  Array.fill h.data 0 (Array.length h.data) blank;
  h.size <- 0

let copy h =
  { size = h.size;
    keys = Array.copy h.keys;
    data = Array.copy h.data }

let length h = h.size

(* next probing element modulo [base] *)
let next_probe base i = if i == 0 then pred base else pred i

(* test if [k] lies cyclically in the interval [i;j[ *)
let between k i j =
  if i < j then i <= k && k < j
  else i <= k || k < j

(* until [threshold] elements we resize only when the hashtable is full *)
let threshold = 100

(* check when hashtable is oversized / undersized *)
let oversized base size =
  size = base || size >= max threshold ((85 * base) / 100)
let undersized base size =
  size < (base / 3)

let iter f h =
  let base = Array.length h.keys in
  for i = 0 to base - 1 do
    let key = h.keys.(i) in
    if key != blank then f key h.data.(i)
  done

let fold f h init =
  let base = Array.length h.keys in
  let accu = ref init in
  for i = 0 to base - 1 do
    let key = h.keys.(i) in
    if key != blank then accu := f key h.data.(i) !accu
  done;
  !accu

(* Functorial interface *)

module type HashedType =
sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

module type S =
sig
  type key
  type 'a t
  val create : int -> 'a t
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length : 'a t -> int
end

module Make(H: HashedType): (S with type key = H.t) =
struct
  type key = H.t
  type 'a t = (key, 'a) hashtbl
  let create = create
  let clear = clear
  let copy = copy
  let iter = iter
  let fold = fold
  let length = length
  
  (* we use a multiplier to avoid clustering *)
  let index base hash =
    ((hash * 113) land max_int) mod base
  
  let resize h =    
    let okeys = h.keys in
    let odata = h.data in
    let obase = Array.length okeys in
    let nbase = (min (max 1 (2 * h.size)) Sys.max_array_length) in
    if nbase = obase then
      failwith "consed: hash set cannot grow more"
    else begin
      let nkeys = Array.create nbase blank in
      let ndata = Array.create nbase blank in
      let rec insert i key x =
        if nkeys.(i) == blank then begin
          nkeys.(i) <- key;
          ndata.(i) <- x;
        end
        else insert (next_probe nbase i) key x
      in
      (* take special care to preserve the order for elements with the     *)
      (* same hashes                                                       *)
      let revisit = ref [] in
      for i = 1 to obase do
        let key = okeys.(obase - i) in
        if key != blank then begin
          let x = odata.(obase - i) in
          let hash = H.hash key in
          let oidx = index obase hash in
          let nidx = index nbase hash in
          if oidx >= obase - i then insert nidx key x
          else revisit := (nidx, key, x) :: !revisit;
        end;
      done;
      List.iter (fun (nidx, key, x) -> insert nidx key x) (List.rev !revisit);
      h.keys <- nkeys;
      h.data <- ndata;            
    end
  
  let mem h key =
    let base = Array.length h.keys in
    let rec mem_from i =
      let jey = h.keys.(i) in
      if jey == blank then false
      else if H.equal jey key then true
      else mem_from (next_probe base i)
    in
    mem_from (index base (H.hash key))
  
  let find h key =
    let base = Array.length h.keys in
    let rec find_from i =
      let jey = h.keys.(i) in
      if jey == blank then raise Not_found
      else if H.equal jey key then h.data.(i)
      else find_from (next_probe base i)
    in
    find_from (index base (H.hash key))
  
  let find_all h key =
    let base = Array.length h.keys in
    let rec find_all_from i =
      let jey = h.keys.(i) in
      if jey == blank then []
      else if H.equal jey key then
        h.data.(i) :: find_all_from (next_probe base i)
      else find_all_from (next_probe base i)
    in
    find_all_from (index base (H.hash key))
  
  let add h key x =    
    let base = Array.length h.keys in
    let rec add_rec i key x =
      let jey = h.keys.(i) in
      h.keys.(i) <- key;
      if jey == blank then (
        h.data.(i) <- x;
        h.size <- succ h.size;
        if oversized base h.size then resize h
      ) else (
        let y = h.data.(i) in
        h.data.(i) <- x;
        add_rec (next_probe base i) jey y
      )
    in
    add_rec (index base (H.hash key)) key x    
  
  let replace h key x =
    let base = Array.length h.keys in
    let rec add_rec i =
      let jey = h.keys.(i) in
      if jey == blank then (
        h.keys.(i) <- key;
        h.data.(i) <- x;
        h.size <- succ h.size;
        if oversized base h.size then resize h
      ) else if H.equal jey key then (
        h.keys.(i) <- key;
        h.data.(i) <- x;
      ) else add_rec (next_probe base i)
    in
    add_rec (index base (H.hash key))
  
  let remove h key =
    (* when removing, we delete only keys, data is irrelevant *)
    let base = Array.length h.keys in
    let rec shift i j =
      let jey = h.keys.(i) in
      if jey == blank || i = j then h.keys.(j) <- blank
      else let k = index base (H.hash jey) in
        if between k i j then
          shift (next_probe base i) j
        else begin
          h.keys.(j) <- jey;
          h.data.(j) <- h.data.(i);
          shift (next_probe base i) i
        end
    in
    let rec rm_rec i =
      let jey = h.keys.(i) in
      if jey == blank then ()
      else if H.equal jey key then begin
        shift (next_probe base i) i;
        h.size <- pred h.size;
        if undersized base h.size then resize h
      end
      else rm_rec (next_probe base i)
    in
    rm_rec (index base (H.hash key))
  
  let iter2 f h1 h2 =
    if length h1 > length h2 then
      iter (fun key y ->
              List.iter (fun x -> f key x y ) (find_all h1 key)
        ) h2
    else
      iter (fun key x ->
              List.iter (fun y -> f key x y ) (find_all h2 key)
        ) h1
  
end

