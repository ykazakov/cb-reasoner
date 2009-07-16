(* Weak hash maps; inspired by Weak module from the standard library *)

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

let hash x = hash_param 10 100 x

(* We do dynamic hashing, and resize the table and rehash the elements     *)
(* when buckets become too long.                                           *)

type 'a t =
  { mutable size: int;                 (* number of elements *)
    mutable data: 'a Weak.t;           (* the data *)
    mutable index: int list array;     (* the buckets of indexes *)
    mutable recycle: int list;         (* list of free indexes *)
    mutable rover: int;                (* for clining up *)
  }

(* The weak array [data] contains elements stored in the weak hash table;  *)
(* these elements can be accessed using hash and equality functions via    *)
(* array of buckets [index], which stores both the index [idx] of an       *)
(* element in [data] and its hash value [hash] according to formulas:      *)
(*| (1) [index.(i) mod (Weak.length data) = idx]  *)
(*| (2) [(index.(i) / (Weak.length data)) * (Array.length index) + i = hash ]  *)
(* The list [recycle] stores indexes [idx] that have been freed and can be reused. *)
(* The integer [rover] is an index [i] of the bucket in [index] that will be next searched *)
(* for indexes [idx] of dead elements in [data]; those indexes [idx] will*)
(* be removed from [index].*)

let create initial_size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { size = 0;
    data = Weak.create s;
    index = Array.make (s / 2) [];
    recycle = [];
    rover = 0;
  }

let clear h =
  Weak.fill h.data 0 (Weak.length h.data - 1) None;
  for i = 0 to Array.length h.index - 1 do
    h.index.(i) <- []
  done;
  h.size <- 0;
  h.recycle <- [];
  h.rover <- 0

let resize h =
(*|  Printf.printf "resizing\n";*)
  (*|  assert (h.recycle = []);*)
  let odtsize = Weak.length h.data in
  (*|  assert (h.size = odtsize);*)
  let oindex = h.index in
  let oidsize = Array.length oindex in
  let ndtsize = min (2 * odtsize + 1) Sys.max_array_length in
  let nidsize = ndtsize / 2 in
  if nidsize = oidsize then
    failwith "Weakhashset.Make: hash set cannot grow more"
  else begin
    let nindex = Array.create nidsize [] in
    let rec insert_bucket oi = function
        [] -> ()
      | ohidx :: rest ->
          let hash = (ohidx / odtsize) * oidsize + oi in
          let idx = ohidx mod odtsize in
          let nhidx = (hash / nidsize) * ndtsize + idx in
          let ni = hash mod nidsize in
          nindex.(ni) <- nhidx :: nindex.(ni);
          insert_bucket oi rest
    in
    for i = 0 to oidsize - 1 do
      insert_bucket i oindex.(i)
    done;
    let ndata = Weak.create ndtsize in
    Weak.blit h.data 0 ndata 0 odtsize;
    h.index <- nindex;
    h.data <- ndata;
    h.rover <- h.rover mod nidsize;
  end

let shrink_bucket h =
  let dtsize = Weak.length h.data in
  let rec shrink = function
    | [] -> []
    | n :: rest ->
        let idx = n mod dtsize in
        if Weak.check h.data idx then
          n :: shrink rest
        else begin
          h.recycle <- idx :: h.recycle;
          h.size <- pred h.size;
          shrink rest
        end;
  in h.index.(h.rover) <- shrink h.index.(h.rover);
  h.rover <- succ h.rover mod (Array.length h.index)

(* Functorial interface *)

module type HashedType =
sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
end

module type S =
sig
  type elt
  type t
  val create: int -> t
  val clear: t -> unit
  val merge: t -> elt -> elt * int
end

module Make(H: HashedType): (S with type elt = H.t) =
struct
  type elt = H.t
  type hashset = (elt * int) t
  type t = hashset
  let create = create
  let clear = clear
  
  let max_hash = max_int lsr 2
  
  let safehash elt = (H.hash elt) land max_hash
  
  let merge h elt =
    let hash = safehash elt in
    let idsize = Array.length h.index in
    let dtsize = Weak.length h.data in
    let k = hash / idsize in
    let rec find = function
      | [] -> None
      | n :: next ->
          if n / dtsize = k then
            match Weak.get h.data (n mod dtsize) with
            | None -> find next
            | Some (elt_tst, _) as elt_consed ->
                if H.equal elt elt_tst then elt_consed
                else find next
          else find next
    in
    let i = hash mod idsize in
    match find h.index.(i) with
    | None -> (* inserting [elt] *)
        shrink_bucket h;
        let idx = begin match h.recycle with
            | [] -> h.size
            | hd :: tail -> h.recycle <- tail; hd
          end in
        let elt_c = (elt, idx) in
        Weak.set h.data idx (Some elt_c);
        h.index.(i) <- (k * dtsize + idx) :: h.index.(i);
        h.size <- succ h.size;
        if h.size = dtsize then resize h;
        elt_c
    | Some elt_c -> elt_c
end
