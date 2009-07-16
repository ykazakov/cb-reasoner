(* Weak hash maps; inspired by Weak module from the standard library *)

module T = struct
  type 'a consed = {
    data : 'a;
    tag : int;
  }
end

open T

type 'a t =
  { mutable size: int;                (* number of elements *)
    mutable content: 'a Weak.t;       (* the content *)
    mutable hashes: int array;        (* hashes *)
    mutable index: int array;         (* indices *)
    mutable recycle: int list;        (* list of free indicies *)
    mutable rover: int;               (* for cleaning up *)
  }

let create initial_size =
  let sw = min (max 1 initial_size) (Sys.max_array_length - 1) in
  let s = min (max 1 (4 * initial_size / 3)) Sys.max_array_length in
  { size = 0;
    content = Weak.create sw;
    hashes = Array.make s (- 1); (* the value by which we recognize unoccupied entry *)
    index = Array.make s 0;
    recycle = [];
    rover = 0;
  }

let resize h =
  let osize = Array.length h.index in
  let oindex = h.index in
  let ohashes = h.hashes in
  let nsize = min ((8 * h.size) / 3 + 1) Sys.max_array_length in
  if nsize = osize then
    failwith "consed: hash set cannot grow more"
  else begin
    let nindex = Array.create nsize 0 in
    let nhashes = Array.create nsize (- 1) in
    let rec insert idx i =
      if nhashes.(i) == (- 1) then begin
        Array.blit ohashes idx nhashes i 1;
        Array.blit oindex idx nindex i 1;
      end
      else insert idx ((i + 1) mod nsize)
    in
    for i = 0 to osize - 1 do
      if ohashes.(i) != (- 1) then insert i (ohashes.(i) mod nsize)
    done;
    h.hashes <- nhashes;
    h.index <- nindex;
    h.rover <- h.rover mod nsize;
    let ncontent = Weak.create (min (2 * h.size + 1) (Sys.max_array_length - 1)) in
    Weak.blit h.content 0 ncontent 0 h.size;
    h.content <- ncontent;
  end

let sweep h =
  let size = Array.length h.index in  
  let rec find_free i =
    if i = h.rover || h.hashes.(i) == (- 1) then i
    else find_free ((i + 1) mod size)
  in
  let rec move () =
    h.rover <- ((h.rover + 1) mod size);
    if h.hashes.(h.rover) != (- 1) then
      if Weak.check h.content h.index.(h.rover) then
        let i = find_free (h.hashes.(h.rover) mod size) in
        if i <> h.rover then begin
          (* moving *)
          Array.blit h.hashes h.rover h.hashes i 1;
          Array.blit h.index h.rover h.index i 1;
          h.hashes.(h.rover) <- (- 1);
          move ();
        end else move ()
      else begin
        (* deleting *)
        h.hashes.(h.rover) <- (- 1);
        h.size <- pred h.size;
        h.recycle <- h.index.(h.rover) :: h.recycle;
        move ()
      end
  in
  h.rover <- ((h.rover + 1) mod size);
  if h.hashes.(h.rover) != (- 1) &&
  not (Weak.check h.content h.index.(h.rover)) then
    begin
      (* deleting *)
      h.hashes.(h.rover) <- (- 1);
      h.size <- pred h.size;
      h.recycle <- h.index.(h.rover) :: h.recycle;
      move ()
    end

(* Functorial interface *)

module type HashedType = sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
end

module type S = sig
  type elt
  val cons: elt -> elt consed
end

module Make(H: HashedType): (S with type elt = H.t) =
struct
  type elt = H.t
  
  let safehash elt = (H.hash elt) land max_int
  
  let (h : H.t consed t) = create 31
  
  let cons elt =
    let hash = safehash elt in
    let size = Array.length h.index in
    let rec cons_rec i =
      if h.hashes.(i) == (- 1) then begin
        let idx = begin match h.recycle with
            | [] -> h.size
            | hd :: tail -> h.recycle <- tail; hd
          end in
        let elt_consed = {
          data = elt;
          tag = idx
        } in
        Weak.set h.content idx (Some elt_consed);
        h.hashes.(i) <- hash;
        h.index.(i) <- idx;
        h.size <- succ h.size;
        let l = Weak.length h.content in
        if h.size = l then resize h;
        sweep h;
        elt_consed
      end
      else if h.hashes.(i) == hash then
        match Weak.get h.content h.index.(i) with
        | Some elt_consed when H.equal elt elt_consed.data -> elt_consed
        | _ -> cons_rec ((i + 1) mod size)
      else cons_rec ((i + 1) mod size)
    in
    cons_rec (hash mod size)
end
