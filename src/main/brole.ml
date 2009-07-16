(* A module for manipulating with roles represented as a pair [(a, ata)]   *)
(* where [a] is an atomic role and [ata] is the boolean encoding whether   *)
(* ther role is inverse: [(a,true)] represents role [a], [(a,false)]       *)
(* represents role [inverse a]                                             *)

open OwlSyntax
open Consed.T

type t = ObjectProperty.t * bool

(* the function of converting roles from standard syntax to ther           *)
(* representation                                                          *)
let to_elt r = match r.data with
  | ObjectPropertyExpression.Constructor.ObjectProperty ar -> ar, true
  | ObjectPropertyExpression.Constructor.InverseObjectProperty ar -> ar, false

let inv (a, ata) = a, (not ata)

let str (a, ata) = Krss.str_of_object_property a ^ (if ata then "" else "-")

module Set = struct
  module S = ObjectProperty.Set
  type elt = t
  
  (* every set of roles is implemented as a pair of atomic sets of role;   *)
  (* the first represent the set of atomic roles; the second is the set of *)
  (* inversed atomic roles                                                 *)
  type t = S.t * S.t
  
  let empty = (S.empty, S.empty)
  
  let is_empty (sa, si) = S.is_empty sa && S.is_empty si
  
  let mem (a, ata) (sa, si) =
    if ata then S.mem a sa else S.mem a si
  
  let add (a, ata) (sa, si) =
    if ata then (S.add a sa, si) else (sa, S.add a si)
  
  (*|  let add_f (a, ata) fresh replace (sa, si) =                                         *)
  (*|    if ata then (S.add_f a fresh replace sa, si) else (sa, S.add_f a fresh replace si)*)
  
  let singleton (a, ata) =
    if ata then (S.singleton a, S.empty) else (S.empty, S.singleton a)
  
  let is_singleton (sa, si) =
    (S.is_singleton sa && S.is_empty si) || (S.is_empty sa && S.is_singleton si)
  
  let remove (a, ata) (sa, si) =
    if ata then (S.remove a sa, si) else (sa, S.remove a si)
  
  let union (asa, asi) (bsa, bsi) =
    (S.union asa bsa, S.union asi bsi)
  
  let inter (asa, asi) (bsa, bsi) =
    (S.inter asa bsa, S.inter asi bsi)
  
  let diff (asa, asi) (bsa, bsi) =
    (S.diff asa bsa, S.diff asi bsi)
  
  let is_subset (asa, asi) (bsa, bsi) =
    S.is_subset asa bsa && S.is_subset asi bsi
  
  let iter f (sa, si) =
    S.iter (fun a -> f (a, true) ) sa;
    S.iter (fun a -> f (a, false) ) si;
  ;;
  
  let iter2 f (s1a, s1i) (s2a, s2i) =
    S.iter2 (fun a -> f (a, true) ) s1a s2a;
    S.iter2 (fun a -> f (a, false) ) s1i s2i;
  ;;
  
  let iter_diff f (s1a, s1i) (s2a, s2i) =
    S.iter_diff (fun a -> f (a, true) ) s1a s2a;
    S.iter_diff (fun a -> f (a, false) ) s1i s2i;
  ;;
  
  let fold f (sa, si) accu =
    S.fold (fun a -> f (a, true)) sa
      (S.fold (fun a -> f (a, false)) si accu)
  
  let for_all f (sa, si) =
    S.for_all (fun a -> f (a, true)) sa &&
    S.for_all (fun a -> f (a, false)) si
  
  let exists f (sa, si) =
    S.exists (fun a -> f (a, true)) sa ||
    S.exists (fun a -> f (a, false)) si
  
  let filter f (sa, si) =
    (S.filter (fun a -> f (a, true)) sa,
      S.filter (fun a -> f (a, false)) si)
  
  let partition f (sa, si) =
    let sat, saf = S.partition (fun a -> f (a, true)) sa in
    let sit, sif = S.partition (fun a -> f (a, false)) si in
    ((sat, sit), (saf, sif))
  
  let cardinal (sa, si) =
    S.cardinal sa + S.cardinal si
  
  let elements (sa, si) =
    List.rev_append
      (List.map (fun a -> (a, true)) (S.elements sa))
      (List.map (fun a -> (a, false)) (S.elements si))
  
  let choose (sa, si) =
    try (S.choose sa, true)
    with Not_found -> (S.choose sa, false)
  
  let equal (s1a, s1i) (s2a, s2i) =
    S.equal s1a s2a && S.equal s1i s2i
  
  let compare (s1a, s1i) (s2a, s2i) =
    let c = S.compare s1a s2a in
    if c <> 0 then c else S.compare s1i s2i
  
  let right (sa, si) (s1a, s1i) (s2a, s2i) =
    if S.equal s1a s2a
    then S.right si s1i s2i
    else S.right sa s1a s2a
  
  (* obtaining a set where all roles are inverted can be done quickly *)
  let inv = function
    | (sa, si) as s -> if sa == si then s else	(si, sa)
  
  (* closing the set under symmetry is as well quickly *)
  let symm = function
    | (sa, si) as s -> if sa = si then s else let ss = S.union sa si in (ss, ss)
  
  (* extracting the set of atomic roles *)
  let atomic (sa, si) = S.union sa si
  
  let str s = fold (fun a s -> s ^ str a) s "[ " ^ "]"
end

module Map = struct
  module M = ObjectProperty.Map
  type key = t
  type 'a t = 'a M.t * 'a M.t
  
  let empty = M.empty, M.empty
  
  let is_empty (ma, mi) = M.is_empty ma && M.is_empty mi
  
  let singleton (a, ata) x =
    if ata then (M.singleton a x, M.empty) else (M.empty, M.singleton a x)
  
  let is_singleton (ma, mi) =
    (M.is_singleton ma && M.is_empty mi) || (M.is_empty ma && M.is_singleton mi)
  
  let add (a, ata) x (ma, mi) =
    if ata then (M.add a x ma, mi) else (ma, M.add a x mi)
  
  let process (a, ata) f (ma, mi) =
    if ata then (M.process a f ma, mi) else (ma, M.process a f mi)
  
  let replace (a, ata) x (ma, mi) =
    if ata then (M.replace a x ma, mi) else (ma, M.replace a x mi)
  
  let find (a, ata) (ma, mi) =
    if ata then M.find a ma else M.find a mi
  
  let remove (a, ata) (ma, mi) =
    if ata then (M.remove a ma, mi) else (ma, M.remove a mi)
  
  let mem (a, ata) (ma, mi) =
    if ata then M.mem a ma else M.mem a mi
  
  let iter f (ma, mi) =
    M.iter (fun a -> f (a, true)) ma;
    M.iter (fun a -> f (a, false)) mi;
  ;;
  
  let iter2 f (m1a, m1i) (m2a, m2i) =
    M.iter2 (fun a -> f (a, true)) m1a m2a;
    M.iter2 (fun a -> f (a, false)) m1i m2i;
  ;;
  
  let union f (m1a, m1i) (m2a, m2i) =
    (M.union f m1a m2a, M.union f m1i m2i)
  
  let inter f (m1a, m1i) (m2a, m2i) =
    (M.inter f m1a m2a, M.inter f m1i m2i)
  
  let diff (m1a, m1i) (m2a, m2i) =
    (M.diff m1a m2a, M.diff m1i m2i)
  
  let map f (ma, mi) = (M.map f ma, M.map f mi)
  
  let mapi f (ma, mi) =
    (M.mapi (fun a -> f (a, true)) ma,
      M.mapi (fun a -> f (a, false)) mi)
  
  let fold f (ma, mi) accu =
    M.fold (fun a -> f (a, true)) ma
      (M.fold (fun a -> f (a, false)) mi accu)
  
  let choose (ma, mi) =
    if M.is_empty ma then let a, x = M.choose mi in (a, false), x
    else let a, x = M.choose ma in (a, true), x
  
  let equal eq (m1a, m1i) (m2a, m2i) =
    M.equal eq m1a m2a && M.equal eq m1i m2i
  
  let compare comp (m1a, m1i) (m2a, m2i) =
    let c = M.compare comp m1a m2a in
    if c <> 0 then c else M.compare comp m1i m2i
  
  let right eq rt (ma, mi) (m1a, m1i) (m2a, m2i) =
    if M.equal eq m1a m2a then M.right eq rt mi m1i m2i
    else M.right eq rt ma m1a m2a
  
  module Set = Set
  
  let iter_s f (ma, mi) (sa, si) =
    M.iter_s (fun a -> f (a, true)) ma sa;
    M.iter_s (fun a -> f (a, false)) mi si;
  ;;
  
  let inv (ma, mi) = (mi, ma)
  
  (* flattaning the map to atomic roles resolving the conflicts if         *)
  (* necessary                                                             *)
  let atomic f (sa, si) = M.union f sa si
  
end

module HSet = struct
  module S = ObjectProperty.HSet
  type elt = t
  
  (* every set of roles is implemented as a pair of atomic sets of role;   *)
  (* the first represent the set of atomic roles; the second is the set of *)
  (* inversed atomic roles                                                 *)
  type t = S.t * S.t
  
  let create i = (S.create (i / 2), S.create (i / 2))
  let clear (sa, si) = S.clear sa; S.clear si
  let copy (sa, si) = (S.copy sa, S.copy si)
  let add (sa, si) (a, ata) = if ata then S.add sa a else S.add si a
  let remove (sa, si) (a, ata) = if ata then S.remove sa a else S.remove si a
  let mem (sa, si) (a, ata) = if ata then S.mem sa a else S.mem si a
  let iter f (sa, si) = S.iter (fun a -> f (a, true)) sa; S.iter (fun a -> f (a, false)) si
  let iter2 f (s1a, s1i) (s2a, s2i) = S.iter2 (fun a -> f (a, true)) s1a s2a; S.iter2 (fun a -> f (a, false)) s1i s2i
  let fold f (sa, si) accu =
    S.fold (fun a -> f (a, true)) sa
      (S.fold (fun a -> f (a, false)) si accu)
  let cardinal (sa, si) = S.length sa + S.length si
  let equal (s1a, s1i) (s2a, s2i) = S.equal s1a s2a && S.equal s1i s2i
  let hash (sa, si) = S.hash sa + S.hash si
end