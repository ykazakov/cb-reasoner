open Owl

(* record stored for every clss *)
type record = {
    (* equivalent classes *)
    mutable equiv : ClassExpression.t list;
    (* direct subclasses *)
    mutable subs : ClassExpression.t list;
    (* direct superclasses *)
    mutable sups : ClassExpression.t list;
}

type t
(* get record for a class including top and bottom *)
val get_record : t -> ClassExpression.t -> record

val compute : ?message: string -> Progress_tracker.t list ->
    Saturation.t -> Ontology.t -> t
val print_fowl : ?message: string -> Progress_tracker.t list ->
    Saturation.t -> Ontology.t -> out_channel -> unit
