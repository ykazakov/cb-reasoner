open Owl2

type t
val compute : ReasonerTBox.t -> Ontology.t -> t
val find_iter : t -> Class.t -> ((Class.t -> unit) -> unit) * ((Class.t -> unit) -> unit) * ((Class.t -> unit) -> unit)
val print_lisp_fast : ReasonerTBox.t -> Ontology.t -> out_channel -> unit
val print_lisp_slow : ReasonerTBox.t -> Ontology.t -> out_channel -> unit
val print_lisp : ReasonerTBox.t -> Ontology.t -> out_channel -> unit
val print_krss : ReasonerTBox.t -> Ontology.t -> out_channel -> unit
val print_fowl : ReasonerTBox.t -> Ontology.t -> out_channel -> unit
val print_statistics : ReasonerTBox.t -> Ontology.t -> out_channel -> unit