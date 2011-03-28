open Owl

(* nodes of taxonomies represent classes of equivalent classes and keep    *)
(* information about the direct sub classes and superclasses               *)
module Node : sig
(* the type of node *)
	type t
	(* [get_classes n] returns the array of the equivalent classes of the    *)
	(* node [n]; the classes are listed in the alphabetical order, where     *)
	(* [owl:Thing] comes first and [owl:Nothing] comes last                  *)
	val get_classes : t -> ClassExpression.t array
	(* [get_child_nodes n] returns the array of nodes representing direct    *)
	(* subclasses of the classes in the node [n]; they are listed in the     *)
	(* alphabetical order of the representative (smallest class) of the      *)
	(* nodes, where [owl:Thing] comes first and [owl:Nothing] comes last     *)
	val get_child_nodes : t -> t array
	(* [get_parent_nodes n] returns the array of nodes representing direct   *)
	(* superclasses of the classes in the node [n]; they are listed in the   *)
	(* alphabetical order of the representative (smallest class) of the      *)
	(* nodes, where [owl:Thing] comes first and [owl:Nothing] comes last     *)
	val get_parent_nodes : t -> t array
end

(* the type of taxonomy *)
type t
(* get the node for a class including [owl:Thing] and [owl:Nothing] *)
val find_node : t -> ClassExpression.t -> Node.t
(* get the array of all nodes in the taxonomy: the list starts with the    *)
(* node for [owl:Thing], ends with the node for [owl:Nothing] and list     *)
(* other nodes in the alphabetical order of its representatives (smallest  *)
(* elements)                                                               *)
val get_nodes : t -> Node.t array

(* computing class taxonomy from the saturation *)
val compute : ?message: string -> Progress_tracker.t list ->
Saturation.t -> Ontology.t -> t