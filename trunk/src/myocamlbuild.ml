open Ocamlbuild_plugin;;

open Command;;

dispatch begin function
		| After_rules ->
		    (* custom camlp4 preprocessing files *)
				flag ["ocaml"; "pp"; "use_typeext"] & S[A"-parser"; A"Camlp4OCamlRevisedParser"; A"-parser"; A"Camlp4OCamlParser"; A"libs/pa_typeext.cmo"];
				dep ["ocaml"; "ocamldep"; "use_typeext"] ["libs/pa_typeext.cmo"];
		| _ -> ()
	end;;