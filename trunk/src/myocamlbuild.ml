open Ocamlbuild_plugin;;

open Command;;

dispatch begin function
		| After_rules ->
			  (* custom camlp4 preprocessing files *)
				flag ["ocaml"; "pp"; "use_typeext"] (A"libs/pa_typeext.cmo");
				dep ["ocaml"; "ocamldep"; "use_typeext"] ["libs/pa_typeext.cmo"];
		| _ -> ()
	end;;