open Ocamlbuild_plugin;;

open Command;;

let cc = getenv "CC" ~default:"cc";;
let ar = getenv "AR" ~default: "ar";;
let javac = getenv "JAVAC" ~default: "javac";;
let javah = getenv "JAVAH" ~default: "javah";;
let jar = getenv "JAR" ~default: "jar";;
let ocamlopt = getenv "OCAMLOPT" ~default: "ocamlopt";;
let std_lib = "`" ^ ocamlopt ^ " -where`";;
let java_home = getenv "JAVA_HOME" ~default: "`/usr/libexec/java_home`";;
let jni_headers = java_home ^ "/../Headers/";;

let cflags = S[A"-std=c99"; A"-Wall"; A"-fPIC"; A"-fsigned-char"; A"-O2"];;
let ldflags = S[A"-lpthread"; A"-lm"; A"-lc"];;

(* collect all c files in java/jni folder and create a list of object      *)
(* files from them                                                         *)
let libobjfiles =
	List.map (Pathname.update_extension "o")
		(List.filter (fun f -> Pathname.check_extension f "c")
				(Array.to_list (Pathname.readdir "java/jni")));;

(* collect all java files in java/cb folder and remove extensions *)
let javaclasses =
	List.map Pathname.remove_extension
		(List.filter (fun f -> Pathname.check_extension f "java")
				(Array.to_list (Pathname.readdir "java/cb")));;

let plibobjfiles = List.map (fun s -> "%" ^ s) libobjfiles;;
let javafiles = List.map (fun s -> "cb/" ^ s ^ ".java") javaclasses;;
let pjavafiles = List.map (fun s -> "%" ^ s) javafiles;;
let pjniheaders = List.map (fun s -> "%jni/cb_" ^ s ^ ".h") javaclasses;;
let javaclassfiles = List.map (fun s -> "cb/" ^ s ^ ".class") javaclasses;;
let pjavaclassfiles = List.map (fun s -> "%" ^ s) javaclassfiles;;
let pkgjavaclasses = List.map (fun s -> "cb." ^ s) javaclasses;;

dispatch begin function
		| Before_rules -> (* override ocaml's C rules. *)
				let parallel files = List.map (fun f -> [f]) files in
				let err_circular file path =
					Printf.sprintf "Circular build detected (%s already seen in [%s])"
						file (String.concat "; " path)
				in
				let parse_deps file =
					let dir = Pathname.dirname file in
					let deps = string_list_of_file file in
					let deps = List.filter (fun d -> d <> "\\") deps in (* remove \ *)
					let deps = List.tl (List.tl deps) in					
					let correct d = if Pathname.dirname d = dir then d else dir / d in
					List.map correct deps
				in
				let deps_action dep prod env build =
					let c = env dep in
					let tags = tags_of_pathname c in
					Cmd (S [Sh cc; T tags;
						A "-MM"; A "-MG"; A "-MF"; Px (env prod); P c])
				in
				
				rule "cc: c -> c.depends"
					~dep:"%.c"
					~prod: "%.c.depends" (deps_action "%.c" "%.c.depends");
				
				rule "cc: h -> h.depends"
					~dep:"%.h"
					~prod:"%.h.depends" (deps_action "%.h" "%.h.depends");
				
				rule "cc: c & c.depends -> o"
					~deps:["%.c"; "%.c.depends"]
					~prod: "%.o"
					begin fun env build ->
								let c = env "%.c" in
								let rec build_transitive_deps = function
									| [] -> ()
									| (_, []) :: todo -> build_transitive_deps todo
									| (path, f :: rest) :: todo ->
											if List.mem f path then failwith (err_circular f path) else
												let deps = parse_deps (f ^ ".depends") in
												let dep_files = List.map (fun d -> d ^ ".depends") deps in
												List.iter Outcome.ignore_good (build (parallel deps));
												List.iter Outcome.ignore_good (build (parallel dep_files));
												build_transitive_deps (((f :: path), deps) :: (path, rest) :: todo)
								in
								build_transitive_deps [([],[c])];
								Cmd (S [Sh cc; cflags;
									T (tags_of_pathname c ++ "compile" ++ "cc");
									A "-c"; P c; A "-o"; Px (env "%.o");])
					end;
				
				rule "javac: java -> class files"
					~deps: pjavafiles
					~prods: pjavaclassfiles
					begin fun env _ ->
								Cmd(S[Sh javac; A"-source"; A"5"; A"-d"; Px (env "%.");
									Command.atomize_paths (List.map env pjavafiles)]);
					end;
				
				rule "javah: classes -> jni headers"
					~dep: "%cb.jar"
					~prods: pjniheaders
					begin fun env _ ->
								Cmd(S[Sh javah; A"-jni"; A"-classpath"; Px (env "%cb.jar"); A "-d"; P (env "%/jni");
									Command.atomize pkgjavaclasses])
					end;
				
				rule "cc: o -> jnilib"
					~deps: ("%../../clib/libcb.a" :: plibobjfiles)
					~prod: "%libjcb.jnilib"
					begin fun env _ ->
								let jnilib = env "%libjcb.jnilib" in
								Cmd(S[Sh cc; A"-framework"; A"JavaVM"; A"-bundle"; ldflags;
									A"-read_only_relocs"; A "suppress";
									A"-o"; Px jnilib; T (tags_of_pathname jnilib ++ "link" ++ "cc" ++ "jni");
									Command.atomize_paths (List.map env plibobjfiles)])
					end;
				
				rule "ar: o -> a"
					~deps: ["%cb.o"; "%cbcaml.o"]
					~prod: "%libcb.a"
					begin fun env _ ->
								Cmd(S[Sh ar; A "rcs"; A (env "%libcb.a"); A (env "%cb.o"); A (env "%cbcaml.o")])
					end;
				
				rule "jar: class files -> jar"
					~deps: pjavaclassfiles
					~prod: "%cb.jar"
					begin fun env _ ->
								Cmd(S[A "cd"; A (env "%"); Sh "&&"; Sh jar; A"cf"; Px (env "cb.jar");
									Command.atomize_paths (List.map env javaclassfiles)]);
					end;
				
				rule "jar: java files -> jar"
					~deps: pjavafiles
					~prod: "%cb-src.jar"
					begin fun env _ ->
								Cmd(S[A "cd"; A (env "%"); Sh "&&"; Sh jar; A"cf"; Px (env "cb-src.jar");
									Command.atomize_paths (List.map env javafiles)]);
					end;
				
				(* c dependencies on ocaml *)
				flag ["cc"; "compile"; "use_caml"] & S[A"-I"; Sh std_lib];
				flag ["cc"; "link"; "use_caml"] & S[Sh ("-L" ^ std_lib); A"-lasmrun"];
				
				(* c dependencies on unix *)
				flag ["cc"; "link"; "use_unix"] (A "-lunix");
				
				(* jni *)
				flag ["cc"; "compile"; "use_jni"] & S[A"-I"; Sh jni_headers];
				flag ["cc"; "link"; "use_unix"; "use_jni"] (P "/usr/lib/crt1.o");
				
				(* c dependencies on cb c library *)
				dep ["cc"; "compile"; "use_cb"] & ["clib/libcb.a"];
				flag ["cc"; "compile"; "use_cb"] & S[A"-I"; A"clib"];
				flag ["cc"; "link"; "use_cb"] & S[A"-L./clib"; A"-lcb"];
		
		| After_rules ->
		(* custom camlp4 preprocessing files *)
				flag ["ocaml"; "pp"; "use_typeext"] &
				S[A"-parser"; A"Camlp4OCamlRevisedParser"; A"-parser"; A"Camlp4OCamlParser"; A"libs/pa_typeext.cmo"];
				dep ["ocaml"; "ocamldep"; "use_typeext"] ["libs/pa_typeext.cmo"];
				(* creating C object files instead of executables *)
				flag ["ocaml"; "link"; "output_obj"] (A"-output-obj");
				(* generating debugging information *)
				flag ["ocaml"; "compile"] (A"-g");
				rule "output C obj"
					~deps:["%clib.cmx"; "%clib.o"]
					~prod:"%caml.o"
					(Ocamlbuild_pack.Ocaml_compiler.native_link "%clib.cmx" "%caml.o");
		| _ -> ()
	end;;