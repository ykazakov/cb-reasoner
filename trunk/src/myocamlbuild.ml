open Ocamlbuild_plugin
open Command

(** helper functions *)
(* splitting strings using a delimeter character *)
let rec strsplit sep str accu len =
	try let idx = String.rindex_from str (len - 1) sep in
		strsplit sep str ((String.sub str (idx + 1) (len - 1 - idx)) :: accu) idx
	with Not_found -> (String.sub str 0 len) :: accu
let strsplit sep str = strsplit sep str [] (String.length str)

(** initializing variables from the environment *)
let host = Ocamlbuild_pack.Ocamlbuild_Myocamlbuild_config.system
let arch = getenv "ARCH" ~default:
		Ocamlbuild_pack.Ocamlbuild_Myocamlbuild_config.arch
let target = try Sys.getenv "PLATFORM"
	with Not_found -> match host with
			| "linux" | "linux_elf" -> "linux"
			| "cygwin" | "mingw" | "win64" | "win32" -> "windows"
			| "macosx" -> "macosx"
			| _ -> invalid_arg "Please set target platform PLATFORM=windows|linux|macosx"
let toolchain = getenv "TOOLCHAIN" ~default: (
			match host with
			| "linux_elf" -> "gcc"
			| "cygwin" | "mingw" -> "gcc"
			| "win64" | "win32" -> "msvc"
			| _ -> "gcc"
		)
let _ = Printf.fprintf stdout "Building for host: %s, target: %s, arch: %s\n" host target arch
let gcc = try Sys.getenv "GCC"
	with Not_found -> match host, arch with
			| "macosx", "i386" -> "gcc -m32"
			| "macosx", "amd64" -> "gcc -m64"
			| _ -> "gcc"
let ar = getenv "AR" ~default: "ar"
let std_lib = getenv "OCAMLSTDLIB" ~default: (Ocamlbuild_pack.Ocamlbuild_Myocamlbuild_config.libdir)
let ocamlbin = Ocamlbuild_pack.Ocamlbuild_Myocamlbuild_config.bindir
let ocamlopt = getenv "OCAMLOPT" ~default: (ocamlbin / "ocamlopt")
let camlp4o = getenv "CAMPLP4O" ~default: (ocamlbin / "camlp4o")
let camlp4orf = getenv "CAMPLP4O" ~default: (ocamlbin / "camlp4orf")
(* java stuff *)
let java_home = try Sys.getenv "JAVA_HOME"
	with Not_found -> match target with
			| "linux" -> "/usr/lib/jvm/java-6-openjdk"
			| "macosx" -> "`/usr/libexec/java_home`"
			| _ -> invalid_arg "Please set JAVA_HOME"
let jni_includes = try Sys.getenv "JNI_INCLUDE"
	with Not_found -> match target with
			| "linux" -> java_home / "include"
			| "macosx" -> java_home / "../Headers"
			| "windows" ->
					java_home / "include" ^ ";" ^
					java_home / "include/win32"
			| _ -> invalid_arg "Please set JNI_INCLUDE"
let jni_includes = strsplit ';' jni_includes
let java_bin = java_home / "bin"
let javac = try Sys.getenv "JAVAC"
	with Not_found -> java_bin / "javac"
let javah = try Sys.getenv "JAVAH"
	with Not_found -> java_bin / "javah"
let jar = try Sys.getenv "JAR"
	with Not_found -> java_bin / "jar"

let jniflags = match target, arch, toolchain with
	| "linux", _, _ -> [A"-shared"]
	| "windows", _, "gcc" ->
			List.map (fun flg -> S[A"-link"; flg])
				[A"-shared"; A"-Wall"; A"-D_JNI_IMPLEMENTATION_"; A"-Wl,--kill-at"]
	| "windows", _, "msvc" -> [A"-link"; A"/DLL"]
	| "macosx", "i386", _ ->
			[A"-framework"; A"JavaVM"; A"-bundle"; A"-read_only_relocs"; A"suppress"]
	| "macosx", "amd64", _ -> [A"-framework"; A"JavaVM"; A"-bundle"]
	| _ -> invalid_arg "Please set JNIFLAGS"
let jniflags = S(List.map (fun flg -> S[A"-ccopt"; flg]) jniflags)
let ccopt = match target, arch, toolchain with
	| "linux", _, _ -> [A"-D__PTHREAD__"; A"-D__LINUX__"; A"-Wall"; A"-O2"]
	| "windows", _, "gcc" -> [A"-D__WINDOWS__"; A"-Wall"; A"-O2"]
	| "windows", _, "msvc" -> [A"/D__WINDOWS__"; A"/D__MSVC__"; A"/O2"]
	| "macosx", _, _ -> [A"-D__PTHREAD__"; A"-Wall"; A"-O2"]
	| _ -> []
let ccopt = S(List.map (fun flg -> S[A"-ccopt"; flg]) ccopt)
let ext_jnilib = match target with
	| "linux" -> "so"
	| "macosx" -> "jnilib"
	| "windows" -> "dll"
	| _ -> invalid_arg "System is not recognized"
let ext_obj = match toolchain with
	| "gcc" -> "o"
	| "msvc" -> "obj"
	| _ -> invalid_arg "System is not recognized"
let _o_obj = match toolchain with
	| "gcc" -> "-o"
	| "msvc" -> "/Fo"
	| _ -> invalid_arg "System is not recognized"

let cbnativedir = "java/nativelib" / target / arch
let cbnativename = match target with
	| "windows" -> "jcb"
	| _ -> "libjcb"
let cbnativelib = cbnativedir / cbnativename -.- ext_jnilib

(* collect all c files in java/jni folder and create a list of object      *)
(* files from them                                                         *)
let libobjfiles =
	List.map (Pathname.update_extension ext_obj)
		(List.filter (fun f -> Pathname.check_extension f "c")
				(Array.to_list (Pathname.readdir "java/jni")));;

let libobjfiles = List.map (fun s -> "java/jni/" ^ s) libobjfiles;;

(* collect all java files in java/cb folder and remove extensions *)
let javaprefix ="org/semanticweb/cb/reasoner"
let javaclasses =
	List.map Pathname.remove_extension
		(List.filter (fun f -> Pathname.check_extension f "java")
				(Array.to_list (Pathname.readdir ("java" / javaprefix))));;

let javafiles = List.map (fun s -> javaprefix / s -.- "java") javaclasses;;
let pjavafiles = List.map (fun s -> "%" ^ s) javafiles;;
let pjniheaders = List.map (fun s -> "%jni" / (String.subst "/" "_" javaprefix) ^ "_" ^ s -.- "h") javaclasses;;
let javaclassfiles = List.map (fun s -> javaprefix / s -.- "class") javaclasses;;
let pjavaclassfiles = List.map (fun s -> "%" ^ s) javaclassfiles;;
let pkgjavaclasses = List.map (fun s -> (String.subst "/" "." javaprefix) -.- s) javaclasses;;

dispatch begin function
		
		(* c Here one can change the default value of options, they can still  *)
		(* be updated by a command line option.                                *)
		| Before_options ->
		
		(*|				Options.ocaml_cflags := ["-w";"A"]*)
				Options.ocaml_cflags := []
		
		(* c Here one can change the final value of options. *)
		| After_options ->
		
		(* c This avoids the creation of symbolic links to the build           *)
		(* directory.                                                          *)
				Options.make_links := false
		
		(* c This hook is called before the hygiene phase. This phase also     *)
		(* serve as collecting all the information about the source tree.      *)
		| Before_hygiene ->
		
				tag_file "protege/lib/nativelib" ["not_hygienic"];
				tag_file cbnativelib [target]
		
		(* c One can also do things after the hygiene step. *)
		| After_hygiene -> ()
		
		(* c One can setup rules before the standard ones but that's not       *)
		(* recommended.                                                        *)
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
					Cmd (S [Sh gcc; T (tags ++ "depends");
						A "-MM"; A "-MG"; A "-MF"; Px (env prod); P c])
				in
				
				rule "cc: c -> c.depends"
					~dep:"%.c"
					~prod: "%.c.depends" (deps_action "%.c" "%.c.depends");
				
				rule "cc: h -> h.depends"
					~dep:"%.h"
					~prod:"%.h.depends" (deps_action "%.h" "%.h.depends");
				
				flag ["depends"; "use_cb"] & S[A"-isystem"; Sh "cwrap"];
				flag ["depends"; "use_caml"] & S[A"-isystem"; Sh std_lib];
				flag ["depends"; "use_jni"] &
				S(List.map (fun inc -> S[A"-isystem"; Sh inc]) jni_includes);
				
				rule "cc: c & c.depends -> o"
					~deps:["%.c"; "%.c.depends"]
					~prod: ("%" -.- ext_obj)
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
								Cmd (S [Sh ocamlopt; T (tags_of_pathname c ++ "compile" ++ "cc");
									ccopt; A "-c"; A"-ccopt"; P(_o_obj ^ env "%" -.- ext_obj); P c;])
					end;
				
				flag ["compile"; "cc"; "use_cb"] & S[A"-I"; Sh "cwrap"];
				flag ["compile"; "cc"; "use_caml"] & S[A"-I"; Sh std_lib];
				flag ["compile"; "cc"; "use_jni"] &
				S(List.map (fun inc -> S[A"-I"; Sh inc]) jni_includes);
				
				rule "javac: java -> class files"
					~deps: pjavafiles
					~prods: pjavaclassfiles
					begin fun env _ ->
								Cmd(S[Sh javac; A"-source"; A"5"; A"-d"; Px (env "%.");
									Command.atomize_paths (List.map env pjavafiles)]);
					end;
				
				rule "javah: class files -> jni headers"
					~deps: pjavaclassfiles
					~prods: pjniheaders
					begin fun env _ ->
								Cmd(S[Sh javah; A"-jni"; A"-classpath"; Px (env "%"); A "-d"; P (env "%jni");
									Command.atomize pkgjavaclasses])
					end;
				
				rule "ar: o -> a"
					~deps: ["%cb" -.- ext_obj ; "%cbcaml" -.- ext_obj]
					~prod: "%libcb.a"
					begin fun env _ ->
								Cmd(S[Sh ar; A "rcs"; Px (env "%libcb.a"); P (env "%cb" -.- ext_obj); P (env "%cbcaml" -.- ext_obj)])
					end;
				
				rule "jar: class files -> jar"
					~deps: pjavaclassfiles
					~prod: "%cb.jar"
					begin fun env _ ->
								Cmd(S[Sh "cd"; P (env "%"); Sh "&&"; Sh jar; A"cf"; Px (env "cb.jar");
									Command.atomize_paths (List.map env javaclassfiles)]);
					end;
				
				rule "jar: java files -> jar"
					~deps: pjavafiles
					~prod: "%cb-src.jar"
					begin fun env _ ->
								Cmd(S[Sh "cd"; A (env "%"); Sh "&&"; Sh jar; A"cf"; Px (env "cb-src.jar");
									Command.atomize_paths (List.map env javafiles)]);
					end;
				
				rule "jni"
					~prod:"%jni"
					~stamp:"%jni_stamp"
					~dep: ("%" ^ cbnativelib)
					(fun _ _ -> Nop);
		
		(* c Here one can add or override new rules *)
		| After_rules ->
		(* compiling camlp4 plugin *)
				flag ["ocaml"; "pp"; "use_camlp4"] (Sh camlp4orf);				
				(* using camlp4 preprocessing plugin *)
				flag ["ocaml"; "pp"; "use_typeext"] & S[Sh camlp4orf; A"libs/pa_typeext.cmo"];
				dep ["ocaml"; "use_typeext"] ["libs/pa_typeext.cmo"];
				(* generating debugging information *)
				flag ["ocaml"; "compile"] (A"-g");
				(* creating C object files instead of executables *)
				flag ["ocaml"; "link"; "output_obj"] (A"-output-obj");
				rule "output C obj"
					~deps:["%clib.cmx"; "%clib" -.- ext_obj]
					~prod: ("%caml" -.- ext_obj)
					(Ocamlbuild_pack.Ocaml_compiler.native_link "%clib.cmx" ("%caml" -.- ext_obj));
				(* creating JNI native library *)
				flag ["native"; "jni"] & S[jniflags; A("cwrap/cb" -.- ext_obj); Command.atomize_paths libobjfiles];
				flag ["native"; "jni"; "macosx"; "use_unix"] & S[A"/usr/lib/crt1.o"];
				rule "ocaml: cmx* & obj* -> jnilib"
					~deps: (List.map (fun s -> "%" ^ s)
							("cwrap/cbclib.cmx" :: "cwrap/cbclib" -.- ext_obj :: "cwrap/cb" -.- ext_obj :: libobjfiles))
					~prod: ("%" ^ cbnativelib)
					begin fun env builder -> Seq [
								Cmd(Sh ("mkdir -p " ^ cbnativedir));
								(Ocamlbuild_pack.Ocaml_compiler.native_link "%cwrap/cbclib.cmx" cbnativelib) env builder
								]
					end;
				(* FIXME: for two rules using [Ocamlbuild_pack.Ocaml_compiler.native_link] dependencies are detected only in one *)
		
	end;;
