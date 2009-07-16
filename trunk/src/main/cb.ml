(* Main Program *)

open Arg
module PB = ProgressBar

let mem_control_init () =
  let old_controls = Gc.get () in
  let new_controls = { old_controls with
    Gc.minor_heap_size = 4 * 1024 * 1024 * 8 / Sys.word_size; (* 4MB *)
    Gc.major_heap_increment = 8 * 1024 * 1024 * 8 / Sys.word_size; (* 8MB *)
    Gc.max_overhead = 100000;
    Gc.space_overhead = 400;
  } in
  Gc.set new_controls

let print_memory_usage () =
  let stat = Gc.stat () and control = Gc.get () in
  let max_words_total = stat.Gc.heap_words + control.Gc.minor_heap_size in
  let bytes = ( max_words_total * ( Sys.word_size / 8) ) in
  let kbytes = (bytes / 1024) in
  let mbytes = (kbytes / 1024) in
  Printf.fprintf stderr "Allocated memory:\t%d Mbytes %d kBytes\n"
    mbytes (kbytes - mbytes * 1024)
;;

let print_cpu_time () =
  Printf.fprintf stderr "CPU time:\t\t%.3fs\n" (Sys.time ())
;;

let () =
  mem_control_init ()
;;

type output_format =
  | Fowl
  | Krss
  | Lisp

let _ =
  
  let input = ref "" in
  let output = ref "" in
  let classify = ref false in
  let distill = ref false in
  let print_info = ref false in
  let output_format = ref Fowl in
  
  parse
    ( align([
          ("-i", Set print_info, " prints short info about the ontology");
          ("-c", Set classify, " classifies the ontology");
          ("-d", Set distill, " do not classify but parse and output the ontology");
          ("-o", String (fun s -> output := s),"[file] outputs the result to [file] instead of standard output (by default)");
          ("-fowl", Unit (fun () -> output_format := Fowl), " set the taxonomy output format to functional style owl syntax (default)");
          ("-krss", Unit (fun () -> output_format := Krss), " set the taxonomy output format to krss");
          ("-lisp", Unit (fun () -> output_format := Lisp), " set the taxonomy output format to lisp");
          ]))
    
    (fun s -> input := s)
    
    (" CB is a Consequence-Based reasoner for Horn-SHIF ontologies." ^
      "\n This program is free for non-commersial use." ^
      " No warranty. Use at your own risk!" ^
      "\n Copyright (c) 2009 Yevgeny Kazakov <yevgeny.kazakov@comlab.ox.ac.uk> and Oxford University" ^
      "\n\n Usage: cb [OPTIONS]... [ONTOLOGY]" ^
      "\n Loads [ONTOLOGY] in funcional-style OWL 2 syntax and optionally prints the" ^
      "\n information about the ontology and computes the ontology taxonomy." ^
      "\n\n accepted [OPTIONS] are:"
    );
  
  let in_channel =
    if (!input = "") then	stdin	else (open_in !input)
  in
  
  let out_channel =
    if (!output = "") then stdout else (open_out !output)
  in
  
  Printf.fprintf stderr "CB is processing the ontology from %s\n"
    (if !input = "" then "standard input" else "\""^ !input ^ "\""); flush stderr;
  Printf.fprintf stderr "1. Loading the ontology...     "; flush stderr;
  PB.init (in_channel_length in_channel);
  let ont = Krss.load_ontology in_channel in
  
  if !print_info then (
    Ontology.print_staticstics ont stderr;
    Printf.fprintf stderr "Expressivity: %s\n" (OntologyLanguage.str_expressivity ont);
    Printf.fprintf stderr "==============================\n";
    Pervasives.flush stderr;
  );
  
  if !distill then (
    classify := false;
    Fowl.print_ontology ont out_channel
  );
  
  if !classify then (
    if not (OntologyLanguage.is_horn ont) then
      Printf.fprintf stderr "   Warning! Reasoning can be incomplete (the ontology is not Horn)!\n";
    Printf.fprintf stderr "2. Classifying the ontology... "; flush stderr;
    let iss = ReasonerTBox.saturate ont in
    
    Printf.fprintf stderr "3. Formatting the taxonomy...  ";
    flush stderr;
    (
      match !output_format with
      | Fowl -> ConceptTaxonomy.print_fowl iss ont out_channel 
      | Krss -> ConceptTaxonomy.print_krss iss ont out_channel
      | Lisp -> ConceptTaxonomy.print_lisp iss ont out_channel
    );
  );
  
  print_memory_usage ();
  print_cpu_time ();