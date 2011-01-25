(* Main Program *)

open Arg

let mem_control_init () =
  let old_controls = Gc.get () in
  let new_controls = { old_controls with
    Gc.minor_heap_size = 4 * 1024 * 1024 * 8 / Sys.word_size; (* 4MB *)
    Gc.major_heap_increment = 8 * 1024 * 1024 * 8 / Sys.word_size; (* 8MB *)
    Gc.max_overhead =   1000;
    Gc.space_overhead = 100;
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

let _ =
  
  let input = ref "" in
  let output = ref "" in
  let classify = ref false in
  let distill = ref false in
  let print_info = ref false in
  
  parse
    ( align([
          ("-i", Set print_info, " prints short info about the ontology");
          ("-c", Set classify, " classifies the ontology");
          ("-d", Set distill, " do not classify but parse and output the ontology");
          ("-o", String (fun s -> output := s),"[file] outputs the result to [file] instead of standard output (by default)");
          ]))
    
    (fun s -> input := s)
    
    (" CB is a Consequence-Based reasoner for Horn-SHIF ontologies." ^
      "\n Copyright (c) 2009, 2010, 2011 Yevgeny Kazakov" ^ 
			"\n <yevgeny.kazakov@comlab.ox.ac.uk> and Oxford University" ^
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
  
(*|	let ont = Owl_io_actions.load_Ontology_from_channel in_channel "1. Loading the ontology:" in*)
  let pm = Progress_monitor.of_out_channel stderr in
	let pt = Progress_tracker.of_progress_monitor pm in
	
  let ont = Owl_io.load_Ontology_from_channel [pt] in_channel in
	
  if !print_info then (
    Ontology.print_info ont stderr;    
    Pervasives.flush stderr;
  );
  
  if !distill then (
    classify := false;
    Owl_io.print_ontology_ch pt ont out_channel
  );
  
  if !classify then (
    if not (Ontology_language.is_horn ont) then
      Printf.fprintf stderr "   Warning! Reasoning can be incomplete (the ontology is not Horn)!\n";
		let iss = Saturation.compute [pt] ont in    
    flush stderr;
		Class_taxonomy.print_fowl [pt] iss ont out_channel     
  );
	
  print_memory_usage ();
  print_cpu_time ();