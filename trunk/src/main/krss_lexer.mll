{
open Krss_parser
  
let update_pos lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  let p = pos.Lexing.pos_cnum in
  ProgressBar.set_state p;
  lexbuf.Lexing.lex_curr_p <- 
  { pos with
    Lexing.pos_lnum = succ pos.Lexing.pos_lnum;
    Lexing.pos_bol = pos.Lexing.pos_cnum
    }
 ;;  
   
}

rule token = parse
    [' ' '\t']         { token lexbuf }
  | "\n"               { update_pos lexbuf; token lexbuf}  
  | "("                { LeftParen    }
  | ")"                { RightParen   }

(* concept axioms *)

  | "implies"          { Implies    }
  | "defprimconcept"   { Implies    }
  | "define-primitive-concept"   { Implies    }
  | "DEFINE-PRIMITIVE-CONCEPT"   { Implies    }    
  | "equivalent"       { Equivalent }
  | "defconcept"       { Equivalent }
  | "define-concept"   { Equivalent }
  | "DEFINE-CONCEPT"   { Equivalent }

(* role axioms *)

  | "defprimrole"    { ImpliesRole }
  | "define-primitive-role"    { ImpliesRole }
  | "DEFINE-PRIMITIVE-ROLE"    { ImpliesRole }  
  | "implies-role"   { ImpliesRole }
	| "role-inclusion" { ImpliesRole }
  | "equivalent-role" { EquivalentRole }
  | "role-equivalent" { EquivalentRole }
  | "inverse"        { Inverse     }
  | "functional"     { Functional  }
  | "FUNCTIONAL"     { Functional  }
  | "transitive"     { Transitive  }
  | "TRANSITIVE"     { Transitive  }
  | "composition"    { Composition }

(* concept constructors *)

  | "and"          { And    }
  | "AND"          { And    }
  | "intersection" { And    }
  | "or"           { Or     }
  | "OR"           { Or     }
  | "union"        { Or     } 
  | "some"         { Some   }
  | "SOME"         { Some   }
  | "all"          { All    }
  | "ALL"          { All    }
  | "not"          { Not    }
  | "complement"   { Not    }
  | "top"          { Top    }
  | "TOP"          { Top    }
  | "bottom"       { Bottom }
  | "BOTTOM"       { Bottom } 
  | "bot"          { Bottom }
  | "BOT"          { Bottom }

(* role constructors *)

  | "inv"          { Inv }

(* identifiers *)

  | [^ ';' '(' ')' ' ' '\n']* { Identifier (Lexing.lexeme lexbuf) }

(* comments annotations *)
  
  | ";" [^ '\n']* '\n'  { update_pos lexbuf; token lexbuf }

(* eof *)

  | eof            { update_pos lexbuf; EOF }
