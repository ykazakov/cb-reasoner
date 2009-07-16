module IS = Intset_hc
module F = Format

let rec print_s f m = function
  | IS.SEmpty ->     
    F.pp_print_string f (string_of_int m);    
  | IS.SNode (l, mr, r) ->
    F.pp_open_box f 1;
    F.pp_print_string f "[";
    print_s f m l;
    F.pp_print_string f "]";
    F.pp_close_box f ();
    F.pp_open_box f 1;
    F.pp_print_string f " [";
    print_s f (m lxor mr) r;
    F.pp_print_string f "]";
    F.pp_close_box f ();
;;
let print_is f = function
  | IS.Empty -> F.pp_print_string f "[]";
  | IS.Node (m, s) -> print_s f m s;
;;        
  
