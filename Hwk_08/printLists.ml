(* Some functions for converting lists into strings
   and printing them. *)

let show_list show lst =
  "[" ^ String.concat "; " (List.map show lst) ^ "]"

let show_lists show lsts =
  String.concat "\n" (List.map (show_list show) lsts)

let print_int_list lst =
  print_endline ((show_list string_of_int) lst)

let print_int_lists lsts =
  print_endline ((show_lists string_of_int) lsts)
