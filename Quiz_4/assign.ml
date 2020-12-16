let () =

let rec read_number () : int = 
  try int_of_string (read_line ()) with 
  | Failure _ -> 
      print_endline "Please, enter an integer value: "; 
      read_number () 
in 

let x = ref 0 in
let y = ref 0 in
let _stop = ref 0 in
x := 1; 
y := (!x + 2)
