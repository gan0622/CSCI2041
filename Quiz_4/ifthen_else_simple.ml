let () =

let rec read_number () : int = 
  try int_of_string (read_line ()) with 
  | Failure _ -> 
      print_endline "Please, enter an integer value: "; 
      read_number () 
in 

let y = ref 0 in
let _stop = ref 0 in
y := 10; 
(
if (!y < 15) 
then
(
 print_int (!y); print_newline() 
)
else
(
 print_int (100); print_newline())
)
