let () =

let rec read_number () : int = 
  try int_of_string (read_line ()) with 
  | Failure _ -> 
      print_endline "Please, enter an integer value: "; 
      read_number () 
in 

let x = ref 0 in
let y = ref 0 in
let z = ref 0 in
let _stop = ref 0 in
print_endline "Enter a value for x:"; 
x := read_number (); 
y := (!x + 2); 
z := (!y * 3); 
print_endline "value of z is:"; 
print_int (!z); print_newline()
