let () =

let rec read_number () : int = 
  try int_of_string (read_line ()) with 
  | Failure _ -> 
      print_endline "Please, enter an integer value: "; 
      read_number () 
in 

let x = ref 0 in
let i = ref 0 in
let sum = ref 0 in
let _stop = ref 0 in
print_endline "Enter a number to sum to:"; 
x := read_number (); 
i := 0; 
sum := 0; 
( 
let rec loop () = 
( 
if (!i < !x)
then 
(
print_int (!i); print_newline(); 
sum := (!sum + !i); 
i := (!i + 1);
loop()
)
else
()
)
in loop()
); 
print_int (!sum); print_newline()
