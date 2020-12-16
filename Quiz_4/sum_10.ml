let () =

let rec read_number () : int = 
  try int_of_string (read_line ()) with 
  | Failure _ -> 
      print_endline "Please, enter an integer value: "; 
      read_number () 
in 

let sum = ref 0 in
let i = ref 0 in
let _stop = ref 0 in
sum := 0; 
i := 1; 
_stop := 10; 
( 
let rec loop () = 
( 
if (!i <= !_stop)
then 
(
sum := (!sum + !i); 
i := (!i + 1);
loop()
)
else
()
)
in loop()
); 
print_endline "sum is"; 
print_int (!sum); print_newline()
