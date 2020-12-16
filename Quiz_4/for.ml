let () =

let rec read_number () : int = 
  try int_of_string (read_line ()) with 
  | Failure _ -> 
      print_endline "Please, enter an integer value: "; 
      read_number () 
in 

let i = ref 0 in
let _stop = ref 0 in
i := 1; 
_stop := 5; 
( 
let rec loop () = 
( 
if (!i <= !_stop)
then 
(
print_int (!i); print_newline(); 
i := (!i + 1);
loop()
)
else
()
)
in loop()
)
