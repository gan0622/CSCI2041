let () =

let rec read_number () : int = 
  try int_of_string (read_line ()) with 
  | Failure _ -> 
      print_endline "Please, enter an integer value: "; 
      read_number () 
in 

let x = ref 0 in
let i = ref 0 in
let sum_evens = ref 0 in
let sum_odds = ref 0 in
let _stop = ref 0 in
print_endline "Enter a number to some sums of:"; 
x := read_number (); 
i := 0; 
sum_evens := 0; 
sum_odds := 0; 
( 
let rec loop () = 
( 
if (!i < !x)
then 
(
print_int (!i); print_newline(); 
(
if ((!i % 2) = 0) 
then
(
 sum_evens := (!sum_evens + !i) 
)
else
(
 sum_odds := (!sum_odds + !i))
); 
i := (!i + 1);
loop()
)
else
()
)
in loop()
); 
print_endline "sum of evens is:"; 
print_int (!sum_evens); print_newline(); 
print_endline "sum of odds is:"; 
print_int (!sum_odds); print_newline()
