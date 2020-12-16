### Feedback for Homework 1

Run on September 29, 13:40:53 PM.

+ Pass: Change into directory "Hwk_01".

+ Pass: Check that file "recursive.ml" exists.

+ Pass: Check that an OCaml file "recursive.ml" has no syntax or type errors.

    OCaml file "recursive.ml" has no syntax or type errors.



+ Pass: 
Check that the result of evaluating
   ```
   odd 3
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   odd 4
   ```
   matches the pattern `false`.

   




+ Pass: 
Check that the result of evaluating
   ```
   euclid 6 9
   ```
   matches the pattern `3`.

   




+ Pass: 
Check that the result of evaluating
   ```
   euclid 5 9
   ```
   matches the pattern `1`.

   




+ Pass: 
Check that the result of evaluating
   ```
   frac_simplify (8, 16)
   ```
   matches the pattern `(1, 2)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   frac_simplify (4, 9)
   ```
   matches the pattern `(4, 9)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   min_list [1; 2; 5; 3; 2]
   ```
   matches the pattern `1`.

   




+ Pass: 
Check that the result of evaluating
   ```
   min_list [-1; -2; -5; -3; -2]
   ```
   matches the pattern `-5`.

   




+ Pass: 
Check that the result of evaluating
   ```
   drop 3 [1; 2; 3; 4; 5]
   ```
   matches the pattern `[4; 5]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   drop 5 ['A'; 'B'; 'C']
   ```
   matches the pattern `[]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   drop 0 [1]
   ```
   matches the pattern `[1]`.

   




+ Pass: Check that file "higher.ml" exists.

+ Pass: Check that an OCaml file "higher.ml" has no syntax or type errors.

    OCaml file "higher.ml" has no syntax or type errors.



+ Pass: You are not allowed to use recursion.

   



+ Pass: 
Check that the result of evaluating
   ```
   all_odds [1; 2; 3; 4]
   ```
   matches the pattern `[1; 3]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   decrement_all [1; 2; 3; 4]
   ```
   matches the pattern `[0; 1; 2; 3]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   min_fold [1; 2; 3; 4]
   ```
   matches the pattern `1`.

   




+ Pass: 
Check that the result of evaluating
   ```
   sum_prod []
   ```
   matches the pattern `(0, 1)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   sum_prod [1; 2; 3]
   ```
   matches the pattern `(6, 6)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   sum_prod [1; 2; 3; 4]
   ```
   matches the pattern `(10, 24)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   partition_left (fun x -> x > 4) [1;3;5;7;9;8;6;4;2]
   ```
   matches the pattern `([5; 7; 9; 8; 6], [1; 3; 4; 2])`.

   




+ Pass: 
Check that the result of evaluating
   ```
   partition_right (fun x -> x > 4) [1;3;5;7;9;8;6;4;2]
   ```
   matches the pattern `([5; 7; 9; 8; 6], [1; 3; 4; 2])`.

   




+ Pass: 
Check that the result of evaluating
   ```
   map_as_fold ((+) 1) [1;2;3;4]
   ```
   matches the pattern `[2;3;4;5]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   map_as_fold ((+) 1) [1;2;3;4]
   ```
   matches the pattern `List.map ((+) 1) [1;2;3;4]`.

   




