### Assessment for Lab 04

#### Total score: _100_ / _100_

Run on October 12, 21:14:53 PM.

+ Pass: Change into directory "Lab_04".

+ Pass: Check that file "lab_04.ml" exists.

+  _5_ / _5_ : Pass: Check that an OCaml file "lab_04.ml" has no syntax or type errors.

    OCaml file "lab_04.ml" has no syntax or type errors.



+  _5_ / _5_ : Pass: You are not allowed to use recursion.

   



+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   length []
   ```
   matches the pattern `0`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   length [1; 2; 3]
   ```
   matches the pattern `3`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   andf []
   ```
   matches the pattern `true`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   andf [true; false; true]
   ```
   matches the pattern `false`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   andf [true; true; true]
   ```
   matches the pattern `true`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   orf []
   ```
   matches the pattern `false`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   orf [true; false; true]
   ```
   matches the pattern `true`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   orf [false; false; false]
   ```
   matches the pattern `false`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   is_elem 3 []
   ```
   matches the pattern `false`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   is_elem 3 [2; 3; 4]
   ```
   matches the pattern `true`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   is_elem 42 [2; 3; 4]
   ```
   matches the pattern `false`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   rev []
   ```
   matches the pattern `[]`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   rev [1; 2; 3; 4]
   ```
   matches the pattern `[4; 3; 2; 1]`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   ascii_sum []
   ```
   matches the pattern `0`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   ascii_sum ['a'; 'b'; 'c']
   ```
   matches the pattern `294`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   lebowski []
   ```
   matches the pattern `[]`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   lebowski ['.']
   ```
   matches the pattern `[','; ' '; 'd'; 'u'; 'd'; 'e'; '.']`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   lebowski ['H'; 'e'; 'y'; '.'; 'W'; 'a'; 's'; 's'; 'u'; 'p'; '.'; 'E'; 'h'; '?']
   ```
   matches the pattern `['H'; 'e'; 'y'; ','; ' '; 'd'; 'u'; 'd'; 'e'; '.'; 'W'; 'a'; 's'; 's'; 'u'; 'p'; ','; ' '; 'd'; 'u'; 'd'; 'e'; '.'; 'E'; 'h'; '?']`.

   




#### Total score: _100_ / _100_

