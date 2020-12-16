### Assessment for Lab 03

#### Total score: _38_ / _54_

Run on October 08, 14:10:16 PM.

+  _1_ / _1_ : Pass: Change into directory "Lab_03".

+  _1_ / _1_ : Pass: Check that file "lab_02.ml" exists.

#### Your code also should generate no errors or warnings:

+  _1_ / _1_ : Pass: Check that an OCaml file "lab_02.ml" has no syntax or type errors.

    OCaml file "lab_02.ml" has no syntax or type errors.



+  _1_ / _1_ : Pass: Check that an OCaml file "lab_02.ml" has warnings.

    OCaml file "lab_02.ml" has no warnings.



#### Your functions should all still work as specified.

+  _1_ / _1_ : Pass: Check that the result of evaluating `circle_circum_v1 2.5` matches the pattern `15.70`.

   



+  _1_ / _1_ : Pass: Check that the result of evaluating `circle_circum_v2 2.5` matches the pattern `15.70`.

   



+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   product []
   ```
   matches the pattern `1`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   product [2; 3; 4]
   ```
   matches the pattern `24`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   sum_sqrdiffs [4; -1; 5; 2]
   ```
   matches the pattern `70`.

   




+  _1_ / _1_ : Pass: Check that the result of evaluating `distance (1.2, 3.4) (4.5, 5.6)` matches the pattern `3.9`.

   



+  _1_ / _1_ : Pass: Check that the result of evaluating `triangle_perimeter (1.0, 2.0) (3.0, 4.0) (5.0, 1.0)` matches the pattern `10.55`.

   



### Checking  Lab 03 improvements.

#### Remove unnecessary ';;' tokens.

+  _0_ / _4_ : Fail: See 'Remove unnecessary ;;' in the lab for more information

#### Exception raised in sum_sqrdiffs.

+  _5_ / _5_ : Pass: Check that the result of evaluating `product []` matches the pattern `1`.

   



+  _0_ / _5_ : Fail: Check that the result of evaluating `sum_sqrdiffs []` matches the pattern `Failure`.

   

   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
- : int = 0
`


#### Avoid awkward list construction of [x] @ xs.

+  _5_ / _5_ : Pass: See 'Avoiding awkward list construction' in the lab for more information.

#### ... leave comments, and style your code consistently?

+  _0_ / _4_ : Pass: Check if prologue comment specifies group member names.

    

+  _8_ / _10_ : Pass: Check if code has explanatory comments.

    unclear comments for circum2 and no jusitification for no changes in product

+  _9_ / _10_ : Pass: Check if code is readable and has consistent style.

    no implmentation of exception case in sqrdiffs

#### Total score: _38_ / _54_

