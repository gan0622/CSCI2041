# Hwk_01 Assessment

Run on October 19, 12:33:53 PM.

*Test results are for the code in this repository as it appeared on October 05 at 17:15 CT.*

+ Pass: Change into directory "Hwk_01".

+ Pass: Check that file "recursive.ml" exists.

+ Pass: Check that an OCaml file "recursive.ml" has no syntax or type errors.

    OCaml file "recursive.ml" has no syntax or type errors.



+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   odd 0
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   odd 3
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   odd 4
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   euclid 6 9
   ```
   matches the pattern `3`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   euclid 5 9
   ```
   matches the pattern `1`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   euclid 6 6
   ```
   matches the pattern `6`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   euclid 1 1
   ```
   matches the pattern `1`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   frac_simplify (1, 1)
   ```
   matches the pattern `(1, 1)`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   frac_simplify (16, 32)
   ```
   matches the pattern `(1, 2)`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   frac_simplify (4, 9)
   ```
   matches the pattern `(4, 9)`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   min_list [73]
   ```
   matches the pattern `73`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   min_list [1; 2; 5; 3; 2]
   ```
   matches the pattern `1`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   min_list [-1; -2; -5; -3; -2]
   ```
   matches the pattern `-5`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   drop 3 [1; 2; 3; 4; 5]
   ```
   matches the pattern `[4; 5]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   drop 5 ['A'; 'B'; 'C']
   ```
   matches the pattern `[]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   drop 0 [1]
   ```
   matches the pattern `[1]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   drop 2 []
   ```
   matches the pattern `[]`.

   




+  _0_ / _1_ : Pass: odd: No unnecessary if-then-else such as if ... then true else false.

    Should use "x mod 2 <> 0" instead of pattern matching

+  _3_ / _3_ : Pass: euclid: Contains three cases corresponding to the observations in the instructions: a=b, a<b, and a>b.

    

+  _3_ / _3_ : Pass: euclid: The value returned in each of the three cases matches the observations in the instructions: a, gcd(a, b-a), and gcd(a-b, a)

    

+  _0_ / _2_ : Pass: frac_simplify: Used a `let` to avoid calling euclid more than once.

    Should use a 'let' statement to avoid calling euclid more than once

+  _1_ / _1_ : Pass: frac_simplify: Numerator and denominator are named as parameters rather than using a separate match, i.e. `let rec frac_simplify (n, m) =...` NOT `let rec frac_simplify x = match x with (n, m)`

    

+  _2_ / _2_ : Pass: frac_simplify: Consistent, reasonable indentation. Deduct 1 pt for a single incorrect indentation, 2 pts for more than one such problem.

    

+  _0_ / _1_ : Pass: min_list: A comment specifies restrictions on the lists.

    Missing comments

+  _0_ / _1_ : Pass: min_list: The comment specifies the list must not be empty.

    Missing comments

+  _0_ / _2_ : Pass: min_list: `min_list` is not called recursively more than once as in `if min_list rest < x then min_list rest else x`.

    recursively more thgan once

+  _2_ / _2_ : Pass: min_list: Consistent, reasonable indentation. Deduct 1 pt for a single incorrect indentation, 2 pts for more than one such problem.

    

+  _2_ / _2_ : Pass: drop: The match does not contain more than two cases. Except, it's ok if a third match case exists that uses a `when` in place of if-then-else.

    

+  _0_ / _1_ : Pass: drop: An underscore is used (e.g. `| _::xs ->`) rather than unnecessarily giving a name to the head of the list.

    Should use underscore for head of list in second match case

+  _2_ / _2_ : Pass: drop: Consistent, reasonable indentation. Deduct 1 pt for a single incorrect indentation, 2 pts for more than one such problem.

    

+ Pass: Check that file "higher.ml" exists.

+ Pass: Check that an OCaml file "higher.ml" has no syntax or type errors.

    OCaml file "higher.ml" has no syntax or type errors.



+ Pass: You are not allowed to use recursion.

   



+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   all_odds []
   ```
   matches the pattern `[]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   all_odds [4]
   ```
   matches the pattern `[]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   all_odds [1; 2; 3; 4]
   ```
   matches the pattern `[1; 3]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   decrement_all []
   ```
   matches the pattern `[]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   decrement_all [1; 2; 3; 4]
   ```
   matches the pattern `[0; 1; 2; 3]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   min_fold [0]
   ```
   matches the pattern `0`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   min_fold [-1; 0; 1]
   ```
   matches the pattern `-1`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   min_fold [1; 2; 3; 4]
   ```
   matches the pattern `1`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   sum_prod []
   ```
   matches the pattern `(0, 1)`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   sum_prod [0]
   ```
   matches the pattern `(0, 0)`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   sum_prod [1]
   ```
   matches the pattern `(1, 1)`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   sum_prod [1; 2; 3; 4]
   ```
   matches the pattern `(10, 24)`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   partition_left (fun x -> x < 4) []
   ```
   matches the pattern `([], [])`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   partition_left (fun x -> x < 4) [0]
   ```
   matches the pattern `([0], [])`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   partition_left (fun x -> x < 4) [7]
   ```
   matches the pattern `([], [7])`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   partition_left (fun x -> x < 4) [1;3;5;7;9;8;6;4;2]
   ```
   matches the pattern `([1; 3; 2], [5; 7; 9; 8; 6; 4])`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   partition_right (fun x -> x < 4) []
   ```
   matches the pattern `([], [])`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   partition_right (fun x -> x < 4) [0]
   ```
   matches the pattern `([0], [])`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   partition_right (fun x -> x < 4) [7]
   ```
   matches the pattern `([], [7])`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   partition_right (fun x -> x < 4) [1;3;5;7;9;8;6;4;2]
   ```
   matches the pattern `([1; 3; 2], [5; 7; 9; 8; 6; 4])`.

   




+  _2_ / _2_ : Pass: all_odds: Called List.filter.

    

+  _2_ / _2_ : Pass: all_odds: Passed a lambda expression, not `odd`.

    

+  _1_ / _1_ : Pass: all_odds: Consistent, reasonable indentation.

    

+  _2_ / _2_ : Pass: decrement_all: Called List.map.

    

+  _1_ / _1_ : Pass: decrement_all: Passed a function that returns its argument minus 1.

    

+  _1_ / _1_ : Pass: decrement_all: Consistent, reasonable indentation.

    

+  _2_ / _2_ : Pass: min_fold: Called either `List.fold_left` or `List.fold_right`.

    

+  _2_ / _2_ : Pass: min_fold: Passed a function that returns the minimum of its two arguments.

    

+  _1_ / _1_ : Pass: min_fold: Consistent, reasonable indentation.

    

+  _1_ / _3_ : Pass: sum_prod: A single fold is used, not two.

    Should use one fold, not two

+  _2_ / _2_ : Pass: sum_prod: Consistent, reasonable indentation. Deduct 1 pt for a single incorrect indentation, 2 pts for more than one such problem.

    

+  _1_ / _1_ : Pass: partition_left: Called List.fold_left.

    

+  _1_ / _1_ : Pass: partition_left: Passed `([], [])` as the second argument to `List.fold_left`.

    

+  _1_ / _1_ : Pass: partition_left: Passed a function to `List.fold_left` that contains `if f x then ...` (the variable names don't have to be `f` and `x`).

    

+  _1_ / _1_ : Pass: partition_left: In the if-then-else mentioned above, one branch puts the element into the first part of the pair and the other branch puts the element into the second part of the pair.

    

+  _1_ / _1_ : Pass: partition_left: Consistent, reasonable indentation.

    

+  _1_ / _1_ : Pass: partition_right: Called List.fold_right.

    

+  _1_ / _1_ : Pass: partition_right: Passed `([], [])` as the third argument to `List.fold_right`.

    

+  _1_ / _1_ : Pass: partition_right: Passed a function to `List.fold_right` that contains `if f x then ...` (the variable names don't have to be `f` and `x`).

    

+  _1_ / _1_ : Pass: partition_right: In the if-then-else mentioned above, one branch puts the element into the first part of the pair and the other branch puts the element into the second part of the pair.

    

+  _1_ / _1_ : Pass: partition_right: Consistent, reasonable indentation.

    

+  _1_ / _1_ : Pass: map_as_fold: A comment gives an explanation of why either `List.fold_left` or `List.fold_right` was chosen.

    

+  _2_ / _2_ : Pass: map_as_fold: The comment gives a reasonable explanation and includes a discussion of any difficulties found using the other (should mention something about the order of the result being reversed).

    

+  _0_ / _2_ : Pass: map_as_fold: `List.fold_right` was chosen.

    Did not choose list.fold_right

+  _1_ / _1_ : Pass: map_as_fold: The function passed to `List.fold_right` contains an application of the given function, i.e. `f x` (the variable names don't have to be `f` and `x`)

    

+  _1_ / _1_ : Pass: map_as_fold: The function passed to `List.fold_right` contains nothing more than the function application mentioned above and a cons, i.e. `(fun x xs -> f x :: xs)`.

    

+  _1_ / _1_ : Pass: map_as_fold: Consistent, reasonable indentation.

    

+  _3_ / _3_ : Pass: Type annotations are included on the parameters of at least one function in higher.ml

    

#### Total score: _88_ / _100_

