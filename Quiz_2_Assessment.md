# Quiz_2 Assessment

Run on November 29, 23:40:09 PM.

*Test results are for the code in this repository as it appeared on October 28 at 17:10 CT.*

+ Pass: Change into directory "Quiz_2".

+ Pass: Check that file "quiz_2.ml" exists.

``Part 1``

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   sum (Leaf 4)
   ```
   matches the pattern `4`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   sum (Fork (Leaf 4, Fork (Leaf 3, Leaf 2)))
   ```
   matches the pattern `9`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   prod (Leaf 4)
   ```
   matches the pattern `4`.

   




+  _0_ / _2_ : Fail: 
Check that the result of evaluating
   ```
   prod (Fork (Leaf 4, Fork (Leaf 3, Leaf 2))
   ```
   matches the pattern `24`.

   


   Test failed. The following errors were reported:

```
 ;;
Line 1, characters 52-54:
1 | (prod (Fork (Leaf 4, Fork (Leaf 3, Leaf 2))) = (24) ;; ;;
                                                        ^^
Error: Syntax error: ')' expected
Line 1, characters 0-1:
1 | (prod (Fork (Leaf 4, Fork (Leaf 3, Leaf 2))) = (24) ;; ;;
    ^
  This '(' might be unmatched

```


+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   sumr (Leaf 4)
   ```
   matches the pattern `4`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   sumr (Fork (Leaf 4, Fork (Leaf 3, Leaf 2)))
   ```
   matches the pattern `9`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   prodr (Leaf 4)
   ```
   matches the pattern `4`.

   




+  _0_ / _2_ : Fail: 
Check that the result of evaluating
   ```
   prodr (Fork (Leaf 4, Fork (Leaf 3, Leaf 2))
   ```
   matches the pattern `24`.

   


   Test failed. The following errors were reported:

```
 ;;
Line 1, characters 53-55:
1 | (prodr (Fork (Leaf 4, Fork (Leaf 3, Leaf 2))) = (24) ;; ;;
                                                         ^^
Error: Syntax error: ')' expected
Line 1, characters 0-1:
1 | (prodr (Fork (Leaf 4, Fork (Leaf 3, Leaf 2))) = (24) ;; ;;
    ^
  This '(' might be unmatched

```


+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   string_of_int_tree (Leaf 4)
   ```
   matches the pattern `"4"`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   string_of_int_tree (Fork (Leaf 1, Leaf 23))
   ```
   matches the pattern `"123"`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   string_of_int_tree (Fork (Leaf 1, Fork (Leaf 2, Leaf 3)))
   ```
   matches the pattern `"123"`.

   




+  _5_ / _5_ : Pass: reduce has generic, correct type annotation

    

+  _0_ / _1_ : Fail: does sumr just call reduce (1=yes)? (if not, then sumr automated points will be deducted later)

    

+  _0_ / _1_ : Fail: deos prodr just call reduce (1=yes)? (if not, then prodr automated points will be deducted later)

    

+ Pass: Check that file "quiz_2.md" exists.

``Part 2``

+  _3_ / _3_ : Pass: state the principle of induction

    

+  _3_ / _3_ : Pass: state the base case

    

+  _3_ / _3_ : Pass: state the inductive case

    

+  _3_ / _3_ : Pass: state the inductive hypothesis

    

+  _6_ / _6_ : Pass: base case proof is correct

    

+  _2_ / _2_ : Pass: inductive proof: a sub-case for even and for odd

    

+  _5_ / _5_ : Pass: correct proof of even case

    

+  _4_ / _5_ : Pass: correct proof of odd case

    Your proof should ommit the element that will be filtered, do not use the anonymous operator

#### Total score: _55_ / _62_

