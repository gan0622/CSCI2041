# Hwk_06 Assessment

Run on December 16, 07:19:15 AM.

*Test results are for the code in this repository as it appeared on November 25 at 17:15 CT.*

+ Pass: Change into directory "Hwk_06".

+ Pass: Check that file "streams.ml" exists.

Note: the following definition of `drop` is used by the automated tests:

```ocaml
let rec drop (n: int) (xs: 'a list) : 'a list =
  if n <= 0 || xs = [] then xs else drop (n-1) (List.tl xs)

```

## Warming up with 3 versions of streams of cubes

## Warming up with 3 versions of streams of cubes

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   take 5 (cubes_from 1)
   ```
   matches the pattern `[1; 8; 27; 64; 125]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   take 5 (cubes_from_map 1)
   ```
   matches the pattern `[1; 8; 27; 64; 125]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   take 5 (cubes_from_zip 1)
   ```
   matches the pattern `[1; 8; 27; 64; 125]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   take 3 (cubes_from 3)
   ```
   matches the pattern `[27; 64; 125]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   take 3 (cubes_from_map 3)
   ```
   matches the pattern `[27; 64; 125]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   take 3 (cubes_from_zip 3)
   ```
   matches the pattern `[27; 64; 125]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```ocaml
   take 20 (cubes_from 20)
   ```
   matches the pattern `[8000; 9261; 10648; 12167; 13824; 15625; 17576; 19683; 21952; 24389; 27000; 29791; 32768; 35937; 39304; 42875; 46656; 50653; 54872; 59319]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```ocaml
   take 20 (cubes_from_map 20)
   ```
   matches the pattern `[8000; 9261; 10648; 12167; 13824; 15625; 17576; 19683; 21952; 24389; 27000; 29791; 32768; 35937; 39304; 42875; 46656; 50653; 54872; 59319]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```ocaml
   take 20 (cubes_from_zip 20)
   ```
   matches the pattern `[8000; 9261; 10648; 12167; 13824; 15625; 17576; 19683; 21952; 24389; 27000; 29791; 32768; 35937; 39304; 42875; 46656; 50653; 54872; 59319]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```ocaml
   drop 100 (take 105 (cubes_from 1))
   ```
   matches the pattern `[1030301; 1061208; 1092727; 1124864; 1157625]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```ocaml
   drop 100 (take 105 (cubes_from_map 1))
   ```
   matches the pattern `[1030301; 1061208; 1092727; 1124864; 1157625]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```ocaml
   drop 100 (take 105 (cubes_from_zip 1))
   ```
   matches the pattern `[1030301; 1061208; 1092727; 1124864; 1157625]`.

   




+  _3_ / _3_ : Pass: Cubes: The submission of cubes_from represents a good effort to solve the problem

    

+  _3_ / _3_ : Pass: Cubes: The submission of cubes_from_map represents a good effort to solve the problem

    

+  _3_ / _3_ : Pass: Cubes: The submission of cubes_from_zip represents a good effort to solve the problem

    

## Factorials, reusing previous multiplication results

+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```ocaml
   take 5 facts'
   ```
   matches the pattern `[1; 1; 2; 6; 24]`.

   




+  _7_ / _7_ : Pass: 
Check that the result of evaluating
   ```ocaml
   drop 10 (take 15 facts')
   ```
   matches the pattern `[3628800; 39916800; 479001600; 6227020800; 87178291200]`.

   




+  _7_ / _7_ : Pass: 
Check that the result of evaluating
   ```ocaml
   drop 15 (take 20 facts')
   ```
   matches the pattern `[1307674368000; 20922789888000; 355687428096000; 6402373705728000; 121645100408832000]`.

   




+  _7_ / _7_ : Pass: Factorials: a comment after the definition of facts' explains why facts' does not repeat the multiplications that facts does

    

+  _7_ / _7_ : Pass: Factorials: The submission of facts' represents a good effort to solve the problem.

    

## The Sieve of Eratosthenes

+  _8_ / _8_ : Pass: 
Check that the result of evaluating
   ```ocaml
   take 10 (sieve (from 2))
   ```
   matches the pattern `[2; 3; 5; 7; 11; 13; 17; 19; 23; 29]`.

   




+  _8_ / _8_ : Pass: 
Check that the result of evaluating
   ```ocaml
   drop 50 (take 55 (sieve (from 2)))
   ```
   matches the pattern `[233; 239; 241; 251; 257]`.

   




+  _8_ / _8_ : Pass: 
Check that the result of evaluating
   ```ocaml
   drop 100 (take 110 (sieve (from 2)))
   ```
   matches the pattern `[547; 557; 563; 569; 571; 577; 587; 593; 599; 601]`.

   




+  _10_ / _10_ : Pass: Sieve: The submission of sieve represents a good effort to solve the problem.

    

#### Total score: _100_ / _100_

