# Hwk_07 Assessment

Run on December 16, 04:09:47 AM.

*Test results are for the code in this repository as it appeared on December 02 at 17:15 CT.*

+ Pass: Change into directory "Hwk_07".

## Part 1. Lazy Modules

+ Pass: Check that file `lazeeModules.ml` exists.

+ Pass: Check standard output of the following command
   ```
   rm lazeeModules.byte
   ```
   matches the following:
   ```
   
   ```

   

    The command succeeded.

+  _10_ / _10_ : Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild lazeeModules.byte
   ```

   

    The command succeeded.

+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```ocaml
   Lazee_v1.demand (Lazee_v1.delay (fun () -> 0))
   ```
   matches the pattern `0`.

   




+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```ocaml
   Lazee_v2.demand (Lazee_v2.delay (fun () -> 1))
   ```
   matches the pattern `1`.

   




## Part 2. A stream functor

+ Pass: Check that file `streamModules.ml` exists.

+ Pass: Check standard output of the following command
   ```
   rm streamModules.byte
   ```
   matches the following:
   ```
   
   ```

   

    The command succeeded.

+  _10_ / _10_ : Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild streamModules.byte
   ```

   

    The command succeeded.

## Part 3. Homework 6 - again, using modules

+ Pass: Check that file `hwk_06_Modules.ml` exists.

+ Pass: Check standard output of the following command
   ```
   rm hwk_06_Modules.byte
   ```
   matches the following:
   ```
   
   ```

   

    The command succeeded.

+  _10_ / _10_ : Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild hwk_06_Modules.byte
   ```

   

    The command succeeded.

## Part 4. Testing your code (hwk_07.ml)

+ Pass: Check that file `hwk_07.ml` exists.

+  _10_ / _10_ : Pass: Check that an OCaml file `hwk_07.ml` has no syntax  or type errors.

    OCaml file `hwk_07.ml` has no syntax or type errors.



+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```ocaml
   Hwk_06_Lazy.take 4 Hwk_06_Lazy.nats
   ```
   matches the pattern `[1; 2; 3; 4]`.

   




+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```ocaml
   Hwk_06_Slow.take 4 (Hwk_06_Slow.cubes_from 1)
   ```
   matches the pattern `[1; 8; 27; 64]`.

   




+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```ocaml
   Hwk_06_Lazy.take 5 Hwk_06_Lazy.facts'
   ```
   matches the pattern `[1; 1; 2; 6; 24]`.

   




+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```ocaml
   Hwk_06_Slow.take 10 Hwk_06_Slow.primes
   ```
   matches the pattern `[2; 3; 5; 7; 11; 13; 17; 19; 23; 29]`.

   




#### Total score: _100_ / _100_

