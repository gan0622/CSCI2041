# Quiz_4 Feedback

Run on December 08, 22:50:13 PM.

+ Pass: Change into directory `Quiz_4`.

+ Pass: Check that file `translator.ml` exists.

+ Pass: Check that an OCaml file `translator.ml` has no syntax  or type errors.

    OCaml file `translator.ml` has no syntax or type errors.



The following tests are for sample programs defined in `translator.ml`

These tests are incomplete.  They do not test all of the langauge constructs that need to be implemented for this quiz.  Other sample programs in `translator.ml` can be used to tests these constructs on your own.

## Testing program sample_1

+ Pass: 
Check that the result of evaluating
   ```
   translate_prog program_sample_1
   ```
   matches the pattern `()`.

   




+ Pass: Check that file `sample_1.ml` exists.

+ Pass: Check that an OCaml file `sample_1.ml` has no syntax  or type errors.

    OCaml file `sample_1.ml` has no syntax or type errors.



+ Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild sample_1.byte
   ```

   

    The command succeeded.

+ Pass: Check standard output of the following command
   ```
   ./sample_1.byte
   ```
   matches the following:
   ```
   3
   ```

   

    The command succeeded.

## Testing program sample_2

+ Pass: 
Check that the result of evaluating
   ```
   translate_prog program_sample_2
   ```
   matches the pattern `()`.

   




+ Pass: Check that file `sample_2.ml` exists.

+ Pass: Check that an OCaml file `sample_2.ml` has no syntax  or type errors.

    OCaml file `sample_2.ml` has no syntax or type errors.



+ Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild sample_2.byte
   ```

   

    The command succeeded.

+ Pass: Check standard output of the following command
   ```
   ./sample_2.byte
   ```
   matches the following:
   ```
   6
   ```

   

    The command succeeded.

## Testing program sample_3

+ Pass: 
Check that the result of evaluating
   ```
   translate_prog program_sample_3
   ```
   matches the pattern `()`.

   




+ Pass: Check that file `sample_3.ml` exists.

+ Pass: Check that an OCaml file `sample_3.ml` has no syntax  or type errors.

    OCaml file `sample_3.ml` has no syntax or type errors.



+ Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild sample_3.byte
   ```

   

    The command succeeded.

+ Pass: Check standard output of the following command
   ```
   ./sample_3.byte
   ```
   matches the following:
   ```
   4
   ```

   

    The command succeeded.

