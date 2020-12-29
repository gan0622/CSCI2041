# Quiz_4 Assessment

*Test results are for the code in this repository as it appeared on December 09 at 17:15 CT.*

+ Pass: Change into directory "Quiz_4".

+ Pass: Check that file "translator.ml" exists.

+  _5_ / _5_ : Pass: Reasonable attempt at Seq, WriteNum and at least one other construct.

    

+  _3_ / _5_ : Pass: Style: the code is readable and transparent.

    the While clause should not be in one single long line

## Testing program assess_1

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   translate_prog ("assess_1", [],
      WriteNum (Val (Int 3))
     )

   ```
   matches the pattern `()`.

   




+ Pass: Check that file `assess_1.ml` exists.

+ Pass: Check that an OCaml file `assess_1.ml` has no syntax  or type errors.

    OCaml file `assess_1.ml` has no syntax or type errors.



+  _1_ / _1_ : Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild assess_1.byte
   ```

   

    The command succeeded.

+  _3_ / _3_ : Pass: Check standard output of the following command
   ```
   ./assess_1.byte
   ```
   matches the following:
   ```
   3
   ```

   

    The command succeeded.

## Testing program assess_2

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   translate_prog ("assess_2", [],
      WriteNum (Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))))
     )

   ```
   matches the pattern `()`.

   




+ Pass: Check that file `assess_2.ml` exists.

+ Pass: Check that an OCaml file `assess_2.ml` has no syntax  or type errors.

    OCaml file `assess_2.ml` has no syntax or type errors.



+  _1_ / _1_ : Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild assess_2.byte
   ```

   

    The command succeeded.

+  _3_ / _3_ : Pass: Check standard output of the following command
   ```
   ./assess_2.byte
   ```
   matches the following:
   ```
   7
   ```

   

    The command succeeded.

## Testing program assess_3

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   translate_prog ("assess_3", ["x"],
      Seq (Assign ("x", Val (Int 4)),
           WriteNum (Id "x")
        )
     )

   ```
   matches the pattern `()`.

   




+ Pass: Check that file `assess_3.ml` exists.

+ Pass: Check that an OCaml file `assess_3.ml` has no syntax  or type errors.

    OCaml file `assess_3.ml` has no syntax or type errors.



+  _1_ / _1_ : Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild assess_3.byte
   ```

   

    The command succeeded.

+  _3_ / _3_ : Pass: Check standard output of the following command
   ```
   ./assess_3.byte
   ```
   matches the following:
   ```
   4
   ```

   

    The command succeeded.

## Testing program assess_4

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   translate_prog ("assess_4", ["a"; "b"; "c" ],
      Seq (Assign ("a", Val (Int 10)),
      Seq (Assign ("b", Mul (Id "a", Val (Int 3))),
      Seq (Assign ("c", Add (Id "a", Id "b")),
           WriteNum (Id "c")
        ) ) )
     )

   ```
   matches the pattern `()`.

   




+ Pass: Check that file `assess_4.ml` exists.

+ Pass: Check that an OCaml file `assess_4.ml` has no syntax  or type errors.

    OCaml file `assess_4.ml` has no syntax or type errors.



+  _1_ / _1_ : Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild assess_4.byte
   ```

   

    The command succeeded.

+  _3_ / _3_ : Pass: Check standard output of the following command
   ```
   ./assess_4.byte
   ```
   matches the following:
   ```
   40
   ```

   

    The command succeeded.

## Testing program assess_5

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   translate_prog ("assess_5", ["x"],
      Seq (Assign ("x", Val (Int 4)),
      Seq (IfThenElse (
               Lt (Id "x", Val (Int 6)),
               Assign ("x", Val (Int 2)),
               Assign ("x", Val (Int 8)) ),
           WriteNum (Id "x")
        ) )
     )

   ```
   matches the pattern `()`.

   




+ Pass: Check that file `assess_5.ml` exists.

+ Pass: Check that an OCaml file `assess_5.ml` has no syntax  or type errors.

    OCaml file `assess_5.ml` has no syntax or type errors.



+  _1_ / _1_ : Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild assess_5.byte
   ```

   

    The command succeeded.

+  _3_ / _3_ : Pass: Check standard output of the following command
   ```
   ./assess_5.byte
   ```
   matches the following:
   ```
   2
   ```

   

    The command succeeded.

## Testing program assess_6

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   translate_prog ("assess_6", ["prod"; "x"],
      Seq (Assign ("prod", Val (Int 1)),
      Seq (Assign ("x", Val (Int 1)),
      Seq (While (Lt (Id "x", Val (Int 5)),
                  Seq (Assign ("prod", Mul (Id "prod", Id "x")),
                       Assign ("x", Add (Id "x", Val (Int 1))) )
                 ),
           WriteNum (Id "prod")
        ) ) )
     )

   ```
   matches the pattern `()`.

   




+ Pass: Check that file `assess_6.ml` exists.

+ Pass: Check that an OCaml file `assess_6.ml` has no syntax  or type errors.

    OCaml file `assess_6.ml` has no syntax or type errors.



+  _1_ / _1_ : Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild assess_6.byte
   ```

   

    The command succeeded.

+  _3_ / _3_ : Pass: Check standard output of the following command
   ```
   ./assess_6.byte
   ```
   matches the following:
   ```
   24
   ```

   

    The command succeeded.

## Testing program assess_7

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   translate_prog ("assess_7", ["x"],
   Seq (Assign ("x", Val (Int 4)),
   Seq (IfThen (
            Lt (Id "x", Val (Int 2)),
            Assign ("x", Val (Int 1))
          ),
        WriteNum (Id "x")
     ) )
  )

   ```
   matches the pattern `()`.

   




+ Pass: Check that file `assess_7.ml` exists.

+ Pass: Check that an OCaml file `assess_7.ml` has no syntax  or type errors.

    OCaml file `assess_7.ml` has no syntax or type errors.



+  _1_ / _1_ : Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild assess_7.byte
   ```

   

    The command succeeded.

+  _3_ / _3_ : Pass: Check standard output of the following command
   ```
   ./assess_7.byte
   ```
   matches the following:
   ```
   4
   ```

   

    The command succeeded.

## Testing program assess_8

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```ocaml
   translate_prog   ("assess_8",
   ["prod"; "x"],
   Seq (Assign ("prod", Val (Int 1)),
   Seq (For ("x", Val (Int 1), Val (Int 4),
             Assign("prod", Mul (Id  "prod", Id  "x"))),
        WriteNum (Id  "prod")
     ) )
  )

   ```
   matches the pattern `()`.

   




+ Pass: Check that file `assess_8.ml` exists.

+ Pass: Check that an OCaml file `assess_8.ml` has no syntax  or type errors.

    OCaml file `assess_8.ml` has no syntax or type errors.



+  _1_ / _1_ : Pass: Check that the following command has a return code of 0:
   ```
   ocamlbuild assess_8.byte
   ```

   

    The command succeeded.

+  _3_ / _3_ : Pass: Check standard output of the following command
   ```
   ./assess_8.byte
   ```
   matches the following:
   ```
   24
   ```

   

    The command succeeded.

#### Total score: _48_ / _50_

