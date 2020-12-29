# Final_Project Assessment

Run on December 20, 18:08:18 PM.

*Test results are for the code in this repository as it appeared on December 19 at 00:15 CT.*

+ Pass: Change into directory "Final_Project".

The following tests that check if certain files exist are here only to indicate in the gradebook which option you choose.  These scoress should be ignored; they are not part of the total score for this assignment.

+  _0_ / _1_ : Fail: Check that file `hwk_02.ml` exists.

     `hwk_02.ml` not found.

+ Fail: Check that file `hwk_02.ml` exists.

     `hwk_02.ml` not found.

+  _0_ / _1_ : Fail: Check that file `qu_quiz.ml` exists.

     `qu_quiz.ml` not found.

+ Fail: Check that file `qu_quiz.ml` exists.

     `qu_quiz.ml` not found.

+  _0_ / _1_ : Fail: Check that file `it_takes_two.ml` exists.

     `it_takes_two.ml` not found.

+ Fail: Check that file `it_takes_two.ml` exists.

     `it_takes_two.ml` not found.

+  _1_ / _1_ : Pass: Check that file `expr.ml` exists.

+ Pass: Check that file `expr.ml` exists.

+ Pass: Testing continuing even if some previous tests fail.

## Running tests for Final Project option 3

It appears that you have decided to submit work for this option. If you did not intend to submit work for this option please email the TAs at csci2041@umn.edu to indicate that the option selection mechanism has failed.



Because `expr.ml` is in your `Final_Project` directory you have choosen option 3. Below are tests for type checking and evaluating expressions using pairs and lists.

#### Testing `Let ("zero", Int_type, Val (Int 0), LetRec ("sumToN", Func_type (Int_type, Int_type),Lam ("n", Int_type, If (Eq (Id "n", Val (Int 0)), Id "zero", Add (Id "n", App (Id "sumToN", Sub (Id "n", Val (Int 1)))))), App (Id "sumToN", Val (Int 4))))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Let ("zero", Int_type, Val (Int 0), LetRec ("sumToN", Func_type (Int_type, Int_type),Lam ("n", Int_type, If (Eq (Id "n", Val (Int 0)), Id "zero", Add (Id "n", App (Id "sumToN", Sub (Id "n", Val (Int 1)))))), App (Id "sumToN", Val (Int 4))))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Let ("zero", Int_type, Val (Int 0), LetRec ("sumToN", Func_type (Int_type, Int_type),Lam ("n", Int_type, If (Eq (Id "n", Val (Int 0)), Id "zero", Add (Id "n", App (Id "sumToN", Sub (Id "n", Val (Int 1)))))), App (Id "sumToN", Val (Int 4))))) [] with
      | OK (Int_type) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Let ("zero", Int_type, Val (Int 0), LetRec ("sumToN", Func_type (Int_type, Int_type),Lam ("n", Int_type, If (Eq (Id "n", Val (Int 0)), Id "zero", Add (Id "n", App (Id "sumToN", Sub (Id "n", Val (Int 1)))))), App (Id "sumToN", Val (Int 4))))) with
      | Int 10 -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Pair (Val (Int 3), Val (Bool false))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Pair (Val (Int 3), Val (Bool false))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Pair (Val (Int 3), Val (Bool false))) [] with
      | OK (Pair_type (Int_type, Bool_type)) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Pair (Val (Int 3), Val (Bool false))) with
      | PairV (Int 3, Bool false) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Pair (Add (Val (Int 3), Val (Int 4)), Val (Bool false))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Pair (Add (Val (Int 3), Val (Int 4)), Val (Bool false))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Pair (Add (Val (Int 3), Val (Int 4)), Val (Bool false))) [] with
      | OK (Pair_type (Int_type, Bool_type)) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Pair (Add (Val (Int 3), Val (Int 4)), Val (Bool false))) with
      | PairV (Int 7, Bool false) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Let ("p", Pair_type (Int_type, Int_type), Pair (Val (Int 3), Val (Int 4)), Add (Fst (Id "p"), Snd (Id "p")))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Let ("p", Pair_type (Int_type, Int_type), Pair (Val (Int 3), Val (Int 4)), Add (Fst (Id "p"), Snd (Id "p")))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Let ("p", Pair_type (Int_type, Int_type), Pair (Val (Int 3), Val (Int 4)), Add (Fst (Id "p"), Snd (Id "p")))) [] with
      | OK (Int_type) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Let ("p", Pair_type (Int_type, Int_type), Pair (Val (Int 3), Val (Int 4)), Add (Fst (Id "p"), Snd (Id "p")))) with
      | Int 7 -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Cons (Add (Val (Int 3), Val (Int 4)), Nil)`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Cons (Add (Val (Int 3), Val (Int 4)), Nil)) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Cons (Add (Val (Int 3), Val (Int 4)), Nil)) [] with
      | OK (List_type (Int_type)) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Cons (Add (Val (Int 3), Val (Int 4)), Nil)) with
      | ListV [Int 7] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Let ("lst", List_type (Int_type), Cons (Val (Int 3), Cons (Val (Int 4), Nil)), Id "lst")`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Let ("lst", List_type (Int_type), Cons (Val (Int 3), Cons (Val (Int 4), Nil)), Id "lst")) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Let ("lst", List_type (Int_type), Cons (Val (Int 3), Cons (Val (Int 4), Nil)), Id "lst")) [] with
      | OK (List_type (Int_type)) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Let ("lst", List_type (Int_type), Cons (Val (Int 3), Cons (Val (Int 4), Nil)), Id "lst")) with
      | ListV [Int 3; Int 4] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Eq (Nil, Nil)`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Eq (Nil, Nil)) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Eq (Nil, Nil)) [] with
      | OK (Bool_type) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Eq (Nil, Nil)) with
      | Bool true -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Eq (Cons (Val (Int 3), Nil), Nil)`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Eq (Cons (Val (Int 3), Nil), Nil)) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Eq (Cons (Val (Int 3), Nil), Nil)) [] with
      | OK (Bool_type) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Eq (Cons (Val (Int 3), Nil), Nil)) with
      | Bool false -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Head (Cons (Val (Int 3), Nil))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Head (Cons (Val (Int 3), Nil))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Head (Cons (Val (Int 3), Nil))) [] with
      | OK (Int_type) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Head (Cons (Val (Int 3), Nil))) with
      | Int 3 -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Tail (Cons (Val (Int 3), Nil))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Tail (Cons (Val (Int 3), Nil))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Tail (Cons (Val (Int 3), Nil))) [] with
      | OK (List_type (Int_type)) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Tail (Cons (Val (Int 3), Nil))) with
      | ListV [] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Tail (Cons (Val (Int 3), Cons (Val (Int 4), Nil)))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Tail (Cons (Val (Int 3), Cons (Val (Int 4), Nil)))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Tail (Cons (Val (Int 3), Cons (Val (Int 4), Nil)))) [] with
      | OK (List_type (Int_type)) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Tail (Cons (Val (Int 3), Cons (Val (Int 4), Nil)))) with
      | ListV [Int 4] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `LetRec ("sum", Func_type (List_type (Int_type), Int_type),Lam ("lst", List_type (Int_type), If (Eq (Id "lst", Nil), Val (Int 0), Add (Head (Id "lst"), App (Id "sum", Tail (Id "lst"))))), App (Id "sum", Cons (Val (Int 1), Cons (Val (Int 2), Cons (Val (Int 3), Nil)))))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (LetRec ("sum", Func_type (List_type (Int_type), Int_type),Lam ("lst", List_type (Int_type), If (Eq (Id "lst", Nil), Val (Int 0), Add (Head (Id "lst"), App (Id "sum", Tail (Id "lst"))))), App (Id "sum", Cons (Val (Int 1), Cons (Val (Int 2), Cons (Val (Int 3), Nil)))))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (LetRec ("sum", Func_type (List_type (Int_type), Int_type),Lam ("lst", List_type (Int_type), If (Eq (Id "lst", Nil), Val (Int 0), Add (Head (Id "lst"), App (Id "sum", Tail (Id "lst"))))), App (Id "sum", Cons (Val (Int 1), Cons (Val (Int 2), Cons (Val (Int 3), Nil)))))) [] with
      | OK (Int_type) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (LetRec ("sum", Func_type (List_type (Int_type), Int_type),Lam ("lst", List_type (Int_type), If (Eq (Id "lst", Nil), Val (Int 0), Add (Head (Id "lst"), App (Id "sum", Tail (Id "lst"))))), App (Id "sum", Cons (Val (Int 1), Cons (Val (Int 2), Cons (Val (Int 3), Nil)))))) with
      | Int 6 -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Fst (Val (Int 3))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Fst (Val (Int 3))) [] with
      | Errs _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has type errors.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Fst (Val (Int 3))) [] with
      | Errs [ (Fst (Val (Int 3)) ,_) ] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type errors.




#### Testing `Snd (Val (Int 3))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Snd (Val (Int 3))) [] with
      | Errs _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has type errors.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Snd (Val (Int 3))) [] with
      | Errs [ (Snd (Val (Int 3)) ,_) ] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type errors.




#### Testing `Cons (Add (Val (Int 3), Val (Bool false)), Nil)`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Cons (Add (Val (Int 3), Val (Bool false)), Nil)) [] with
      | Errs _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has type errors.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Cons (Add (Val (Int 3), Val (Bool false)), Nil)) [] with
      | Errs [ (Val (Bool false) ,_) ] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type errors.




#### Testing `Cons (Val (Int 3), Val (Int 4))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Cons (Val (Int 3), Val (Int 4))) [] with
      | Errs _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has type errors.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Cons (Val (Int 3), Val (Int 4))) [] with
      | Errs [ (Cons (Val (Int 3), Val (Int 4)) ,_) ] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type errors.




#### Testing `Head (Cons (Add (Val (Int 3), Val (Bool false)), Nil))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Head (Cons (Add (Val (Int 3), Val (Bool false)), Nil))) [] with
      | Errs _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has type errors.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Head (Cons (Add (Val (Int 3), Val (Bool false)), Nil))) [] with
      | Errs [ (Val (Bool false) ,_) ] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type errors.




#### Testing `Tail (Cons (Add (Val (Int 3), Val (Bool false)), Nil))`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Tail (Cons (Add (Val (Int 3), Val (Bool false)), Nil))) [] with
      | Errs _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has type errors.




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Tail (Cons (Add (Val (Int 3), Val (Bool false)), Nil))) [] with
      | Errs [ (Val (Bool false) ,_) ] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type errors.




+  _10_ / _10_ : Pass: The function `type_check` makes a reasonable effort to correctly type-check at least some of the pair constructs `Pair`, `Fst`, and `Snd`

    

+  _10_ / _10_ : Pass: The function `type_check` makes a reasonable effort to correctly type-check at least some of the list constructs `Nil`, `Cons`, 'Head', and `Tail`

    

+  _10_ / _10_ : Pass: The function `eval` makes a reasonable effort to correctly evaluate at least some of the pair constructs `Pair`, `Fst`, and `Snd`

    

+  _10_ / _10_ : Pass: The function `eval` makes a reasonable effort to correctly evaluate at least some of the list constructs `Nil`, `Cons`, 'Head', and `Tail`

    

#### Total score: _136_ / _136_

