# Final_Project Feedback

Run on December 12, 20:11:57 PM.

+ Pass: Change into directory "Final_Project".

## Running tests for Final Project option 3

It appears that you have decided to submit work for this option. If you did not intend to submit work for this option please email the TAs at csci2041@umn.edu to indicate that the option selectin mechanism has failed.



Because `expr.ml` is in your `Final_Project` directory you have choosen option 3. Below are tests for type checking and evaluating expressions using pairs and lists.

#### Testing `Pair (Val (Int 3), Val (Bool false))`

+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Pair (Val (Int 3), Val (Bool false))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Pair (Val (Int 3), Val (Bool false))) [] with
      | OK (Pair_type (Int_type, Bool_type)) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Pair (Val (Int 3), Val (Bool false))) with
      | PairV (Int 3, Bool false) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Pair (Add (Val (Int 3), Val (Int 4)), Val (Bool false))`

+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Pair (Add (Val (Int 3), Val (Int 4)), Val (Bool false))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Pair (Add (Val (Int 3), Val (Int 4)), Val (Bool false))) [] with
      | OK (Pair_type (Int_type, Bool_type)) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Pair (Add (Val (Int 3), Val (Int 4)), Val (Bool false))) with
      | PairV (Int 7, Bool false) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Let ("p", Pair_type (Int_type, Int_type), Pair (Val (Int 3), Val (Int 4)), Add (Fst (Id "p"), Snd (Id "p")))`

+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Let ("p", Pair_type (Int_type, Int_type), Pair (Val (Int 3), Val (Int 4)), Add (Fst (Id "p"), Snd (Id "p")))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Let ("p", Pair_type (Int_type, Int_type), Pair (Val (Int 3), Val (Int 4)), Add (Fst (Id "p"), Snd (Id "p")))) [] with
      | OK (Int_type) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Let ("p", Pair_type (Int_type, Int_type), Pair (Val (Int 3), Val (Int 4)), Add (Fst (Id "p"), Snd (Id "p")))) with
      | Int 7 -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Cons (Add (Val (Int 3), Val (Int 4)), Nil)`

+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Cons (Add (Val (Int 3), Val (Int 4)), Nil)) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Cons (Add (Val (Int 3), Val (Int 4)), Nil)) [] with
      | OK (List_type (Int_type)) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Cons (Add (Val (Int 3), Val (Int 4)), Nil)) with
      | ListV [Int 7] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Let ("lst", List_type (Int_type), Cons (Val (Int 3), Cons (Val (Int 4), Nil)), Id "lst")`

+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Let ("lst", List_type (Int_type), Cons (Val (Int 3), Cons (Val (Int 4), Nil)), Id "lst")) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Let ("lst", List_type (Int_type), Cons (Val (Int 3), Cons (Val (Int 4), Nil)), Id "lst")) [] with
      | OK (List_type (Int_type)) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Let ("lst", List_type (Int_type), Cons (Val (Int 3), Cons (Val (Int 4), Nil)), Id "lst")) with
      | ListV [Int 3; Int 4] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Eq (Nil, Nil)`

+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Eq (Nil, Nil)) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Eq (Nil, Nil)) [] with
      | OK (Bool_type) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Eq (Nil, Nil)) with
      | Bool true -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Eq (Cons (Val (Int 3), Nil), Nil)`

+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Eq (Cons (Val (Int 3), Nil), Nil)) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Eq (Cons (Val (Int 3), Nil), Nil)) [] with
      | OK (Bool_type) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Eq (Cons (Val (Int 3), Nil), Nil)) with
      | Bool false -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Head (Cons (Val (Int 3), Nil))`

+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Head (Cons (Val (Int 3), Nil))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Head (Cons (Val (Int 3), Nil))) [] with
      | OK (Int_type) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Head (Cons (Val (Int 3), Nil))) with
      | Int 3 -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Tail (Cons (Val (Int 3), Nil))`

+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Tail (Cons (Val (Int 3), Nil))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Tail (Cons (Val (Int 3), Nil))) [] with
      | OK (List_type (Int_type)) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Tail (Cons (Val (Int 3), Nil))) with
      | ListV [] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Tail (Cons (Val (Int 3), Cons (Val (Int 4), Nil)))`

+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Tail (Cons (Val (Int 3), Cons (Val (Int 4), Nil)))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (Tail (Cons (Val (Int 3), Cons (Val (Int 4), Nil)))) [] with
      | OK (List_type (Int_type)) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (Tail (Cons (Val (Int 3), Cons (Val (Int 4), Nil)))) with
      | ListV [Int 4] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `LetRec ("sum", Func_type (List_type (Int_type), Int_type),Lam ("lst", List_type (Int_type), If (Eq (Id "lst", Nil), Val (Int 0), Add (Head (Id "lst"), App (Id "sum", Tail (Id "lst"))))), App (Id "sum", Cons (Val (Int 1), Cons (Val (Int 2), Cons (Val (Int 3), Nil)))))`

+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (LetRec ("sum", Func_type (List_type (Int_type), Int_type),Lam ("lst", List_type (Int_type), If (Eq (Id "lst", Nil), Val (Int 0), Add (Head (Id "lst"), App (Id "sum", Tail (Id "lst"))))), App (Id "sum", Cons (Val (Int 1), Cons (Val (Int 2), Cons (Val (Int 3), Nil)))))) [] with
      | OK _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has a type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
   match type_check (LetRec ("sum", Func_type (List_type (Int_type), Int_type),Lam ("lst", List_type (Int_type), If (Eq (Id "lst", Nil), Val (Int 0), Add (Head (Id "lst"), App (Id "sum", Tail (Id "lst"))))), App (Id "sum", Cons (Val (Int 1), Cons (Val (Int 2), Cons (Val (Int 3), Nil)))))) [] with
      | OK (Int_type) -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match eval [] (LetRec ("sum", Func_type (List_type (Int_type), Int_type),Lam ("lst", List_type (Int_type), If (Eq (Id "lst", Nil), Val (Int 0), Add (Head (Id "lst"), App (Id "sum", Tail (Id "lst"))))), App (Id "sum", Cons (Val (Int 1), Cons (Val (Int 2), Cons (Val (Int 3), Nil)))))) with
      | Int 6 -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression evaluates to the correct value.




#### Testing `Fst (Val (Int 3))`

+ Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Fst (Val (Int 3))) [] with
      | Errs _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has type errors.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Fst (Val (Int 3))) [] with
      | Errs [ (Fst (Val (Int 3)) ,_) ] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type errors.




#### Testing `Snd (Val (Int 3))`

+ Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Snd (Val (Int 3))) [] with
      | Errs _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has type errors.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Snd (Val (Int 3))) [] with
      | Errs [ (Snd (Val (Int 3)) ,_) ] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type errors.




#### Testing `Cons (Add (Val (Int 3), Val (Bool false)), Nil)`

+ Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Cons (Add (Val (Int 3), Val (Bool false)), Nil)) [] with
      | Errs _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has type errors.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Cons (Add (Val (Int 3), Val (Bool false)), Nil)) [] with
      | Errs [ (Val (Bool false) ,_) ] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type errors.




#### Testing `Cons (Val (Int 3), Val (Int 4))`

+ Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Cons (Val (Int 3), Val (Int 4))) [] with
      | Errs _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has type errors.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Cons (Val (Int 3), Val (Int 4))) [] with
      | Errs [ (Cons (Val (Int 3), Val (Int 4)) ,_) ] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type errors.




#### Testing `Head (Cons (Add (Val (Int 3), Val (Bool false)), Nil))`

+ Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Head (Cons (Add (Val (Int 3), Val (Bool false)), Nil))) [] with
      | Errs _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has type errors.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Head (Cons (Add (Val (Int 3), Val (Bool false)), Nil))) [] with
      | Errs [ (Val (Bool false) ,_) ] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type errors.




#### Testing `Tail (Cons (Add (Val (Int 3), Val (Bool false)), Nil))`

+ Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Tail (Cons (Add (Val (Int 3), Val (Bool false)), Nil))) [] with
      | Errs _ -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has type errors.




+ Pass: 
Check that the result of evaluating
   ```ocaml
    match type_check (Tail (Cons (Add (Val (Int 3), Val (Bool false)), Nil))) [] with
      | Errs [ (Val (Bool false) ,_) ] -> true
      | _ -> false

   ```
   matches the pattern `true`.

   This test checks if the expression has the correct type errors.




