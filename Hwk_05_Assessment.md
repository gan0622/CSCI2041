# Hwk_05 Assessment

*Test results are for the code in this repository as it appeared on November 12 at 17:15 CT.*

+ Pass: Change into directory "Hwk_05".

+ Pass: Check that file "expr.ml" exists.

## Part 1: Unparse Expressions

For the following tests, `filter_string` removes parentheses and spaces from your output. The filtered output is then compared with the filtered solution. This is to ensure we give points to different correct answers.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (Val (Int 12))) ['('; ' '; ')']
   ```
   matches the pattern `("12")`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (Id "y")) ['('; ' '; ')']
   ```
   matches the pattern `("y")`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (App (Id "inc", Val (Bool true)))) ['('; ' '; ')']
   ```
   matches the pattern `("inctrue")`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (LetRec ("x", Int_type, Add (Val (Int 3), Val (Int 4)), Add (Id "x", Val (Int 5))))) ['('; ' '; ')']
   ```
   matches the pattern `("letrecx:int=3+4inx+5")`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (Add (Val (Int 3), Val (Int 5)))) ['('; ' '; ')']
   ```
   matches the pattern `("3+5")`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (Add (Val (Int 3), Val (Bool true)))) ['('; ' '; ')']
   ```
   matches the pattern `("3+true")`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (Mul (Val (Bool true), Val (Int 5)))) ['('; ' '; ')']
   ```
   matches the pattern `("true*5")`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), Add (Id "x", Val (Int 5))))) ['('; ' '; ')']
   ```
   matches the pattern `("letx:int=3+4inx+5")`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (Let ("x", Bool_type,Lt (Val (Int 3), Val (Int 5)), And (Id "y", Let ("x", Int_type,Add (Val (Int 1), Val (Int 2)), Eq (Id "x", Val (Int 3))))))) ['('; ' '; ')']
   ```
   matches the pattern `("letx:bool=3<5iny&&letx:int=1+2inx=3")`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (Let ("inc", Func_type (Int_type, Int_type),Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Int 3))))) ['('; ' '; ')']
   ```
   matches the pattern `("letinc:int->int=funn:int->n+1ininc3")`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (If (Eq (Val (Int 4), Val (Int 0)), Val (Int 0), Val (Int 1)))) ['('; ' '; ')']
   ```
   matches the pattern `("if4=0then0else1")`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (Let ("x", Bool_type,Mul (Val (Int 3), Val (Int 5)), Add (Id "x", Val (Int 3))))) ['('; ' '; ')']
   ```
   matches the pattern `("letx:bool=3*5inx+3")`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   filter_string (unparse (LetRec ("sumToN", Func_type (Int_type, Int_type),Lam ("n", Int_type, If (Eq (Id "n", Val (Int 0)), Val (Int 0), Add (Id "n", App (Id "sumToN", Sub (Id "n", Val (Int 1)))))), Id "sumToN"))) ['('; ' '; ')']
   ```
   matches the pattern `("letrecsumToN:int->int=funn:int->ifn=0then0elsen+sumToNn-1insumToN")`.

   




## Part 2: Free Variables

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort compare (freevars (Id "y"))
   ```
   matches the pattern `["y"]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort compare (freevars (LetRec ("x", Int_type, Add (Val (Int 3), Val (Int 4)), Add (Id "x", Val (Int 5)))))
   ```
   matches the pattern `[]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort compare (freevars (LetRec ("x", Int_type, Add (Id "x", Val (Int 4)), Add (Id "x", Val (Int 5)))))
   ```
   matches the pattern `[]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort compare (freevars (Let ("x", Int_type, Add (Id "x", Val (Int 4)), Add (Id "x", Val (Int 5)))))
   ```
   matches the pattern `["x"]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort compare (freevars (Lam ("n", Int_type, Add (Id "n", Val (Int 4)))))
   ```
   matches the pattern `[]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort compare (freevars (And (Id "z", And (Id "x", Id "y"))))
   ```
   matches the pattern `["x"; "y"; "z"]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort compare (freevars (Lam ("n", Int_type, Add (Id "n", Val (Int 4)))))
   ```
   matches the pattern `[]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort compare (freevars (Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), Add (Id "x", Val (Int 5)))))
   ```
   matches the pattern `[]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort compare (freevars (Let ("x", Bool_type,Lt (Val (Int 3), Val (Int 5)), And (Id "y", Let ("x", Int_type,Add (Val (Int 1), Val (Int 2)), Eq (Id "x", Val (Int 3)))))))
   ```
   matches the pattern `["y"]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort compare (freevars (Let ("x", Int_type,Add (Id "x", Val (Int 4)), And (Id "y", Val (Bool true)))))
   ```
   matches the pattern `["x"; "y"]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort compare (freevars (Let ("inc", Func_type (Int_type, Int_type),Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Bool true)))))
   ```
   matches the pattern `[]`.

   




+  _3_ / _3_ : Pass: 
Make sure that freevars is written the "right" way:
    - (Id x) returns [x]
    - It doesn't use helper function with an extra bound_vars parameter.
    - Filtering `freevars e` (not necessarily with List.filter) for the interesting cases (Let, LetRec, Lam)


    

## Part 3: Type Checking

+  _0_ / _1_ : Fail: Submitted part 3 (0: no, 1: yes).

    

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Val (Int 12)) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "OK" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Val (Int 12)) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(OK Int_type)`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Add (Val (Int 3), Val (Int 5))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "OK" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Add (Val (Int 3), Val (Int 5))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(OK Int_type)`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Add (Val (Int 3), Val (Bool true))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "Errs" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Add (Val (Int 3), Val (Bool true))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(Errs [((Val (Bool true)), "")])`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Mul (Val (Bool true), Val (Int 5))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "Errs" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Mul (Val (Bool true), Val (Int 5))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(Errs [((Val (Bool true)), "")])`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), Add (Id "x", Val (Int 5)))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "OK" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), Add (Id "x", Val (Int 5)))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(OK Int_type)`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)), Lt (Id "x", Val (Int 5)))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "OK" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)), Lt (Id "x", Val (Int 5)))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(OK Bool_type)`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Let ("x", Bool_type,Lt (Val (Int 3), Val (Int 5)), And (Id "x", Let ("x", Int_type,Add (Val (Int 1), Val (Int 2)), Eq (Id "x", Val (Int 3)))))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "OK" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Let ("x", Bool_type,Lt (Val (Int 3), Val (Int 5)), And (Id "x", Let ("x", Int_type,Add (Val (Int 1), Val (Int 2)), Eq (Id "x", Val (Int 3)))))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(OK Bool_type)`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Let ("x", Bool_type,Lt (Val (Int 3), Val (Int 5)), And (Id "y", Let ("x", Int_type,Add (Val (Int 1), Val (Int 2)), Eq (Id "x", Val (Int 3)))))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "Errs" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Let ("x", Bool_type,Lt (Val (Int 3), Val (Int 5)), And (Id "y", Let ("x", Int_type,Add (Val (Int 1), Val (Int 2)), Eq (Id "x", Val (Int 3)))))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(Errs [((Id "y"), "")])`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Let ("inc", Func_type (Int_type, Int_type),Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Int 3)))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "OK" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Let ("inc", Func_type (Int_type, Int_type),Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Int 3)))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(OK Int_type)`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (If (Eq (Val (Int 4), Val (Int 0)), Val (Int 0), Val (Int 1))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "OK" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (If (Eq (Val (Int 4), Val (Int 0)), Val (Int 0), Val (Int 1))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(OK Int_type)`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), And (Id "x", Val (Bool true)))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "Errs" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), And (Id "x", Val (Bool true)))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(Errs [((Id "x"), "")])`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Let ("x", Int_type,Add (Id "x", Val (Int 4)), And (Id "y", Val (Bool true)))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "Errs" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Let ("x", Int_type,Add (Id "x", Val (Int 4)), And (Id "y", Val (Bool true)))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(Errs [((Id "x"), "")])`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Let ("x", Bool_type,Mul (Val (Int 3), Val (Int 5)), Add (Id "x", Val (Int 3)))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "Errs" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Let ("x", Bool_type,Mul (Val (Int 3), Val (Int 5)), Add (Id "x", Val (Int 3)))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(Errs [((Let ("x", Bool_type,Mul (Val (Int 3), Val (Int 5)), Add (Id "x", Val (Int 3)))), "")])`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (Let ("inc", Func_type (Int_type, Int_type),Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Bool true)))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "Errs" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (Let ("inc", Func_type (Int_type, Int_type),Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Bool true)))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(Errs [(App (Id "inc", Val (Bool true)), "")])`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (If (Val (Int 4), Val (Int 0), Val (Int 1))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "Errs" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (If (Val (Int 4), Val (Int 0), Val (Int 1))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(Errs [((Val (Int 4)), "")])`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (If (Eq (Val (Int 4), Val (Int 4)), Val (Int 0), Val (Bool false))) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "Errs" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (If (Eq (Val (Int 4), Val (Int 4)), Val (Int 0), Val (Bool false))) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(Errs [((If (Eq (Val (Int 4), Val (Int 4)), Val (Int 0), Val (Bool false))), "")])`.

   


  This test was not run because of an earlier failing test.

+  _0_ / _1_ : Skip: 
Check that the result of evaluating
   ```
   
        (match (type_check (LetRec ("sumToN", Func_type (Int_type, Int_type),Lam ("n", Int_type, If (Eq (Id "n", Val (Int 0)), Val (Int 0), Add (Id "n", App (Id "sumToN", Sub (Id "n", Val (Int 1)))))), Id "sumToN")) []) with
        | OK _ -> "OK"
        | Errs _ -> "Errs")
        
   ```
   matches the pattern ` "OK" `.

   


  This test was not run because of an earlier failing test.

+  _0_ / _2_ : Skip: 
Check that the result of evaluating
   ```
   
        (match type_check (LetRec ("sumToN", Func_type (Int_type, Int_type),Lam ("n", Int_type, If (Eq (Id "n", Val (Int 0)), Val (Int 0), Add (Id "n", App (Id "sumToN", Sub (Id "n", Val (Int 1)))))), Id "sumToN")) [] with
        | OK x -> OK x
        | Errs ((x, _)::_) ->  Errs [(x, "")])
        
   ```
   matches the pattern `(OK (Func_type (Int_type, Int_type)))`.

   


  This test was not run because of an earlier failing test.

## Part 4: Evaluation

+  _1_ / _1_ : Pass: Submitted part 4; 0 if part 3 was submitted.

    

+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   
        (match eval [] (Mul (Val (Int 3), Val (Int 5))) with
        | Int x -> Int x
        | Bool x -> Bool x
        | Closure (s, e, env) -> Closure (s, e, env)
        | _ -> failwith ("Wrong value."))
        
   ```
   matches the pattern `(Int 15)`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   
        (match eval [] (Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), Add (Id "x", Val (Int 5)))) with
        | Int x -> Int x
        | Bool x -> Bool x
        | Closure (s, e, env) -> Closure (s, e, env)
        | _ -> failwith ("Wrong value."))
        
   ```
   matches the pattern `(Int 12)`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   
        (match eval [] (Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)), Lt (Id "x", Val (Int 5)))) with
        | Int x -> Int x
        | Bool x -> Bool x
        | Closure (s, e, env) -> Closure (s, e, env)
        | _ -> failwith ("Wrong value."))
        
   ```
   matches the pattern `(Bool false)`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   
        (match eval [] (Let ("x", Bool_type,Lt (Val (Int 3), Val (Int 5)), And (Id "x", Let ("x", Int_type,Add (Val (Int 1), Val (Int 2)), Eq (Id "x", Val (Int 3)))))) with
        | Int x -> Int x
        | Bool x -> Bool x
        | Closure (s, e, env) -> Closure (s, e, env)
        | _ -> failwith ("Wrong value."))
        
   ```
   matches the pattern `(Bool true)`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   
        (match eval [] (Let ("inc", Func_type (Int_type, Int_type),Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Int 3)))) with
        | Int x -> Int x
        | Bool x -> Bool x
        | Closure (s, e, env) -> Closure (s, e, env)
        | _ -> failwith ("Wrong value."))
        
   ```
   matches the pattern `(Int 4)`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   
        (match eval [] (If (Eq (Val (Int 4), Val (Int 0)), Val (Int 0), Val (Int 1))) with
        | Int x -> Int x
        | Bool x -> Bool x
        | Closure (s, e, env) -> Closure (s, e, env)
        | _ -> failwith ("Wrong value."))
        
   ```
   matches the pattern `(Int 1)`.

   




+  _7_ / _7_ : Pass: 
Check that the result of evaluating
   ```
   
        (match eval [] (App (LetRec ("sumToN", Func_type (Int_type, Int_type),
            Lam ("n", Int_type,
                 If (Eq (Id "n", Val (Int 0)),
                     Val (Int 0),
                     Add (Id "n", 
                          App (Id "sumToN", 
                               Sub (Id "n", Val (Int 1))
                              )
                         )
                    )
                ),
            Id "sumToN"
           ), Val (Int 10))) with
        | Int x -> Int x
        | Bool x -> Bool x
        | Closure (s, e, env) -> Closure (s, e, env)
        | _ -> failwith ("Wrong value."))
        
   ```
   matches the pattern `(Int 55)`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   
        (match eval [] (App ((App (Lam ("x", Int_type,
               Lam ("y", Int_type, Add (Id "x", Id "y"))
              ), Val (Int 1))), Val (Int 2))) with
        | Int x -> Int x
        | Bool x -> Bool x
        | Closure (s, e, env) -> Closure (s, e, env)
        | _ -> failwith ("Wrong value."))
        
   ```
   matches the pattern `(Int 3)`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   
        (match eval [] (App (Lam ("x", Int_type,
               Lam ("y", Int_type, Add (Id "x", Id "y"))
              ), Val (Int 1))) with
        | Int x -> Int x
        | Bool x -> Bool x
        | Closure (s, e, env) -> Closure (s, e, env)
        | _ -> failwith ("Wrong value."))
        
   ```
   matches the pattern `(Closure ("y", Add (Id "x", Id "y"), [("x", Int 1)]))`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   
        (match eval [] (Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))) with
        | Int x -> Int x
        | Bool x -> Bool x
        | Closure (s, e, env) -> Closure (s, e, env)
        | _ -> failwith ("Wrong value."))
        
   ```
   matches the pattern `(Int 7)`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   
        (match eval [] (Eq (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))) with
        | Int x -> Int x
        | Bool x -> Bool x
        | Closure (s, e, env) -> Closure (s, e, env)
        | _ -> failwith ("Wrong value."))
        
   ```
   matches the pattern `(Bool false)`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   
        (match eval [] (If (Lt (Val (Int 1), Mul (Val (Int 2), Val (Int 3))),
     Val (Int 4),
     Val (Int 5))) with
        | Int x -> Int x
        | Bool x -> Bool x
        | Closure (s, e, env) -> Closure (s, e, env)
        | _ -> failwith ("Wrong value."))
        
   ```
   matches the pattern `(Int 4)`.

   




**PLEASE READ: Your score is out of 100 total points, not 150 (unless you're working in a group).**

#### Total score: _100_ / _150_

