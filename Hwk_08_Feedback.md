# Hwk_08 Feedback

Run on December 10, 11:37:17 AM.

More tests to come.  Just getting started now.  Tests will be complete by around December 9 if not sooner.

+ Pass: Change into directory "Hwk_08".

## Part 1. Collecting subset sum solutions: `subsetsum_exn_ref_all`

+ Pass: Check that file "search_exceptions.ml" exists.

+ Pass: Check that an OCaml file `search_exceptions.ml` has no syntax  or type errors.

    OCaml file `search_exceptions.ml` has no syntax or type errors.



+ Pass: 
Check that the result of evaluating
   ```
   List.mem [6; -5; -1]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   List.mem [6; -3; -4; 2; -1]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   List.mem [6; -3; -5; 2]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   List.mem [2; -4; 2]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   List.mem [2; -3; 2; -1]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   List.mem [2; 6; -5; -4; 2; -1]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   List.mem [2; 6; -3; -4; -1]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   List.mem [2; 6; -3; -5]
       (subsetsum_exn_ref_all [2; 6; -3; -5; -4; 2; -1])
   ```
   matches the pattern `true`.

   




## Part 2. Continuations in the wolf-goat-cabbage problem.

+ Pass: Check that file "search_wolf.ml" exists.

+ Pass: Check that an OCaml file `search_wolf.ml` has no syntax  or type errors.

    OCaml file `search_wolf.ml` has no syntax or type errors.



+ Pass: 
Check that the result of evaluating
   ```
   List.mem 
     [(L, L, L, L); (R, L, R, L); (L, L, R, L); (R, L, R, R); (L, L, L, R); (R, R, L, R); (L, R, L, R); (R, R, R, R)]
     (crossing_all ())
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   List.mem 
     [(L, L, L, L); (R, L, R, L); (L, L, R, L); (R, R, R, L); (L, R, L, L); (R, R, L, R); (L, R, L, R); (R, R, R, R)]
     (crossing_all ())
   ```
   matches the pattern `true`.

   




## Part 3. Evaluating to 24

+ Pass: 
Check that the result of evaluating
   ```
   rat_simplify (eval (BinOp (Rat (4,1), Add, Rat (3,1) ) ) )
   ```
   matches the pattern `(7, 1)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   rat_simplify (eval (BinOp (Rat (4,1), Div, Rat (3,1) ) ) )
   ```
   matches the pattern `(4, 3)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   try rat_simplify (eval (BinOp (Rat (4,1), Div, Rat (0,1) ) ) ) with
    | EvalError DivByZero -> (99,99)
   ```
   matches the pattern `(99,99)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   find_expr [(8,1); (3,1); (8,1); (3,1)]
   ```
   matches the pattern `Some (BinOp (Rat (8, 1), Div, BinOp (Rat (3, 1), Sub, BinOp (Rat (8, 1), Div, Rat (3, 1)))))`.

   




+ Pass: 
Check that the result of evaluating
   ```
   match find_expr [(4,1); (2,1); (6,1); (3,1)] with
  | None -> false
  | Some e -> e = BinOp (Rat (4, 1), Add,
                         BinOp (Rat (2, 1), Add, 
                                BinOp (Rat (6, 1), Mul, Rat (3, 1))))
              ||
              e = BinOp (Rat (4, 1), Mul, 
                         BinOp (Rat (2, 1), Mul,
                                BinOp (Rat (6, 1), Sub, Rat (3, 1))))

   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   find_expr [(2,1); (3,1); (5,1); (7,1)]
   ```
   matches the pattern `None`.

   




