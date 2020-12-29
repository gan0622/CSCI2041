# Hwk_08 Assessment

Run on December 20, 12:26:18 PM.

*Test results are for the code in this repository as it appeared on December 16 at 17:15 CT.*

+ Pass: Change into directory "Hwk_08".

+ Pass: Check that file "search_exceptions.ml" exists.

``Part 1``

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem [4; 2; 1; -7]
       (subsetsum_exn_ref_all [3; 4; -5; 2; 1; -7])
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem [4; -5; 1]
       (subsetsum_exn_ref_all [3; 4; -5; 2; 1; -7])
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem [3; -5; 2]
       (subsetsum_exn_ref_all [3; 4; -5; 2; 1; -7])
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem [3; 4; -7]
       (subsetsum_exn_ref_all [3; 4; -5; 2; 1; -7])
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   subsetsum_exn_ref_all [3; 4; -2; -9]
   ```
   matches the pattern `[]`.

   




+  _5_ / _5_ : Pass: The function subsetsum_exn_ref_all uses OCaml references in an appropriate way and the function is a good effort at a solution

    

+ Pass: Check that file "search_wolf.ml" exists.

``Part 2``

+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```ocaml
   
match (crossing_continuation (fun c -> Some c)) with
  | None -> false
  | Some c -> List.mem c
    [[(L, L, L, L); (R, L, R, L); (L, L, R, L); (R, L, R, R); (L, L, L, R);
      (R, R, L, R); (L, R, L, R); (R, R, R, R)];
     [(L, L, L, L); (R, L, R, L); (L, L, R, L); (R, R, R, L); (L, R, L, L);
      (R, R, L, R); (L, R, L, R); (R, R, R, R)]]

   ```
   matches the pattern `true`.

   




+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```ocaml
   
  List.sort compare (crossing_all ()) =
    [[(L, L, L, L); (R, L, R, L); (L, L, R, L); (R, L, R, R); (L, L, L, R);
      (R, R, L, R); (L, R, L, R); (R, R, R, R)];
     [(L, L, L, L); (R, L, R, L); (L, L, R, L); (R, R, R, L); (L, R, L, L);
      (R, R, L, R); (L, R, L, R); (R, R, R, R)]]

   ```
   matches the pattern `true`.

   




+  _5_ / _5_ : Pass: crossing_continuation uses a continuation as required and the code is a reasonable attempt

    

+  _5_ / _5_ : Pass: crossing_all shows reasonable effort and uses a reference to hold the list of solutions

    

+ Pass: Check that file "twentyfour.ml" exists.

``Part 3``

+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```ocaml
   rat_simplify (eval (BinOp (Rat (4,1), Add, Rat (3,1) ) ) )
   ```
   matches the pattern `(7, 1)`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```ocaml
   rat_simplify (eval (BinOp (Rat (4,1), Div, Rat (3,1) ) ) )
   ```
   matches the pattern `(4, 3)`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```ocaml
   try rat_simplify (eval (BinOp (Rat (4,1), Div, Rat (0,1) ) ) ) with
    | EvalError DivByZero -> (99,99)
   ```
   matches the pattern `(99,99)`.

   




+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```ocaml
   find_expr [(8,1); (3,1); (8,1); (3,1)]
   ```
   matches the pattern `Some (BinOp (Rat (8, 1), Div, BinOp (Rat (3, 1), Sub, BinOp (Rat (8, 1), Div, Rat (3, 1)))))`.

   




+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```ocaml
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

   




+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```ocaml
   find_expr [(2,1); (3,1); (5,1); (7,1)]
   ```
   matches the pattern `None`.

   




+  _5_ / _5_ : Pass: reasonable style and proper indentation

    

+  _5_ / _5_ : Pass: shows reasonable effort

    

#### Total score: _100_ / _100_

