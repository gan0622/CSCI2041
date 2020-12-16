let sum_a lst =
  let rec accum sum lst' = match lst' with
    | [] -> sum
    | x::xs -> accum (sum + x) xs
  in
  accum 0 lst

(* Exercise S4.2: #1: Tail recursive length )
let length_a lst =
  let rec accum len lst' = match lst' with
  | [] -> len
  | x::xs -> accum (len + 1) xs
in
accum 0 lst

( Exercise S4.2: #2: folds 
fold_left is tail recursive and fast
fold_right is a more general pattern, replace constructors (cons :: and nil [ ]) with functions or values
)

( Exercise S4.2: #3: fibonacci numbers )
let fib' n = 
  let rec f n1 n2 n' = 
    if n' = n 
    then n2
    else f (n1 + n2) n1 (n'+1)
  in
  f 1 0 0 (computed sum, return, # in series*)