let cond c t e = if c then t else e

let rec sumToN n = cond (n = 0) 0 (n + sumToN (n-1))




(* OCaml functions for comparing leaves in binary trees.  Based on
   examples in Chapters 8 and 9 of Chris Reade's Elements of
   Functional Programming *)

   type 'a bintree = Lf of 'a
   | Nd of 'a bintree * 'a bintree

let rec equal_list l1 l2 = match l1, l2 with
| [], [] -> true
| (x::xs), (y::ys) -> if x = y then equal_list xs ys else false
| _, _ -> false

let rec append l1 l2 = match l1 with
| [] -> l2
| (x::xs) -> x :: append xs l2

let rec flatten t = match t with
| Lf x -> [x]
| Nd (t1,t2) -> append (flatten t1) (flatten t2)

(* If we evaluate the following function eagerly, it is slow because it 
must flatten each tree completely before making any comparisons. 
If we evaluate it lazily, we can avoid some unnecessary computaions. *)
let eqleaves_v1 t1 t2 = equal_list (flatten t1) (flatten t2)




(* This is the fast version that only flattens trees as much as is
necessary.  This complexity is needed in a language that uses eager
evaluation.  It is not needed in a lazy language.  *)
let rec eqleaves_v2 t1 t2 = comparestacks [t1] [t2]
and comparestacks f1 f2 = 
match f1, f2 with
| [ ], [ ] -> true
| [ ], a::x -> false
| a::x, [ ] -> false
| (Nd (l, r) :: x), y -> comparestacks (l::r::x) y
| x, (Nd (l, r) :: y) -> comparestacks x (l::r::y)
| (Lf a)::x, (Lf b)::y when a = b -> comparestacks x y
| _, _ -> false


(* a few simple sample trees *)
let t1 = Lf (3 * 2)
let t2 = Nd (Lf (3 + 3), Lf 5)





(*
sum (take 2 (squares_from 3))

sum (take 2 ( 3*3 :: squares_from (3+1) )

sum ( (3*3) :: take (2-1) (squares_from (3+1)) )

(3*3) + (sum (take (2-1) (squares_from (3+1))))

9 + (sum (take (2-1) (squares_from (3+1))))

9 + (sum (take 1 (squares_from (3+1))))

9 + (sum (take 1 ( (a1 * a1) :: squares_from (a1 + 1))))
  where a1 = 3+1

9 + (sum ( (a1 * a1) :: (take (1-1) ( squares_from (a1 + 1)))))
  where a1 = 3+1 

9 + ((a1 * a1) + ( sum (take (1-1) ( squares_from (a1 + 1)))))
  where a1 = 3+1 

9 + ((a1 * a1) + ( sum (take (1-1) ( squares_from (a1 + 1)))))
  where a1 = 4

9 + ((4 * 4) + ( sum (take (1-1) ( squares_from (4 + 1)))))

9 + (16 + ( sum (take 0 ( squares_from (4 + 1)))))

9 + (16 + ( sum [] ) )

9 + (16 + ( 0 ) )

9 + 16

25
*)




(*
allnats = let ns n = n :: ns (n + 1) in ns 0

[0,1,2,3,4,5,6,7,8,9,10]

drop 3 allnats:

drop 3 [0,1,2,3,4,5,6,7,8,9,10]

drop 3 - 1 > 0 then [1,2,3,4,5,6,7,8,9,10]

drop 2 - 1 > 0 then [2,3,4,5,6,7,8,9,10]

drop 1 - 1 > 0 then [3,4,5,6,7,8,9,10]

drop 0 > 0 then [3,4,5,6,7,8,9,10]

[3,4,5,6,7,8,9,10]



*)