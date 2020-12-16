(*
call - by - name 

a x y = x + 3
      = (3 + 4) + 3

call_by_value

no value


*)

(*

take 2 (makefrom 4 5)
= take 2 (5 :: makefrom (4-1) (5+1))
= 5 :: take (2-1) (makefrom (4-1) (5+1))
= 5 :: take (2-1) (makefrom 3 (5+1))
= 5 :: take (2-1) (v1 :: makefrom (3-1) (v1+1))
    where v1 = 5+1
= 5 :: take 1 (v1 :: makefrom (3-1) (v1+1))
    where v1 = 5+1
= 5 :: (v1 :: (take (1-1) (makefrom (3-1) (v1+1))))
    where v1 = 5+1
= 5 :: (v1 :: (take (1-1) (makefrom (3-1) (v1+1))))
    where v1 = 6
= 5 :: (6 :: (take (1-1) (makefrom (3-1) (6+1))))
    where v1 = 6
= 5 :: (6 :: (take (1-1) (makefrom (3-1) (6+1))))
= 5 :: (6 :: (take (1-1) (makefrom 2 (6+1))))
= 5 :: (6 :: (take (1-1) (v2 :: makefrom (2-1) (v2+1))))
    where v2 = 6 + 1
= 5 :: (6 :: (take 0 (v2 :: makefrom (2-1) (v2+1))))
    where v2 = 6 + 1
= 5 :: (6 :: [])
    where v2 = 6 + 1
= 5 :: (6 :: [])

*)






(* OCaml functions for comparing leaves in binary trees.  Based on
   examples in Chapters 8 and 9 of Chris Reade's Elements of
   Functional Programming *)

   type 'a bintree = Lf of 'a
   | Nd of 'a bintree * 'a bintree

let rec equal_list l1 l2 = match l1, l2 with
| [], [] -> true
| (x::xs), (y::ys) when x = y -> equal_list xs ys 
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