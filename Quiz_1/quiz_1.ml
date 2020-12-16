(*Jianyuan Gan*)
(*I understand that the work to be done on Quiz 1 is to be done on my
own, without any help from my classmates or others.  Discussing the
questions or solutions is an act of academic misconduct and I
understand the penalties as laid out in the syllabus for this.*)


let even x = x mod 2 = 0

(*First option is using List.fold to do it. Second option is using recursion. The reason why I chose
List.fold to do it was because I did not need to write so many lines and the reason why I chose
List.fold_left was because I am more comfortable by using List.fold_left than right.*)

let count_evens lst = List.fold_left(fun acc x -> if x mod 2 = 0 then acc+1 else acc)0 lst

let count f lst = List.fold_left(fun acc x -> if (f x = true) then acc + 1 else acc)0 lst
              
let count_evens_2 lst = count (even) lst



let is_square n =
    let rec helper i =
        if i >= 0 && i * i = n
            then true
        else if i < 0
            then false
        else
            helper(i-1)
    in 
        helper n        

    
let count_squares lst = count (is_square) lst



let subtract_lst lst = match lst with
    |[] -> 0
    |hd :: tail -> List.fold_right(fun x acc -> acc - x)tail hd

(*Since List.fold_right is not a tail recursive, and the questions wanted us to subtract from 
left to right, so chosing List.fold_right was the best approach for this question.*)



let suml lst =
    let rec sum lst acc = match lst with
        |[] -> acc
        |hd :: tail -> sum tail (acc + hd)
        in
        sum lst 0

(* suml (1::2::4::[])
=  sum (1::2::4::[]) 0
=  sum (2::4::[]) 1
=  sum (4 :: []) 3
=  sum ([])     7
=  7
*)