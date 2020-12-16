(*one line*)
(*yes*)
(*it doesn't have to*)
let circle_circum_v1 x = 2.0 *. 3.1415 *. x;;



(*two lines*)
(*yes *)
(*one tap*)
(*located is below the let*)

let circle_circum_v2 x =
    let r = 3.1415
    in
    2.0 *. x*. r;;



(*there no any improvenments, two clauses are mandatory
no changes necessary*)
let rec product ls =
match ls with
|[] -> 1
|x :: rest -> x * product rest;;

(*we didn't use it yet we dont know is a good idea or not
*)
(*replace the [] and [x] to
| _ -> raise (Failure "sum_sqrdiffs input list needs at least two elements")
*)
let sum x y = (x - y) * (x - y)
let rec sum_sqrdiffs ls = match ls with
|[] -> 0
|[x] -> 0
|x :: (y :: rest) -> sum x y + sum_sqrdiffs (y::rest);;


(* yes. I used triangle_perimeter use distance as a helper function*)
(* I prefer the second one*)
let sum x y = (x -. y) *. (x -. y)
let rec sum_sqrdiffss (ls: float list): float = match ls with
|[] -> 0.0
|[x] -> 0.0
|x :: (y :: rest) -> sum x y +. sum_sqrdiffss (y::rest);;

let distance(a,b)(c,d) = Float.sqrt((sum_sqrdiffss([c;a]) +. sum_sqrdiffss([d;b])));;

(*let triangle_perimeter(a,b)(c,d)(e,f) = 
distance(a,b)(c,d) +. distance(a,b)(e,f) +. distance(c,d)(e,f)*)

(*change your tuple into an element, improvenment here is making you type
less*)
let triangle_perimeter v1 v2 v3 =
  (distance v1 v2) +. (distance v2 v3) +. (distance v3 v1);;