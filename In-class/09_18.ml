let m = [ ("dog", 1); ("chicken",2); ("dog",3);
          ("cat",5)]

(* Recall lookup_all from S1.1, and shown below *)
let rec lookup_all s m =
  match m with
  | [] -> []
  | (name,value)::ms -> 
     let rest = lookup_all s ms
     in if s = name  then value :: rest else rest

let rec find_all_by eq elem lst =
  match lst with
    | [] -> []
    | x::xs when eq x elem ->
       x::(find_all_by eq elem xs)
    | _::xs -> find_all_by eq elem xs


let rec find_all_with f l =
  match l with
  | [] -> []
  | x::xs -> 
     let rest = find_all_with f xs
     in if f x then x::rest else rest


let big_num n lst = find_all_with(fun elem -> elem > n) lst

let big_strs n lst = find_all_by(fun s n -> String.length s > n) n lst

let is_elem v lst = 
    match find_all_by (=) v lst with
    |[] -> false
    |_ ->true;;

let rec drop_while lst f = match lst with
    |[] -> []
    |x :: xs when f x -> drop_while xs f
    |_ ::_ -> lst

let compose (f: 'a -> 'b) (g: 'b -> 'c) (x:'a) : 'c =
  g (f x)

(*what is the type of map?*)

(*map : ('a -> 'b) -> 'a list(input list) -> 'b list(output list)*)

(*what is the implementation of map*)
let inc x = x+1
let rec map f lst = match lst with 
	|[] -> []
	|x :: xs -> f x :: map f xs

(*what is the type of filter*)
(*filter: ('a -> bool) -> 'a list -> 'a list*)

(*what is the implementation of filter*)

let even n = if n mod 2 = 0 then true else false
let rec filter f lst = match lst with
	|[] -> []
	|x :: xs -> if f x 
		    then x :: filter f xs
		    else filter f xs 

let removeABC lst=
	let f c = if c <> 'A' && c <> 'B' && c <> 'C'
		  then true else false
	in filter f lst ;;

(*what is the type of fold*)
(*fold :( 'a -> 'a -> 'a ) -> 'a ->'a list -> 'a *)


(*what is the implementation of fold*)
let rec fold f base lst = match lst with
    | [] -> base
    | x::xs -> f x (fold f base xs)

let rec foldr (f: 'a -> 'b -> 'b) (lst: 'a list) (base: 'b) : 'b = 
  match lst with
  | [] -> base
  | x::xs -> f x (foldr f xs base)

let rec foldl (f: 'b -> 'a -> 'b) (accum: 'b) (lst: 'a list) : 'b =
  match lst with
  | [] -> accum
  | x::xs -> foldl f (f accum x) xs

let partition f lst = 
    let g ele(pass,fail) = if f ele
                           then (ele :: pass,fail)
                           else (pass, ele :: fail)
    in
    foldr g lst ([],[])



let length lst = List.fold_right(fun ls x -> x + 1)lst 0
let rev ls = List.fold_left(fun accum x -> x ::accum)[] ls
let group_by_3 lst =
  let accum : 'a list * 'a list list = ([], [])
  in
  let f (elems, groups) elem= 
    if length elems = 2
    then ([], ((elem::elems) :: groups))
    else (elem::elems, groups)
  in
  foldl f accum (rev lst)



(*let find_all x lst = find_all_with (fun elem -> elem = x) lst;;

let find_all x lst = find_all_with ((=) x) lst;;

let big_nums n lst = find_all_with((fun elem -> elem > n)) lst;;

let big_nums n lst = find_all_with((<=) n) lst;;

let flip (f:a' -> 'b -> 'c) : (b' -> a' -> c')
 = fun b a -> f a b;;

let big_num' n lst = find_all_with((flip(>)) n) lst;;


let find_all_by' eq elem lst = find_all_with (fun x -> eq x elem) lst;;*)

let compose (f: 'a -> 'b) (g: 'b -> 'c) (x:'a) : 'c = g (f x);;

(*let gcd (x : int) (y : int) : int =
    let minimum = if x > y then y else x
    in
    let rec helper i =
        if x mod i = 0 && y mod i = 0
        then i
        else helper (i - 1)
    in
    helper minimum

let add_frac (f1:fraction) (f2:fraction) : fraction = 
    match f1, f2 with
    | (n1, d1), (n2, d2) -> let denom = d1 * d2
    in
    let numer = (n1(denom/d1) + n2(denom/d2)) 
    in
    let divisor = gcd numer denom
    in
    (numer/divisor,denom/divisor)
    *)





