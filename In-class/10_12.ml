(* Oct 12 - techniques *)

(* Priming the pump *)

let gcd (x: int) (y: int) : int =
  let rec helper i =
    if x mod i = 0  && y mod i = 0
    then i
    else helper (i - 1)
  in helper (min x y)

(* this was an in-class exercise, but no need to turn it in. *)
let is_square (n: int) : bool =
  let rec check i = if i * i = n
                    then true
                    else if  i > (n / 2)
                    then false
                    else check (i + 1)
  in
  n >= 0 && check 0

let max lst = match lst with
    | [] -> failwith "Oh no - empty list"
    | x::xs -> List.fold_left (fun m x' -> if m > x' then m else x') x xs

let max' (lst: int list) : int option  = match lst with
    | [] -> None
    | x::xs -> Some
                 (List.fold_left (fun m x' -> if m > x' then m else x')
                    x xs)

let max'' lst = match max' lst with
    | None -> failwith "Oh no - empty list"
    | Some v -> v


(* Skimming the cream *)
let group_by_3 lst =
  let accum : 'a list * 'a list list = ([], [])
  in
  let f (elems, groups) elem = 
    if List.length elems = 2
    then ([], ( List.rev (elem::elems) :: groups))
    else ( elem::elems, groups )
  in
  match List.fold_left f accum lst with
  | (left_overs, lists_of_3) ->
     List.rev (left_overs :: lists_of_3)

(* group_by_3 [1;2;3;4;5;6;7;8;9;10]
  ->  [ [1;2;3]; [4;5;6]; [7;8;9]; [10] ] 
Trace of the function we've written:
  group_by_3 [1;2;3;4]
= List.fold_left f ([], []) [1;2;3;4]
= List.fold_left f ([1], []) [2;3;4]
= List.fold_left f ([2;1], []) [3;4]
       = List.fold_left f ([], [ [3;2;1] ]) [4]  -- before adding reverse
= List.fold_left f ([], [ [1;2;3] ]) [4]
= List.fold_left f ([4], [ [1;2;3] ])  []
= ([4], [1;2;3])
What do we want to happen?
group_by_3 [1;2;3;4;5;6;7;8;9;10]
  ->  [ [1;2;3]; [4;5;6]; [7;8;9]; [10] ] 
([], []) [1;2;3;4]
([1], []) [2;3;4]
([1,2], []) [3;4]
([1,2,3], []) [4]
([], [ [1;2;3] ])  [4]
([4], [ [1;2;3] ]) []
*)



let rec foldl (f: 'b -> 'a -> 'b) (accum: 'b) (lst: 'a list) : 'b =
  match lst with
  | [] -> accum
  | x::xs -> foldl f (f accum x) xs

let rec rev lst =
  match lst with
  | [] -> []
  | x::xs -> rev xs @ [x]  

(* rev [1;2;3;4]
   rev [2;3;4] @ [1]
   (rev [3;4] @ [2]) @ [1]
   ((rev [4] @ [3]) @ [2]) @ [1])
   (((rev [] @ [4]) @ [3]) @ [2]) @ [1])
   ((([] @ [4]) @ [3]) @ [2]) @ [1])
   ((([4]) @ [3]) @ [2]) @ [1])
   ((([4;3]) @ [2]) @ [1])
   ((([4;3;2]) @ [1])
   ((([4;3;2;1])
   
write append : 'a list -> 'a list -> 'a list
write rev' - and use an accumulator
List.fold_left to write rev' 
*)

let rec rev lst = match lst with
	|[] -> []
	|x :: xs -> rev xs @ [x]

rev(l1 @ l2) = rev l2 @ l1

(*
Base case: rev([]@l2) =  rev l2 @ rev []
	rev([]@l2)
	=rev l2
	by properties of lists and @
	=rev l2 @ []
	by properties of list
	=rev l2 @ rev []
	by def of rev
	
Inductive case: rev((x :: xs) @ l2) = rev l2 @ rev (x ::xs)
Ind hyp :rev(xs @ l2) = rev l2 @ rev xs) *)

	(* reverse using an accumulator *)

let rec rev lst = match lst with
  | [] -> []
  | x::xs -> rev xs @ [x]


let rev_accum lst =
  let rec ra accum ys =
    match ys with
    | [] -> accum
    | x::xs -> ra (x::accum) xs
  in
  ra [] lst
let rev_fold lst =
   List.fold_left (fun accum x -> x::accum) [] lst
(*
  []
  4::3::2::1::[]
ra [] 3::2::1::[]
ra 3::[] 2::1::[]
ra 2::3::[] 1::[]
ra 1::2::3::[] []
1::2::3::[]
*)



let rec place e l = match l with 
  | [ ] -> [e]
  | x::xs when e < x -> e::x::xs
  | x::xs -> x :: (place e xs)

let rec is_elem e l = match l with
  | [ ] -> false
  | x::xs -> e = x || (e > x && is_elem e xs)
(* is_elem e (place e l)

Base case:  is_elem e (place e [])
       is_elem e (place e [])
    =  is_elem e [e]
    by def place
    = e = e  || e > e && is_elem e []
    by def is_elem
    = true
    for obvious reasons

inductive case :  is_elem e (place e (y::ys))
Ind hyp : is_elem e (place e (y::ys))
        proof by sub cases: e = y, e < y, e > y

          is_elem e (place e (y::ys))
        = is_elem e (y :: place e ys)
        by def. of place, using our assumption e = y
        = e = y || (e > y && is_elem e ys)
        by def. of is_elem
        = true || (e > y && is_elem e ys)
        = true

    case: e < y
          is_elem e (place e (y::ys))
        = is_elem e (e ::  y :: ys)
        by def of place
        = e = e || (e > y && is_elem e ys)
        by def of is_elem
        = true
        by assumption and simplification 

    case: e > y
          is_elem e (place e (y::ys))
        = is_elem e (y :: place e ys)
        by def of place
        = e = y || (e > y && is_elem e (place e ys))
        by def of is_elem
        = is_elem e (place e ys)
        by simplification and assumption of e > y
        = true
        by inductive hypothesis
Ind hyp : is_elem e (place e (y::ys))

*)



let rec sorted l = match l with
  | [ ] -> true
  | x::[] -> true
  | x1::x2::xs -> x1 <= x2 && sorted (x2::xs)

(* base case : sorted[] => sorted (place e [])
    Ind case : sorted(y :: ys) => sorted (place e (y :: ys)
    Ind hyp: sorted ys => sorted (place e (y :: ys)
    Assume: sorted (y :: ys)
    Show : sorted(place e (y :: ys))

    case: e < y
      sorted(place e (y :: ys))
    = sorted (e :: y :: ys)
      by def of place
    = e <= y && sorted (y :: ys)
      by def of sorted
    = true  && true
      by case assumption and sorted assumption y :: ys

    case: e >= y and ys = []
        sorted(place e (y::ys))
      = sorted(y :: place e ys)
        by def of place
      = sorted (y :: [e])
        by def of place
      = y <= && sorted(e :: [])
        by def of sorted
      = true && true
        by assumption and def of sorted


     case: e >= y and ys = z :: zs and e < z

        sorted(place e (y::z::zs))
      = sorted(y :: place e (z :: zs))
        by def of place
      = sorted(y:: e :: z :: zs)
        by def of place
      = y <= e && sorted (e :: z ::zs)
        by def of sorted
      = y <= e && e <= z && sorted(z :: zs)
        by def of sorted
      = sorted(z :: zs)
        by assumption using 
      = true
      since ys are sorted and ys = z :: zs 
      true bc sorted(y :: ys) implies sorted ys



      case: e >= y and ys = z :: zs and e >= z

        sorted(place e (y::z::zs))
      = sorted(y :: place e (z :: zs))
        by def of place
      = sorted(y:: z :: z :: place e zs)
        by def of place
      = y <= z && sorted (z :: place e zs)
        by def of sorted
        true && c
        by def of sorted and our assumption
      = sorted (z :: place e zs)
        by simplification 
      = sorted (place e (z :: zs))
        by def place
      = sorted (place e ys)
        by ys = z :: zs
      = true  
      by inductive hypo
    *)