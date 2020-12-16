
let all_odds =
	let odd n = n mod 2 = 1
in List.filter odd

let decrement_all =
	let minus n = n - 1
in List.map minus

let min_fold (ls:int list):int = match ls with
	|[] -> raise(Failure"input cannot be empty")
	|x :: xs -> let min a b = if a < b then a else b in List.fold_left min x xs 
	
let sum_prod (lst:int list):int*int = match lst with
	|[] -> (0,1)
	|x -> (List.fold_left(fun accum x -> accum + x ) 0 lst, List.fold_left(fun accum x -> accum * x) 1 lst)
	
(*'a list -> 'b -> 'b*)
let partition_right f lst = 
	let f element (pass,fail) = if f element
				    then (element :: pass,fail)
				    else (pass, element :: fail)
	in
	List.fold_right f lst ([],[])
	
(*'a -> 'b list -> 'a*)
let rev ls = List.fold_left(fun accum x -> x ::accum)[] ls
let partition_left f lst =
	let f (pass,fail) element = if f element
				    then(element :: pass, fail)
				    else(pass, element :: fail)
	in
	List.fold_left f ([],[]) (rev lst)

(*List.fold_left is tail recursive, so if i wanted the right output i needed to reverse the whole
	list at the end*)
let map_as_fold f lst = List.fold_left(fun accum x -> ([f x])@accum)[] lst
