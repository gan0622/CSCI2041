

(*what is the type of map?*)

(*map : ('a -> 'b) -> 'a list(input list) -> 'b list(output list)*)

(*what is the implementation of map*)

(*let rec map f lst = match lst with 
	|[] -> []
	|x :: xs -> f x :: map f xs*)
	

(*what is the type of filter*)
(*filter: ('a -> bool) -> 'a list -> 'a list*)

(*what is the implementation of filter*)

(*let rec filter f lst = match lst with
	|[] -> []
	|x :: xs -> if f x 
		    then x :: filter f xs
		    else filter f xs *)
		    
let smuch cs =
	let f c = if c <> ' ' && c <> '\t' && c <> '\n'
		  then true else false
	in filter f cs ;;
	
let removeABC ls = 
	let f c = c <> 'A' && c <> 'B' && c <>'C'
	|x :: xs  ;;

(*what is the type of fold*)
(*fold :( 'a -> 'a -> 'a ) -> 'a ->'a list -> 'a *)


(*what is the implementation of fold*)
let rec fold f base lst = match lst with
	|[] -> base
	|x :: xs -> f x (fold f base xs);;(*fold x into base till the tail*) 




