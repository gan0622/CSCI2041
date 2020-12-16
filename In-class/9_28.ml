type weekday = Monday | Tuesday | Wednesday | Thrusday | Friday | Saturday | Sunday

type color = Red | Green | Black

let isWorkDay (d: weekday) : bool = match d with
	| Monday |Tuesday | Wednesday | Thrusday | Friday -> true
	| _ -> false

type coord = float * float

type circ_desc = coord * float
type tri_desc = coord * coord * coord
type sqr_desc = coord * coord * coord * coord

type shape = Circle of circ_desc
	   | Triangle of coord * coord * coord
	   | Rect of sqr_desc

let isRec(ls : shape) : bool = match ls with
	|Rect _ ->true
	|_ -> false
	
	
(*1.4 #4*)
(*In C the array type constructor int[]
  	   pointer type constructor int* 
  	   struct {int x; int y;}*)


let rec minList lst = match lst with
	|[] -> None
	|x :: [] -> Some x
	|x :: xs -> match minList xs with
		    |None -> failwith "Oops"
		    |Some m ->
		    	if x < m then Some x
		    	else Some m 
	            
	            
	            
	            
