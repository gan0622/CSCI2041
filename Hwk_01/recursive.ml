

let rec odd n = match n with
	|0-> false
	|1-> true
	|_-> odd (n mod 2)

let rec euclid (x:int)(y:int) :int = 
	if x = y then x
	else if x < y then euclid x (y-x)
	else euclid (x-y) y
	
(*8 / euclid 8 16 - 8 = 8 / 8 = 1*)
(*16 / euclid 16 - 8 = 16 / 8 = 2*)
let rec frac_simplify(x,y) = x / (euclid x y), y/(euclid x y)

let rec min_list (ls:int list):int = match ls with
	|[]-> raise(Failure "input list must not be empty")
	|[x] -> x
	|x :: tail -> if (min_list tail) < x then (min_list tail ) else x
	
let rec drop(x : int)(lst : 'a list):'a list = match lst with
	|[]->[]
	|xs :: tail -> if x <= 0 then lst else drop(x-1) tail
	
