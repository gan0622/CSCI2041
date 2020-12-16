let circle_area r = 
    let pi = 3.14159
    in r *. r*. pi

let rec power (n: int)(m: float) : float = if n = 0 then 1.0 else m *. power(n-1) m

let cube = power 3

let rec all lst =
    match lst with
    |[] -> false
    |x :: xs -> x && all xs

let rec even2ways(lst : int list) : bool = 
    match lst with
    | [] -> true
    | x :: xs -> if x mod 2 = 1 then false else match xs with
        | [] -> false
        | y :: more -> if y mod 2 = 0 then even2ways more else false

let rec is_empty lst = match lst with
    |[] ->true
    |_ -> false

let rec head lst = match lst with
    |[] -> 0
    |x :: xs -> x

let rec drop_value to_drop lst = match lst with
    |[] -> []
    |hd :: tl when hd = to_drop -> drop_value to_drop tl
    |hd :: tl -> hd :: drop_value to_drop tl

(*let first_of_3*)

let add_fraction(f1:int*int)(f2:int*int) : int*int =
    let (f1n,f1m) = f1 in
    let (f2n,f2m) = f2 in
    (f1n*f2m + f1m*f2n, f1m*f2m)

let m = [("dog",1);("chicken",2);("dog",3);("cat",5)]
let rec lookup_all key m = match m with
    |[] -> []
    |(name,value) :: more ->
            let res = lookup_all key more
            in if key = name then value :: res else res

let rec fib x = match x with
    |0 -> 0
    |1 -> 1
    |hd -> fib(hd - 2) + fib(hd - 1)

