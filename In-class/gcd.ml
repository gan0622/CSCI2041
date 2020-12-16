let gcd(x: int)(y: int) : int = 
    let minimum x y = if x > y then y else x
    in 
    let rec helper i =
        if x mod i = 0 && y mod i = 0
        then i
        else helper(i-1)
    in
    helper(minimum x y)


let is_square n = 
    let is_no m n = if m = n then true else false
    in
    let rec helper m =
        if m < n
        then helper(m+1)
        else m * m
    in
    helper(is_no 0 n)