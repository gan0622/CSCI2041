let rec power n y =
    if n = 0 then 1.0 else y *. power(n-1)y
    let cube = power 3