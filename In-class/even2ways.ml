let rec even2ways(xs : int list) : bool =
match xs with
| [] -> true
| x :: [] -> false
| x :: (y :: res) -> x mod 2 = 0 && y mod 2 = 0 && even2ways res