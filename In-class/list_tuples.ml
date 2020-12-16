let rec all xs =
match xs with
| []->true
| x :: res -> x && all res