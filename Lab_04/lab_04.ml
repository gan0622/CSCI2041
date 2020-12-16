
(**)
let length lst = List.fold_left(fun accum _ -> accum + 1) 0 lst

let andf lst = List.fold_left(fun boolean x -> boolean && x) true lst

let orf lst = List.fold_left(fun boolean x -> boolean || x) false lst

let is_elem ele lst = List.fold_left(fun accum x -> if x = ele then true else accum) false lst

(* [1,2,3,4]*)
let rev ls = List.fold_left(fun accum x -> x ::accum)[] ls

let ascii_sum lst = List.fold_left(fun accum x -> Char.code(x) + accum) 0 lst

let lebowski (lst:char list) : char list = List.fold_right(fun x accum -> if x = '.' then ','::' ':: 'd'::'u':: 'd':: 'e':: '.'::accum else x::accum)lst []

let lebowski_left (lst:char list) :char list = List.fold_left(fun accum x -> if x = '.' then ','::' '::'d'::'u'::'d'::'e'""'.'::accum else x :: accum)[] rev(lst)