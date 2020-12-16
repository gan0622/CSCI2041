type rat = int * int

type op = Add | Sub | Mul | Div

type expr
  = Rat of rat
  | BinOp of expr * op * expr 


type evalError = DivByZero | FacOfNegativeNum | FacOfNonWholeNum

exception EvalError of evalError

let a  = (BinOp (Rat (8, 1), Div,
   BinOp (Rat (3, 1), Sub, BinOp (Rat (8, 1), Div, Rat (3, 1)))))
let show_evalError : evalError -> string = function
  | DivByZero -> "Division by zero"
  | FacOfNegativeNum -> "Factorial of negative number"
  | FacOfNonWholeNum -> "Factorial of non-whole number"

let rec show (e: expr) : string =
  let show_op = function
    | Add -> " + "
    | Sub -> " - "
    | Mul -> " * "
    | Div -> " / "
  in
  match e with
    | BinOp (e1, op, e2) -> "(" ^ show e1 ^ show_op op ^ show e2 ^ ")"
    | Rat (n,d) -> 
       if d = 1 then string_of_int n
       else "(" ^ string_of_int n ^ "/" ^ string_of_int d ^ ")"

let gcd a' b' =
  let a = if a' < 0 then -a' else a' in
  let b = if b' < 0 then -b' else b' in
  let rec gcd' a b =
    if a = 1 || b = 1 then 1
    else
    if a = b
    then a
    else if a < b
    then gcd' a (b-a)
    else gcd' (a-b) b
  in gcd' a b
   
let rat_simplify (n,d) = 
  if n = 0 then (0,1) else
  let gcd_of_n_d = gcd n d 
  in  (n / gcd_of_n_d, d / gcd_of_n_d)

exception FoundExpr of expr

let rat_add (n1,d1) (n2,d2) = (n1 * d2 + n2 * d1, d1 * d2)
let rat_mul (n1,d1) (n2,d2) = (n1 * n2, d1 * d2)
let rat_sub (n1,d1) (n2,d2) = (n1 * d2 - n2 * d1, d1 * d2)
let rat_div (n1,d1) (n2,d2) = if d1 = 0 || n2 = 0 then raise (EvalError DivByZero)
                              else (n1 * d2, d1 * n2)


let rec eval (e : expr) : int * int = match e with
    | BinOp(ce, Add, x) -> rat_add (eval ce) (eval x) 
    | BinOp(ce, Mul, x) -> rat_mul (eval ce) (eval x)  
    | BinOp(ce, Sub, x) -> rat_sub (eval ce) (eval x) 
    | BinOp(ce, Div, x) -> rat_div (eval ce) (eval x) 
    | Rat (ce, x) -> (ce, x) 

let find_expr(lst : rat list) : expr option =
    let rec build_expr (ce:expr) (rest: rat list) : unit =
    print_endline ("Trying " ^ show ce);
    let ce_val : rat option = 
      try Some (rat_simplify (eval ce)) with
      | EvalError er -> print_endline (show_evalError er); None
    in
    match ce_val with
    | None -> ()
    | Some v ->
       if v = (24 ,1) && rest = []
       then
         raise (FoundExpr ce)
       else
         match rest with
         | [] -> ()
         | x :: xs -> let exprlist = [(fun e -> BinOp(e, Add, ce)); (fun e -> BinOp(e, Mul, ce)); (fun e -> BinOp(e, Sub, ce)); (fun e -> BinOp(e, Div, ce))]
                      in
                      List.iter (fun (f : expr -> expr) -> build_expr (f (Rat x)) xs) exprlist
         (* let exprlist = [BinOp(ce, Add, x); BinOp(ce, Mul, x); Bin....]) in
         for each element in exprlist, build_expr elem 
         List.iter (fun) exprlist  *)
     in
     match (List.rev lst) with
     |(h :: tl) -> (try build_expr (Rat h) tl; None with
                   | FoundExpr ce -> Some ce
                  )
     |[] -> raise (Failure "emptylist")

(* let a = match (find_expr [(4,1); (2,1); (6,1); (3,1)]) with
| None -> false
| Some e -> e = BinOp (Rat (8, 1), Div, 
                      BinOp (Rat (3, 1), Sub, 
                             BinOp (Rat (8, 1), Div, Rat (3, 1))))
           ||
           e = BinOp (Rat (4, 1), Mul, 
                      BinOp (Rat (2, 1), Mul,
                             BinOp (Rat (6, 1), Sub, Rat (3, 1))))
 *)



let b = find_expr [(4,1); (2,1); (6,1); (3,1)] 

let c = b = Some
   (BinOp (Rat (4, 1), Add,
     BinOp (Rat (2, 1), Add, BinOp (Rat (6, 1), Mul, Rat (3, 1)))))