let rec euclid m n =
  if m = n then m
  else 
    if m < n 
    then euclid m (n-m) 
    else euclid (m-n) n

let rec euclid' m n = match m, n with 
  | m, n when m = n -> m
  | m, n when m < n -> euclid' m (n-m) 
  | m, n when m > n -> euclid' (m-n) n 
  | _, _ -> failwith "Can't actually get here."


(*
lower * lower <= n <= upper * upper
->
let (lower’,upper’) = sqrt_step n lower upper in 
    lower’ * lower’ <= n <= ’upper * upper’
 *)
 let sqrt n =
  let accuracy = 0.0001 in
  let rec sqrt_step lower upper =
    if (upper -. lower) < accuracy
    then (lower, upper)
    else let guess = (lower +. upper) /. 2.0 in
         (if (guess *. guess) > n
          then sqrt_step lower guess
          else sqrt_step guess upper
         )
  in
  if n > 0.0 
  then sqrt_step 1.0 n
  else failwith "Error: positive input required."


(*what is loop invariant



*)

(*let rec euclid' m n = match m, n with 
  | m, n when m = n -> m
  | m, n when m < n -> euclid' m (n-m) 
  | m, n when m > n -> euclid' (m-n) n 
  | _, _ -> failwith "Can't actually get here."
Show: euclid m n = gcd m n

Induction on structure of the computation

Base case: m = n

  euclid m n 
= m
  by def of euclid
= gcd m n
  by specification of gcd
Inductive cases: m < n Ind. Hyp. : euclid m (n - m) = gcd m (n - m)

  euclid m n 
= euclid m (n - m)
  by def of euclid
= gcd m (n - m)
  by ind. hyp.
= gcd m n
  by specifications of gcd
Inductive cases: m > n ... very similar ...*)


let rec reduce_list (n: 'b)(c:'a -> 'b -> 'b)(lst : 'a list) : 'b = match lst with
    |[] -> n
    |x :: xs -> c x (reduce_list n c xs)

let () = assert(reduce_list 0 (+) [1;2;3;4] = 10) 

type 'a myList 
    = Nil
    | Cons of 'a * 'a myList

let rec reduce_list (n: 'b)(c:'a -> 'b -> 'b)(lst : 'a myList) : 'b = match lst with
    |Nil -> n
    |Cons(x,xs) -> c x (reduce_list n c xs)


let () = assert(reduce_list 0 (+) (Cons (1,Cons(2, Cons(3,Cons(4,Nil))))) = 10); 

type nat
    = Zero
    | Succ of nat

let rec reduce_nat (z: 'b)(s: 'b-> 'b)(n: nat) : 'b = match n with
    |Zero -> z
    |Succ n' -> s (reduce_nat z s n')

let to_int(n: nat) : int = reduce_nat 0 (fun i-> i + 1) n

let () = assert(to_int(Succ(Succ(Succ Zero))) = 3 )


type 'a btree
    = Empty
    |Node of 'a * 'a btree * 'a btree

let rec reduce_btree(e: 'b)(n: 'a -> 'b -> 'b -> 'b)(t:'a btree) : 'b = 
    match t with
    |Empty -> e
    |Node(v,t1,t2) -> n v (reduce_btree e n t1)(reduce_btree e n t2)

let t = Node(5,Node(4,Node(1,Empty,Empty),Empty),Empty)
let () = assert(reduce_btree 0 (fun v b1 b2 -> v + b1 + b2) t = 10)