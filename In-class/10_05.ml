type color = Red | Green | Blue

let my_fav = Blue

type weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

let isWorkDay (d:weekday) : bool = match d with
    | Monday | Tuesday | Wednesday
      | Thursday | Friday -> true
    | _ -> false

             
type value = IntVal of int
           | BoolVal of bool

let rec sumList_v1 l = match l with
  | (hd :: tl) ->
     (match hd with
     | IntVal i -> i + sumList_v1 tl
     | BoolVal _ -> sumList_v1 tl
     )
  | [] -> 0

let rec sumList lst = match lst with 
    | [] -> 0
    | (IntVal i) :: tl -> i + sumList tl
    | (BoolVal _) :: tl -> sumList tl


type coord = float * float

type circ_desc = coord * float
type tri_desc = coord * coord * coord
type sqr_desc = coord * coord * coord * coord

type shape = Circle of circ_desc
           | Triangle of coord * coord * coord
           | Rect of sqr_desc

let isRect s = match s with
    | Rect _ -> true
    | _ -> false

let area s = match s with
    | Circle (_, r) -> r *. r *. 3.14
    | _ -> failwith "Finish this"

let head lst = match lst with
    | [] -> None
    | x::xs -> Some x

let rec minList lst = match lst with
    | [] -> None
    | x::[] -> Some x
    | x::xs -> match minList xs with
               | None -> failwith "Oops"
               | Some m ->
                  if x < m then Some x
                  else Some m

type nat = Zero
         | Succ of nat
                 
type 'a myList = Nil
               | Cons of ('a * ('a myList))

let rec sumList (lst : int myList) : int =
  match lst with
  | Nil -> 0
  | Cons (hd, tl) -> hd + sumList tl

let rec myMap (f: 'a -> 'b) (lst: 'a myList) : 'b myList =
  match lst with
    | Nil -> Nil
    | Cons (hd, tl) -> Cons (f hd, myMap f tl)

type 'a btree = Empty
              | Node of 'a * 'a btree * 'a btree

let t = Node (5,
              Node (4,
                    Node (1, Empty, Empty),
                    Empty),
              Node (2, Empty, Empty)
          )

let rec sumTree (t: int btree) : int =
  match t with
  | Empty -> 0
  | Node (v, t1, t2) -> v + sumTree t1 + sumTree t2

let rec mapTree (f: 'a -> 'b) (t: 'a btree) : 'b btree =
  match t with
  | Empty -> Empty
  | Node (v, t1, t2) -> Node (f v, mapTree f t1, mapTree f t2)
      
let rec reduce (b: 'b) (f: 'a -> 'b -> 'b -> 'b)
          (t: 'a btree) : 'b =
    match t with
    | Empty -> b
    | Node (v, t1, t2) -> f v (reduce b f t1) (reduce b f t2)

let sumr (t: int btree) =
  let add3 x y z = x + y + z
  in reduce 0 add3 t

let mapTreeR (f: 'a -> 'b) (t: 'a btree) : 'b btree =
  let g (v: 'a) (t1: 'b btree) (t2: 'b btree) : 'b btree =
    Node (f v, t1, t2)
  in
  reduce Empty g t

let rec is_elem (e: 'a) (t: 'a btree) : bool =
  match t with
  | Empty -> false
  | Node (v, t1, t2) -> e = v || is_elem e t1 || is_elem e t2
        
let is_elemR (e: 'a) (t: 'a btree) : bool =
  let p (v :'a) (b1: bool) (b2: bool) =
    e = v || b1 || b2
  in
  reduce false p t
  
type nat = Zero | Succ of nat

let rec toInt = function
    |Zero ->0
    |Succ n -> toInt n + 1
    
let rec add n1 n2 = match n1,n2 with
	|Zero, n -> n
	|Succ m' , m -> Succ (add m' m)

let rec sumTo(n:int):int = match n with
	|0 -> 0
	|n -> n + sumTo(n-1)
	
let rec sumTo' (n:nat) :nat = match n with
	|Zero -> Zero
	|Succ n' -> add n (sumTo' n')
	 

let rec to_list(t:'a btree) : 'a list = match t with
	|Empty -> []
	|Node(v,t1,t2) -> to_list t1 @ [v] @ to_list t2

