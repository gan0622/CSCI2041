(*Jianyuan Gan*)
(*I understand that the work to be done on Quiz 2 is to be done on my
own, without any help from my classmates or others.  Discussing the
questions or solutions is an act of academic misconduct and I
understand the penalties as laid out in the syllabus for this.*)


type 'a tree = Leaf of 'a
             | Fork of 'a tree * 'a tree

let t : int tree = Fork(Leaf 1, Fork(Fork(Leaf 3, Fork(Leaf 4, Leaf 8)), Leaf 6))

let rec sum (t: int tree) = 
    match t with
    |Leaf n -> n 
    |Fork(t1, t2) -> sum t1 + sum t2

let rec prod(t: int tree) = 
    match t with
    |Leaf n -> n
    |Fork(t1, t2) -> prod t1 * prod t2

let rec reduce(fc:'a -> 'b)(f: 'b -> 'b -> 'b)(t:'a tree) : 'b = 
    match t with
    |Leaf n -> fc n
    |Fork(t1, t2) -> f (reduce fc f t1)(reduce fc f t2) 

let sumr(t: int tree) : int = 
    let add t1 t2 = t1 + t2 
    in
    reduce (fun v -> v) add t

let prodr(t: int tree) : int = 
    let product t1 t2 = t1 * t2 
    in
    reduce (fun v -> v) product t

let string_of_int_tree(t: int tree) : string = 
    let concat t1 t2 =  t1 ^ t2
    in
    reduce (fun v -> string_of_int v) concat t

