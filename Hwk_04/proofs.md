#Homework 4:Reasoning about correctness

by Jianyuan Gan


(*----------------------------Question 1------------------------------------------*)

```ocaml
let rec prod = function
	|[] -> 1
	|y :: ys -> y * prod ys
```

## Prove that:

```ocaml
prod(l1 @ l2) = prod l1 * prod l2

```

## Base case: 

```ocaml 
l1 = []
```

```
prod([] @ l2) = prod(l2)
#by properties of lists and @
= 1 * prod(l2)
#by def of prod
= prod[] * prod(l2)
```


## Inductive case: 

```ocaml
prod((y::ys)@l2) = prod(y::ys) * prod(l2)
```

## Inductive hypothesis: 

```ocaml
prod(ys @ l2) = prod ys * prod l2
```

```
prod((y::ys)@l2) = prod(y::ys) * prod(l2)
#by properties of lists and @
= y * prod ys * prod l2 = prod(y::ys) * prod(l2)
#by def of prod
= y * prod ys * prod l2 = y * prod ys  * prod(l2)
#by def left == right
```

```
P(lst) holds if P([]) holds and if P(lst') => P(y :: lst') holds 
```




(*----------------------------Question 2------------------------------------------*)

```ocaml
let rec sum  = function
	|[] -> 0
	|y :: ys -> y + sum ys
	
let rec length = function
	|[] -> 0
	|y :: ys -> 1 + length ys
	
let rec inc_all = function
	|[] -> []
	|y :: ys -> (y+1) :: inc_all ys
```

## Prove that:

```ocaml 
sum (inc_all l) = length l + sum l
```

## Base case:

```ocaml 
l = []
```

```
sum (inc_all []) = length [] + sum []
#by def of sum,length and inc_all
sum([]) = 0 + 0
#by def of sum
0 = 0 + 0
#by def of proof
sum (inc_all []) = length [] + sum []
```

## Inductive case: 
```ocaml
sum(inc_all y ::ys) = length y :: ys + sum y ::ys
```
## Inductive hypothesis: 
```ocaml
sum (inc_all ys) = length ys + sum ys
```

```
sum(inc_all y ::ys) = length y :: ys + sum y ::ys
sum((y+1) :: inc_all ys) = 1 + length ys + y + sum ys
#by def of sum,length and inc_all
(y+1) + sum(inc_all ys) = 1+y + length ys + sum ys
#by def of sum
sum(inc_all ys) = length ys + sum ys
#by simpification
```

```
P(lst) holds if P([]) holds and if P(lst') => P(y :: lst') holds 
```




(*----------------------------Question 3------------------------------------------*)

```ocaml
let rec map f l = function
	|[] -> []
	|y :: ys -> f y :: map f ys
	
let inc x = x + 1

let rec inc_all = function
	|[] -> []
	|y :: ys -> (y+1) :: inc_all ys
```

## Prove that:

```ocaml
map inc l = inc_all l
```

## Base case: 

```ocaml
l = []
```

```
map inc [] = inc_all []
#by def map and inc_all
[] = []
#by def of proof
map inc[] = inc_all[]
```

## Inductive case: 
```ocaml
map inc y :: ys = inc_all y :: ys
```

## Inductive hypothesis: 
```ocaml
map inc ys = inc_all ys
```

```
map inc y :: ys = inc_all y :: ys
#by def map and inc_all
(y+1) :: map inc ys = (y+1) :: inc_all ys
#by def map and inc_all 
map inc ys = inc_all ys
#by simpification
```

```
P(lst) holds if P([]) holds and if P(lst') => P(y :: lst') holds
``` 



(*----------------------------Question 4------------------------------------------*)

```ocaml
type 'a tree = Empty
	     | Node of 'a * 'a tree * 'a tree
	     
let rec to_list(t:'a tree) : 'a list = match t with
	|Empty -> []
	|Node(v,t1,t2) -> to_list t1 @ [v] @ to_list t2
	
let rec product(t:int tree):int = 
    match t with
    |Empty -> 1
    |Node(v,t1,t2) -> v * product t1 * product t2	
 
let rec prod = function
	|[] -> 1
	|y :: ys -> y * prod ys

```

## Prove that:

```ocaml
prod (to_list t) = product t
```

## Base case:

```ocaml 
t = Empty
```

```
prod(to_list Empty) = product Empty
#by def of to_list and product
prod([]) = 1
#by def of prod
1 = 1
which means that
prod(to_list t) = product t
```

## Inductive case: 

```ocaml
t = Node(v, t1, t2) 
prod(to_list (Node(v, t1, t2)) = product Node(v, t1, t2)
```

## Inductive hypothesis: 

```ocaml
prod(to_list t1) = product t1
prod(to_list t2) = product t2
```

```
prod(to_list (Node(v, t1, t2)) = product Node(v, t1, t2)
#by def to_list and product
prod(to_list t1 @ [v] @ to_list t2) = v * product t1 * product t2
#by properities of list and tree
v * prod t1 * prod t2 = v * product t1 * product t2
#by simplification
```

```
P(t) holds if P(Empty) holds and (if P(t1) and p(t2) => P(Node(t,t1,t2)) holds)
```

(*-----------------------------------Question 5-------------------------------------*)

```ocaml
let rec size(t:'a tree) : int = 
    match t with
    |Empty -> 0
    |Node(v,t1,t2) -> 1 + size t1 + size t2 

let size_r(t:'a tree):int = 
  let add3 v v1 v2 =  1 + v1 + v2 
  in
  reduce 0 add3 t 

let rec reduce (b: 'b) (f: 'a -> 'b -> 'b -> 'b)
       (t: 'a tree) : 'b =
        match t with
        | Empty -> b
        | Node (v, t1, t2) -> f v (reduce b f t1) (reduce b f t2)
```


## Prove that:

```ocaml
size t = size_r t
```

## Base case: 

```ocaml
t = Empty
size Empty = size_r Empty
```


```
#by def of size and reduce
0 = reduce 0 (fun _ v1 v2 -> 1 + v1 + v2) Empty 
#by def reduce
0 = 0
```

## Inductive case: 

```ocaml
size Node(v,t1,t2) = size_r Node(v,t1,t2)
```

## Inductive hypothesis: 

```ocaml
size t1 = size_r t1
size t2 = size_r t2
```

```
size Node(v,t1,t2) = 1 + size t1 + size t2 = 1 + size_r t1 + size_r t2
											#by def size_r
									       = 1 + reduce 0 (fun _ v1 v2 -> 1 + v1 + v2) t1 + reduce 0 (fun _ v1 v2 -> 1 + v1 + v2) t2
										   # by def reduce
										   = (fun _ v1 v2 -> 1 + v1 + v2) 0 (reduce 0 (fun _ v1 v2 -> 1 + v1 + v2) t1) (reduce 0 (fun _ v1 v2 -> 1 + v1 + v2) t2)
										   # by simipification 
										   = reduce 0 (fun _ v1 v2 -> 1 + v1 + v2) Node(v,t1,t2)

										   = size_r Node(v,t1,t2)
```

```
P(t) holds if P(Empty) holds and (if P(t1) and p(t2) => P(Node(t,t1,t2)) holds)
```
