
```ocaml
let even x = x mod 2 = 0
           
let rec all_evens (lst: int list) : int list =
  match lst with
  | [] -> []
  | x::xs when even x -> x :: (all_evens xs)
  | _::xs -> all_evens xs

let rec filter (f: 'a -> bool) (lst: 'a list) : 'a list =
  match lst with
  | [] -> []
  | x::xs when f x -> x :: filter f xs
  | _::xs -> filter f xs

let all_evens_f lst = filter even lst
```

## Proof that:

```ocaml
all_evens lst = all_evens_f lst
```

## Base Case: 

```ocaml
lst = []
```

```
all_evens [] = all_evens_f []
#by def of all_evens and all_evens_f
[] = filter even []
#by def of filter
[] = []
```


## Inductive case: 
```ocaml
all_evens x::xs = all_evens_f x :: xs
```
## Inductive hypothesis:
```ocaml 
all_evens xs  = all_evens_f xs
```
## proof by sub cases: 

```
x is even, x is not even
```


## case 1

```
   all_evens_f x :: xs
#by def of all_evens_f
=  filter even xs
#by the case and def of filter 
=  x :: filter even xs
#by def all_evens_f
=  all_evens_f (x :: xs)


   all_evens x :: xs
#by def of all_eves
= x :: all_evens xs
#by simpiflication 
= all_evens (x :: xs)
all_evens xs  = all_evens_f xs for obvious reason
```


## case 2

```
    all_evens_f x :: xs
#by def of all_evens_f
=  filter even xs 
#by the case and def of filter
=  x :: fliter even xs
#by def all_evens_f
= all_evens_f (x :: xs)

    all_evens x :: xs
#by def of all_eves
= all_even xs 
#by simpiflication 


all_evens []  = all_evens_f [] for obvious reason
```

```
P(lst) holds if P([]) holds if P(lst') => P(x :: lst') holds
```

