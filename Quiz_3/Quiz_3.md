# Quiz 3, CSci 2041

- **Due at 5:00pm on Wednesday, November 18.**

- **There are no late-day extensions allowed for this quiz.**

- **In case of problems with Git, you may email your work to the TAs
  before the due date.**

- Add your answers to this file by replacing the line 
   ```
   .... remove this line and write your answer here ...
   ```
   and replacing it with the lines containing your answer.

   Please leave the "```" marks that are there now.


# Trees and functions

Consider the following OCaml declarations:
```ocaml
type 'a tree = One of 'a
             | Node of 'a tree * 'a tree

let rec prod (t: int tree) : int = match t with
  | One v -> v
  | Node (t1, t2) -> prod t1 * prod t2

let rec reduce (l: 'a -> 'b) (f: 'b -> 'b -> 'b) (t: 'a tree) : 'b =
  match t with
  | One v -> l v
  | Node (t1, t2) -> f (reduce l f t1) (reduce l f t2)

let prodr t = reduce (fun a -> a) ( * ) t
```


# A correctness property

Consider the following correctness property over ``t``, an ``int
tree``, that relates the functions
and types given above:
```
  prod t = prodr t
```

# A proof

Write your answer to the following 6 parts of a proof of the above
property in the 6 spaces indicated below.


### 1. Principle of induction

What is the principle of induction to be used in this proof?
```
P(t) holds if P(One v) holds and (if P(t1) and p(t2) => P(Node(t1,t2)) holds)
```


### 2. Base case

What is the base case that needs to be proved?
```
t = One v
prod (One v) = prodr (One v)
```


### 3. Inductive case

What is the inductive case that needs to be proved?
```
prod Node(t1,t2) = prodr Node(t1,t2)
```


### 4. Inductive hypothesis

What is the inductive hypothesis to be used in the proof?
```
prod t1 = prodr t1
prod t2 = prodr t2
```

### 5. Proof of the base case

Write your proof of the base case below.
```
#by def of prod and reduce
prod (One v) = reduce (fun a -> a) f (One v)
#by def of reduce
v = prodr (One v)
```


### 6. Proof of the inductive case

Write your proof of the inductive case below.
```
#by inductive hypothesis
prod Node(t1,t2) = prod t1 * prod t2 = prodr t1 * prodr t2 

#by def prodr 
= (*) (reduce (fun x -> x) (*) t1) (reduce (fun x -> x) (*) t2)
#by def reduce
= reduce (fun x -> x) (*) Node(t1,t2)
#by simipification
= prodr Node(t1,t2)
```
