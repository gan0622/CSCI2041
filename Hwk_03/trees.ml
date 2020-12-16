
(* A tree type declaration. *)
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

(* A sample tree containing ints *)
let int_tree : int tree =
  Node (3, 
        Node (1,
              Node (4, Empty, Empty), Empty), 
        Node (2, Empty, Empty) 
       )

(* A sample tree containing strings *)
let str_tree : string tree = 
  Node ("love ", 
        Node ("really ", 
              Node ("I ", Empty, Empty), Empty), 
        Node ("OCaml!", Empty, Empty) 
       )

let none_tree : int tree = Node(0, Empty, Empty)

let null_tree : int tree = Empty

let one_node_tree : string tree = Node("love",Empty,Empty)

let null_node_tree : string tree = Empty


let rec size(t:'a tree) : int = 
    match t with
    |Empty -> 0
    |Node(v,t1,t2) -> 1 + size t1 + size t2 

              
let rec sum(t:'a tree):int = 
    match t with
    |Empty -> 0
    |Node(v,t1,t2) -> v + sum t1 + sum t2
    

let rec product(t:int tree):int = 
    match t with
    |Empty -> 1
    |Node(v,t1,t2) -> v * product t1 * product t2


let rec charcount(t: string tree):int =
      match t with
      |Empty -> 0
      |Node(v,t1,t2) -> String.length v + charcount t1 + charcount t2 

let rec concat(t: string tree) : string = 
      match t with
      |Empty -> ""
      |Node(v,t1,t2) -> concat t1 ^ v ^ concat t2



let () = 
  print_string "Testing part 1 ... " ;
  try
    
    assert (size str_tree = 4);
    assert (size int_tree = 4);
    assert (size none_tree = 1);
    assert (size null_tree = 0);

    assert (sum int_tree = 10);
    assert (sum none_tree = 0);
    assert (sum null_tree = 0);

    assert (product int_tree = 24);
    assert (product none_tree = 0);
    assert (product null_tree = 1);

    assert (charcount str_tree = 20);
    assert (charcount one_node_tree = 4);
    assert (charcount null_node_tree = 0);

    assert (concat str_tree = "I really love OCaml!");
    assert (concat one_node_tree = "love");
    assert (concat null_node_tree ="");
    
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg 






    
let ints_tree: int list tree =
  Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )

let strs_tree: string list tree = 
  Node (["Ocaml!  "; "It "; "must "; "be "],
        Node (["do "; "love "], 
              Node (["I "; "really "], Empty, Empty), Empty), 
        Node (["your "; "favorite "; "too!"], Empty, Empty) 
       )   
 
let one_int_tree: int list tree = Node([1;0],Empty,Empty)

let null_int_tree : int list tree = Empty

let one_word_tree : string list tree = Node(["hello"],Empty,Empty)

let null_word : string list tree = Empty

let rec list_tree_size(t:'a list tree):int = 
	match t with
	|Empty -> 0
	|Node(v,t1,t2) -> List.length(v) + list_tree_size t1 + list_tree_size t2


let sumList lst = List.fold_right(fun x accum -> accum + x)lst 0      
let rec list_tree_sum(t:int list tree):int = 
      match t with
      |Empty -> 0
      |Node(v,t1,t2) -> sumList(v) + list_tree_sum t1 + list_tree_sum t2


let produ lst = List.fold_right(fun x accum -> accum * x)lst 1
let rec list_tree_product(t:int list tree):int = 
      match t with
      |Empty -> 1
      |Node(v,t1,t2) ->produ(v) * list_tree_product t1 * list_tree_product t2


let count lst = List.fold_right(fun x accum -> String.length(x) + accum)lst 0
let rec list_tree_charcount(t:string list tree):int =
      match t with
      |Empty -> 0
      |Node(v,t1,t2) -> count(v) + list_tree_charcount t1 + list_tree_charcount t2

let concat_string_list lst = List.fold_right(fun x accum -> x ^ accum)lst ""
let rec list_tree_concat(t:string list tree):string =
      match t with
      |Empty -> ""
      |Node(v,t1,t2) -> list_tree_concat t1 ^ concat_string_list(v) ^ list_tree_concat t2

      
let () = 
  print_string "Testing part 2 ... " ;
  try
    
    assert (list_tree_size strs_tree = 11);
    assert (list_tree_size one_int_tree = 2);
    assert (list_tree_size null_int_tree = 0);
    assert (list_tree_size ints_tree = 10);

    assert (list_tree_sum ints_tree = 45);
    assert (list_tree_sum one_int_tree = 1);
    assert (list_tree_sum null_int_tree = 0);  


    assert (list_tree_product ints_tree = 311040);
    assert (list_tree_product one_int_tree = 0);
    assert (list_tree_product null_int_tree = 1);

    assert (list_tree_charcount strs_tree = 54);
    assert (list_tree_charcount one_word_tree = 5);
    assert (list_tree_charcount null_word = 0 );

    assert (list_tree_concat strs_tree = 
              "I really do love Ocaml!  It must be your favorite too!");
    assert (list_tree_concat one_word_tree = "hello");
    assert (list_tree_concat null_word = "");
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg 


