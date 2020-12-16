
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
(*
      ["Ocaml!  "; "It "; "must "; "be "]
          /                     \
        /                         \
      /                             \
  ["do "; "love "]              ["your "; "favorite "; "too!"]
    /             \                 /               \
  /                 \           Empty             Empty
 ["I "; "really "]   Empty
  /          \    
  Empty     Empty
*)


let rec reduce (b: 'b) (f: 'a -> 'b -> 'b -> 'b)
       (t: 'a tree) : 'b =
        match t with
        | Empty -> b
        | Node (v, t1, t2) -> f v (reduce b f t1) (reduce b f t2)

let size(t:'a tree):int = 
  let add3 v v1 v2 =  1 + v1 + v2 
  in
  reduce 0 add3 t 


let sum(t:'int tree) :int = 
  let add3 v v1 v2 =  v + v1 + v2 
  in
  reduce 0 add3 t         

let product(t:'int tree) : int =
  let sum_up v v1 v2 = v * v1 * v2
  in
  reduce 1 sum_up t


let charcount(t:'string tree) : int  =
  let count v v1 v2 = String.length v + v1 + v2
  in
  reduce 0 count t

let concat(t:'string tree) : string = 
  let combine v v1 v2 = v1 ^ v ^ v2
  in
  reduce "" combine t


let list_tree_size(t:'a list tree) :int = 
  let sum v v1 v2 = List.length(v) + v1 + v2
  in
  reduce 0 sum t

let sumList lst = List.fold_right(fun x accum -> accum + x)lst 0      
let list_tree_sum(t:int list tree):int = 
  let sum_list v v1 v2 = sumList(v) + v1 + v2
  in
  reduce 0 sum_list t

let produ lst = List.fold_right(fun x accum -> accum * x)lst 1
let list_tree_product(t:int list tree):int =
    let pro_list v v1 v2 = produ(v) * v1 * v2
    in
    reduce 1 pro_list t

let count lst = List.fold_right(fun x accum -> String.length(x) + accum)lst 0
let list_tree_charcount(t:string list tree):int =
    let count_char v v1 v2 = count(v) + v1 + v2
    in
    reduce 0 count_char t

let concat_string_list lst = List.fold_right(fun x accum -> x ^ accum)lst ""
let list_tree_concat(t:string list tree):string =
     let concat_list v v1 v2 = v1 ^ concat_string_list(v) ^ v2
     in
     reduce "" concat_list t





    let () = 
      print_string "Testing part 3 ... " ;
      try
        
        assert (size str_tree = 4);
        assert (size int_tree = 4);
        assert (size none_tree = 1);
        assert (size null_tree = 0);

        assert (sum int_tree = 10);
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