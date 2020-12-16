
type 'a btree = Nil
              | Leaf of 'a
              | Fork of 'a btree * 'a * 'a btree


(*Nil is a namebait, actually means NULL. Step 1 if NULL then insert a number, then check the leaf
, if the number is greater than leaf num, then put it on the right else left, else no move....*)
let rec insert_by(func:'a -> 'a -> int)(num:'a)(t:'a btree) : 'a btree =
    match t with
    |Nil -> Leaf(num)
    |Leaf(v) -> let c = func num v 
                in
                if c > 0 then Fork(Nil,v,Leaf(num))
                else if c < 0 then Fork(Leaf(num),v,Nil)
                else Leaf(num)
    |Fork(t1,v,t2) -> if func v num > 0
                      then Fork(insert_by func num t1, v, t2)
                      else Fork(t1, v, insert_by func num t2) 



(* let rec from_list (func:'a -> 'a -> int)(lst:'a list): 'a btree = match lst with
    |[] -> Nil
    |hd :: tl -> insert_by flip(func hd (from_list func tl)) *)

let from_list (func:'a -> 'a -> int)(lst:'a list): 'a btree =
    List.fold_left (fun accum x -> insert_by func x accum)Nil lst

let rec reduce (b: 'b) (f: 'b -> 'a -> 'b -> 'b)(fc:'a -> 'b)
    (t: 'a btree) : 'b =
     match t with
     | Nil -> b
     | Leaf(v) -> fc v
     | Fork (t1,v,t2) -> f (reduce b f fc t1) v (reduce b f fc t2)                                


let to_list(t:'a btree) : 'a list =
    let append t1 v t2 = t1 @ [v] @ t2
    in
    reduce [] append (fun v -> [v]) t






let () = 
  print_string "Testing part 4 ... " ;
  try
    assert (insert_by compare 4 Nil = Leaf 4);

    assert (insert_by compare 2 (insert_by compare 4 Nil) =
              Fork (Leaf 2, 4, Nil));
    assert (insert_by compare 4 (insert_by compare 2 Nil) =
              Fork (Nil, 2, Leaf 4));
    assert (insert_by compare 4 (insert_by compare 4 Nil) = 
              insert_by compare 4 Nil);
    assert (from_list compare [4;2;5;3;6;7;8] =
            Fork (Fork (Nil, 2, Leaf 3), 4,
                  Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8)))
                  ) 
    );
    
    assert (from_list compare [] = Nil);

    assert (from_list compare [1] = Leaf 1);

    assert (List.sort compare [] =
    to_list (from_list compare []));

    assert (List.sort compare [4;2;5;3;6;7;8] =
    to_list (from_list compare [4;2;5;3;6;7;8]));

    assert (List.sort compare [1] =
    to_list (from_list compare [1]));

    (* Add more asserts here as you need them *)
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg
  