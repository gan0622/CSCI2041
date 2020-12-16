# Hwk_03 Assessment

Run on November 04, 17:24:07 PM.

*Test results are for the code in this repository as it appeared on October 21 at 17:15 CT.*

+ Pass: Change into directory "Hwk_03".

## Part 1

+ Pass: Check that file "trees.ml" exists.

### `size`

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   size   (Node ("World",
        Node (" ", 
              Node ("Hello", Empty, Empty),
              Empty
             ),
        Node ("!",
              Empty,
              Empty
             )
       )
   )
   ```
   matches the pattern `4`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   size   (Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `6`.

   




### `sum`

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   sum   (Node (-3, 
        Node (-2, Empty, Empty),
        Node (-1,
              Node (4, Empty, Empty), 
              Empty
              )
       )
   )
   ```
   matches the pattern `-2`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   sum   (Node (3,
        Node (3, 
              Empty,
              Empty
             ),
        Node (2,
              Node (5, Empty, Empty),
              Node (5,Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `18`.

   




### `product`

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   product   (Node (-3, 
        Node (-2, Empty, Empty),
        Node (-1,
              Node (4, Empty, Empty), 
              Empty
              )
       )
   )
   ```
   matches the pattern `-24`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   product   (Node (3,
        Node (3, 
              Empty,
              Empty
             ),
        Node (2,
              Node (5, Empty, Empty),
              Node (5,Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `450`.

   




### `charcount`

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   charcount   (Node ("World",
        Node (" ", 
              Node ("Hello", Empty, Empty),
              Empty
             ),
        Node ("!",
              Empty,
              Empty
             )
       )
   )
   ```
   matches the pattern `12`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   charcount   (Node ("French ", 
	Node ("love ", 
		Node ("I ",
			Empty,
			Node ("really ", Node ("", Empty, Empty), Empty)),
	        Empty),
        Node ("Toast.", Empty, Empty))
	)


   ```
   matches the pattern `27`.

   




### `concat`

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   concat   (Node ("World",
        Node (" ", 
              Node ("Hello", Empty, Empty),
              Empty
             ),
        Node ("!",
              Empty,
              Empty
             )
       )
   )
   ```
   matches the pattern `"Hello World!"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   concat   (Node ("French ", 
	Node ("love ", 
		Node ("I ",
			Empty,
			Node ("really ", Node ("", Empty, Empty), Empty)),
	        Empty),
        Node ("Toast.", Empty, Empty))
	)


   ```
   matches the pattern `"I really love French Toast."`.

   




### Assert Cases

+  _0_ / _5_ : Pass: Part 1: Extra assert cases (1pt per function).

    No extra assert cases

### Style

+  _7_ / _7_ : Pass: Part 1 Style: check if each function looks OK (no obvious problems with style).

    

## Part 2

### `list_tree_size`

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_size   (Node (["13"],
        Node (["45";"6"], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node (["1";"6"], Empty, Empty),
              Node (["928"],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `6`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_size   (Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `10`.

   




### `list_tree_sum`

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_sum   (Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `45`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_sum   (Node ([],
        Node ([1;2], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([4;5],
              Node ([-3], Empty, Empty),
              Node ([],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `9`.

   




### `list_tree_product`

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_product   (Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `311040`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_product   (Node ([],
        Node ([1;2], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([4;5],
              Node ([-3], Empty, Empty),
              Node ([],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `-120`.

   




### `list_tree_charcount`

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_charcount   (Node (["13"],
        Node (["45";"6"], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node (["1";"6"], Empty, Empty),
              Node (["928"],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `10`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_charcount   (Node (["like "; "French "; "Toast "; ""],
        Node ([], 
              Node (["I "; ""; "really "], Empty, Empty), Empty), 
        Node (["pancake!"], Empty, Empty) 
       )
   )
   ```
   matches the pattern `35`.

   




### `list_tree_concat`

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_concat   (Node (["13"],
        Node (["45";"6"], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node (["1";"6"], Empty, Empty),
              Node (["928"],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `"4561316928"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_concat   (Node (["like "; "French "; "Toast "; ""],
        Node ([], 
              Node (["I "; ""; "really "], Empty, Empty), Empty), 
        Node (["pancake!"], Empty, Empty) 
       )
   )
   ```
   matches the pattern `"I really like French Toast pancake!"`.

   




### Assert Cases

+  _0_ / _5_ : Pass: Part 2: Extra assert cases (1pt per function).

    No extra assert cases

### Style

+  _7_ / _7_ : Pass: Part 2 Style: check if each function looks OK (no obvious problems with style).

    

## Part 3

+ Pass: Check that file "reduce.ml" exists.

### `size`

+ Pass: The function ``size`` must not be recursive.





+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   size   (Node ("World",
        Node (" ", 
              Node ("Hello", Empty, Empty),
              Empty
             ),
        Node ("!",
              Empty,
              Empty
             )
       )
   )
   ```
   matches the pattern `4`.

   




+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   size   (Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `6`.

   




### `sum`

+ Pass: The function ``sum`` must not be recursive.





+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   sum   (Node (-3, 
        Node (-2, Empty, Empty),
        Node (-1,
              Node (4, Empty, Empty), 
              Empty
              )
       )
   )
   ```
   matches the pattern `-2`.

   




+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   sum   (Node (3,
        Node (3, 
              Empty,
              Empty
             ),
        Node (2,
              Node (5, Empty, Empty),
              Node (5,Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `18`.

   




### `product`

+ Pass: The function ``product`` must not be recursive.





+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   product   (Node (-3, 
        Node (-2, Empty, Empty),
        Node (-1,
              Node (4, Empty, Empty), 
              Empty
              )
       )
   )
   ```
   matches the pattern `-24`.

   




+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   product   (Node (3,
        Node (3, 
              Empty,
              Empty
             ),
        Node (2,
              Node (5, Empty, Empty),
              Node (5,Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `450`.

   




### `charcount`

+ Pass: The function ``charcount`` must not be recursive.





+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   charcount   (Node ("World",
        Node (" ", 
              Node ("Hello", Empty, Empty),
              Empty
             ),
        Node ("!",
              Empty,
              Empty
             )
       )
   )
   ```
   matches the pattern `12`.

   




+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   charcount   (Node ("French ", 
	Node ("love ", 
		Node ("I ",
			Empty,
			Node ("really ", Node ("", Empty, Empty), Empty)),
	        Empty),
        Node ("Toast.", Empty, Empty))
	)


   ```
   matches the pattern `27`.

   




### `concat`

+ Pass: The function ``concat`` must not be recursive.





+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   concat   (Node ("World",
        Node (" ", 
              Node ("Hello", Empty, Empty),
              Empty
             ),
        Node ("!",
              Empty,
              Empty
             )
       )
   )
   ```
   matches the pattern `"Hello World!"`.

   




+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   concat   (Node ("French ", 
	Node ("love ", 
		Node ("I ",
			Empty,
			Node ("really ", Node ("", Empty, Empty), Empty)),
	        Empty),
        Node ("Toast.", Empty, Empty))
	)


   ```
   matches the pattern `"I really love French Toast."`.

   




### `list_tree_size`

+ Pass: The function ``list_tree_size`` must not be recursive.





+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_size   (Node (["13"],
        Node (["45";"6"], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node (["1";"6"], Empty, Empty),
              Node (["928"],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `6`.

   




+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_size   (Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `10`.

   




### `list_tree_sum`

+ Pass: The function ``list_tree_sum`` must not be recursive.





+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_sum   (Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `45`.

   




+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_sum   (Node ([],
        Node ([1;2], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([4;5],
              Node ([-3], Empty, Empty),
              Node ([],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `9`.

   




### `list_tree_product`

+ Pass: The function ``list_tree_product`` must not be recursive.





+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_product   (Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `311040`.

   




+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_product   (Node ([],
        Node ([1;2], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([4;5],
              Node ([-3], Empty, Empty),
              Node ([],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `-120`.

   




### `list_tree_charcount`

+ Pass: The function ``list_tree_charcount`` must not be recursive.





+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_charcount   (Node (["13"],
        Node (["45";"6"], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node (["1";"6"], Empty, Empty),
              Node (["928"],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `10`.

   




+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_charcount   (Node (["like "; "French "; "Toast "; ""],
        Node ([], 
              Node (["I "; ""; "really "], Empty, Empty), Empty), 
        Node (["pancake!"], Empty, Empty) 
       )
   )
   ```
   matches the pattern `35`.

   




### `list_tree_concat`

+ Pass: The function ``list_tree_concat`` must not be recursive.





+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_concat   (Node (["13"],
        Node (["45";"6"], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node (["1";"6"], Empty, Empty),
              Node (["928"],Empty,Empty)
             )
       )
   )
   ```
   matches the pattern `"4561316928"`.

   




+  _0.5_ / _0.5_ : Pass: 
Check that the result of evaluating
   ```
   list_tree_concat   (Node (["like "; "French "; "Toast "; ""],
        Node ([], 
              Node (["I "; ""; "really "], Empty, Empty), Empty), 
        Node (["pancake!"], Empty, Empty) 
       )
   )
   ```
   matches the pattern `"I really like French Toast pancake!"`.

   




### Style

+  _6_ / _6_ : Pass: Part 3 Style: check if each function looks OK (no obvious problems with style).

    

## Part 4

+ Pass: Check that file "binary_trees.ml" exists.

### `insert_by`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   insert_by compare 6     (Fork (
        Fork (Nil, -2, Leaf 5),
        10,
        Fork (
            Nil,
            13,
            Fork (
                Nil,
                14,
                Fork (Nil, 16, Leaf 20)
            )
        )
    ))
   ```
   matches the pattern `    (Fork (
        Fork (
	    Nil,
	    -2,
	    Fork (Nil, 5, Leaf 6)
	),
        10,
        Fork (
            Nil,
            13,
            Fork (
                Nil,
                14,
                Fork (Nil, 16, Leaf 20)
            )
        )
    ))`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   insert_by compare 11     (Fork (
        Fork (Nil, -2, Leaf 5),
        10,
        Fork (
            Nil,
            13,
            Fork (
                Nil,
                14,
                Fork (Nil, 16, Leaf 20)
            )
        )
    ))
   ```
   matches the pattern `    (Fork (
        Fork (Nil, -2, Leaf 5),
        10,
        Fork (
            Leaf 11,
            13,
            Fork (
                Nil,
                14,
                Fork (Nil, 16, Leaf 20)
            )
        )
    ))`.

   




+  _0_ / _2_ : Fail: 
Check that the result of evaluating
   ```
   insert_by compare 13     (Fork (
        Fork (Nil, -2, Leaf 5),
        10,
        Fork (
            Nil,
            13,
            Fork (
                Nil,
                14,
                Fork (Nil, 16, Leaf 20)
            )
        )
    ))
   ```
   matches the pattern `    (Fork (
        Fork (Nil, -2, Leaf 5),
        10,
        Fork (
            Nil,
            13,
            Fork (
                Nil,
                14,
                Fork (Nil, 16, Leaf 20)
            )
        )
    ))`.

   


   Your solution evaluated incorrectly and produced some part of the following:

 
   ```
                            - : int btree =
    Fork (Fork (Nil, -2, Leaf 5), 10,
     Fork (Nil, 13, Fork (Leaf 13, 14, Fork (Nil, 16, Leaf 20))))
    ```



+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   insert_by (fun x y -> compare x y * (-1)) 11     (Fork (
        Fork (Nil, 13, Leaf 12),
        10,
        Leaf 3
    ))
   ```
   matches the pattern `    (Fork (
        Fork (
	    Nil,
	    13,
	    Fork (Nil, 12, Leaf 11)    
	),
        10,
        Leaf 3
    ))`.

   




+  _0_ / _2_ : Pass: insert_by: extra assert cases

    No extra assert cases

+  _4_ / _4_ : Pass: insert_by: check that function style looks OK (no obvious problems with style).

    

### `from_list`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   from_list compare [10; -2; 5; 13; 14; 16; 20]
   ```
   matches the pattern `    (Fork (
        Fork (Nil, -2, Leaf 5),
        10,
        Fork (
            Nil,
            13,
            Fork (
                Nil,
                14,
                Fork (Nil, 16, Leaf 20)
            )
        )
    ))`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   from_list (fun x y -> compare x y * (-1)) [10; 10; 3; 13; 12; 12]
   ```
   matches the pattern `    (Fork (
        Fork (Nil, 13, Leaf 12),
        10,
        Leaf 3
    ))`.

   




+  _1_ / _1_ : Pass: from_list: uses List.fold_left

    

+  _1_ / _1_ : Pass: from_list: makes call to `insert_by` in fold's helper function

    

+  _0_ / _2_ : Pass: from_list: extra assert cases

    

+  _2_ / _2_ : Pass: from_list: check that function style looks OK (no obvious problems with style).

    

### `reduce`

+  _1_ / _1_ : Pass: reduce: Nil - returns accumulator argument

    

+  _1_ / _1_ : Pass: reduce: Fork - applies a generic function to Fork value and the 2 reduced children.

    

+  _1_ / _1_ : Pass: reduce: Leaf - applies a generic function (different from Fork's) to Leaf value (Leaf v -> f v)

    

+  _0_ / _2_ : Pass: reduce: extra assert cases

    Same comments as before

+  _2_ / _2_ : Pass: reduce: check that function style looks OK (no obvious problems with style).

    

### `to_list`

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   to_list     (Fork (
        Fork (Nil, -2, Leaf 5),
        10,
        Fork (
            Nil,
            13,
            Fork (
                Nil,
                14,
                Fork (Nil, 16, Leaf 20)
            )
        )
    ))
   ```
   matches the pattern `[-2; 5; 10; 13; 14; 16; 20]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   to_list     (Fork (
        Fork (Nil, 13, Leaf 12),
        10,
        Leaf 3
    ))
   ```
   matches the pattern `[13; 12; 10; 3]`.

   




+  _1_ / _1_ : Pass: to_list: doesn't use rec keyword

    

+  _1_ / _1_ : Pass: to_list: uses reduce

    

+  _0_ / _2_ : Pass: to_list: extra assert cases

    Same comments as before

+  _1_ / _1_ : Pass: to_list: check that function style looks OK (no obvious problems with style).

    

#### Total score: _80_ / _100.0_

