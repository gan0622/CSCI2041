# Hwk_03 Feedback

Run on November 06, 15:13:31 PM.

+ Pass: Change into directory "Hwk_03".

## Part 1

+ Pass: Check that file "trees.ml" exists.

+ Pass: Check that an OCaml file "trees.ml" has no syntax or type errors.

    OCaml file "trees.ml" has no syntax or type errors.



Be sure your ``trees.ml`` file contains the sample trees ``str_tree`` and ``int_tree`` that are provided in the Hwk 03 specification.

+ Pass: 
Check that the result of evaluating
   ```
   size str_tree
   ```
   matches the pattern `4`.

   




+ Pass: 
Check that the result of evaluating
   ```
   sum int_tree
   ```
   matches the pattern `10`.

   




+ Pass: 
Check that the result of evaluating
   ```
   product int_tree
   ```
   matches the pattern `24`.

   




+ Pass: 
Check that the result of evaluating
   ```
   charcount str_tree
   ```
   matches the pattern `20`.

   




+ Pass: 
Check that the result of evaluating
   ```
   concat str_tree
   ```
   matches the pattern `"I really love OCaml!"`.

   




## Part 2. Trees with lists

+ Pass: Check that file "trees.ml" exists.

+ Pass: Check that an OCaml file "trees.ml" has no syntax or type errors.

    OCaml file "trees.ml" has no syntax or type errors.



Be sure your ``trees.ml`` file contains the sample trees ``strs_tree`` and ``ints_tree`` that are provided in the Hwk 03 specification.

+ Pass: 
Check that the result of evaluating
   ```
   list_tree_size strs_tree
   ```
   matches the pattern `11`.

   




+ Pass: 
Check that the result of evaluating
   ```
   list_tree_sum ints_tree
   ```
   matches the pattern `45`.

   




+ Pass: 
Check that the result of evaluating
   ```
   list_tree_product ints_tree
   ```
   matches the pattern `311040`.

   




+ Pass: 
Check that the result of evaluating
   ```
   list_tree_charcount strs_tree
   ```
   matches the pattern `54`.

   




+ Pass: 
Check that the result of evaluating
   ```
   list_tree_concat strs_tree
   ```
   matches the pattern `"I really do love Ocaml!  It must be your favorite too!"`.

   




## Part 3 - using ``reduce``

+ Pass: Check that file "reduce.ml" exists.

+ Pass: Check that an OCaml file "reduce.ml" has no syntax or type errors.

    OCaml file "reduce.ml" has no syntax or type errors.



+ Pass: Make sure you are only using recursion in functions reduce

   



Be sure your ``reduce.ml`` file contains the sample trees ``strs_tree``, ``strs_tree``, ``int_tree``, and ``ints_tree`` that are provided in the Hwk 03 specification.

+ Pass: 
Check that the result of evaluating
   ```
   size str_tree
   ```
   matches the pattern `4`.

   




+ Pass: 
Check that the result of evaluating
   ```
   sum int_tree
   ```
   matches the pattern `10`.

   




+ Pass: 
Check that the result of evaluating
   ```
   product int_tree
   ```
   matches the pattern `24`.

   




+ Pass: 
Check that the result of evaluating
   ```
   charcount str_tree
   ```
   matches the pattern `20`.

   




+ Pass: 
Check that the result of evaluating
   ```
   concat str_tree
   ```
   matches the pattern `"I really love OCaml!"`.

   




+ Pass: 
Check that the result of evaluating
   ```
   list_tree_size strs_tree
   ```
   matches the pattern `11`.

   




+ Pass: 
Check that the result of evaluating
   ```
   list_tree_sum ints_tree
   ```
   matches the pattern `45`.

   




+ Pass: 
Check that the result of evaluating
   ```
   list_tree_product ints_tree
   ```
   matches the pattern `311040`.

   




+ Pass: 
Check that the result of evaluating
   ```
   list_tree_charcount strs_tree
   ```
   matches the pattern `54`.

   




+ Pass: 
Check that the result of evaluating
   ```
   list_tree_concat strs_tree
   ```
   matches the pattern `"I really do love Ocaml!  It must be your favorite too!"`.

   




## Part 4 - sorted binary trees

+ Pass: Check that file "binary_trees.ml" exists.

+ Pass: Check that an OCaml file "binary_trees.ml" has no syntax or type errors.

    OCaml file "binary_trees.ml" has no syntax or type errors.



+ Pass: Make sure you are only using recursion in functions insert_by, reduce, check, form_check

   



+ Pass: 
Check that the result of evaluating
   ```
   insert_by compare 4 Nil
   ```
   matches the pattern `Leaf 4`.

   




+ Pass: 
Check that the result of evaluating
   ```
   insert_by compare 2 (insert_by compare 4 Nil)
   ```
   matches the pattern `Fork (Leaf 2, 4, Nil)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   insert_by compare 4 (insert_by compare 2 Nil)
   ```
   matches the pattern `Fork (Nil, 2, Leaf 4)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   insert_by compare 4 (insert_by compare 4 Nil)
   ```
   matches the pattern `insert_by compare 4 Nil`.

   




+ Pass: 
Check that the result of evaluating
   ```
   from_list compare [4;2;5;3;6;7;8] = Fork (Fork (Nil, 2, Leaf 3), 4, Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8))))
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   List.sort compare [4;2;5;3;6;7;8]
   ```
   matches the pattern `to_list (from_list compare [4;2;5;3;6;7;8])`.

   




