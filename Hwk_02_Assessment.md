# Hwk_02 Assessment

Run on October 27, 20:01:11 PM.

*Test results are for the code in this repository as it appeared on October 14 at 17:15 CT.*

+ Pass: Change into directory "Hwk_02".

+ Pass: Check that file "hwk_02.ml" exists.

``length``

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   length []
   ```
   matches the pattern `0`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   length [1;2;3;4;5;6]
   ```
   matches the pattern `6`.

   




+  _1_ / _1_ : Pass: length: calls fold

    

+  _1_ / _1_ : Pass: length: correct type description

    

``rev``

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   rev []
   ```
   matches the pattern `[ ]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   rev [1]
   ```
   matches the pattern `[1]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   rev [1; 2; 3; 4]
   ```
   matches the pattern `[4; 3; 2; 1]`.

   




+  _2_ / _2_ : Pass: rev: calls fold

    

``is_elem_by``

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   is_elem_by (fun x y -> (x+1) = y) 6 [2;3;5;7]
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   is_elem_by (fun x y -> (x+1) = y) 0 [2;3;5;7]
   ```
   matches the pattern `false`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   is_elem_by (fun x y -> x < y) 4 [2;3;5;7]
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   is_elem_by (fun x y -> x < y) 0 [2;3;5;7]
   ```
   matches the pattern `false`.

   




+  _2_ / _2_ : Pass: is_elem_by: ‘rec’ keyword is never used

    

+  _4_ / _4_ : Pass: is_elem_by: type annotations

    

``is_elem``

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   is_elem 3 []
   ```
   matches the pattern `false`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   is_elem 3 [1; 2; 3]
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   is_elem 4 [1; 2; 3]
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: is_elem: correct type annotations

    

+  _2_ / _2_ : Pass: is_elem: calls is_elem_by

    

``dedup``

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort (fun x y -> if x < y then -1 else 1) (dedup [1;1;4;5;2;3;2])
   ```
   matches the pattern `[1; 2; 3; 4; 5]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort (fun x y -> if x < y then -1 else 1) (dedup [[13; 1]; [13; 1]; [1; 2]; [10; 5]])
   ```
   matches the pattern `[[1; 2]; [10; 5]; [13; 1]]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   dedup []
   ```
   matches the pattern `[ ]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.sort (fun x y -> if x < y then -1 else 1) (dedup [4;5;2;3])
   ```
   matches the pattern `[2; 3; 4; 5]`.

   




+  _1_ / _1_ : Pass: dedup: calls fold (no points here if ‘rec’ keyword is ever used)

    

+  _1_ / _1_ : Pass: dedup: calls fold_right (not fold_left) (no points here if ‘rec’ keyword is ever used)

    

+  _2_ / _2_ : Pass: dedup: passes function that uses is_elem (not is_elem_by) (no points here if ‘rec’ keyword is ever used)

    

``split_by``

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   split_by (=) ['a';'b';'c';'d'] ['c']
   ```
   matches the pattern `[['a'; 'b']; ['d']]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   split_by (>) ['a';'b';'c';'d'] ['c']
   ```
   matches the pattern `[[]; []; ['c'; 'd']]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   split_by (=) ['a';'b';'c';'d'] []
   ```
   matches the pattern `[['a'; 'b'; 'c'; 'd']]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   split_by (=) [] ['x']
   ```
   matches the pattern `[[ ]]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   split_by (=) [] []
   ```
   matches the pattern `[[ ]]`.

   




+  _2_ / _2_ : Pass: split_by: passes a helper function that uses is_elem_by

    

+  _2_ / _2_ : Pass: split_by: calls fold on this helper function

    

+  _5_ / _5_ : Pass: split_by: fully generic type annotations

    

+  _5_ / _5_ : Pass: split_by: A comment explaining behavior of function (no points here if ‘rec’ keyword is ever used or module function not on approved list is ever used)

    

+  _3_ / _3_ : Pass: split_by: A comment explaining type of function (no points here if ‘rec’ keyword is ever used or module function not on approved list is ever used)

    

+  _1_ / _1_ : Pass: split_by: good enough indentation

    

``convert_to_non_blank_lines_of_words``

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.map (List.map (fun y -> List.map Char.lowercase y)) (convert_to_non_blank_lines_of_words ['W'; 'h'; 'e'; ','; 'a'; ' '; 'b'])
   ```
   matches the pattern `[[['w'; 'h'; 'e']; ['a']; ['b']]]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.map (List.map (fun y -> List.map Char.lowercase y)) (convert_to_non_blank_lines_of_words ['W'; 'h'; 'e'; ','; 'a'; '
'; 'b'])
   ```
   matches the pattern `[[['w'; 'h'; 'e']; ['a']]; [['b']]]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   List.map (List.map (fun y -> List.map Char.lowercase y)) (convert_to_non_blank_lines_of_words ['W'; 'h'; 'e'; ' '; ','; 'a'; '-'; 'b'])
   ```
   matches the pattern `[[['w'; 'h'; 'e']; ['a']; ['b']]]`.

   




+  _4_ / _4_ : Pass: convert_to_non_blank_lines_of_words: 'rec' keyword is never used

    

+  _2_ / _2_ : Pass: convert_to_non_blank_lines_of_words: correct type annotations (with output annotated as ‘line list’)

    

+  _1_ / _1_ : Pass: convert_to_non_blank_lines_of_words: good enough indentation

    

``paradelle``

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   paradelle "not_a_paradelle_emma_1.txt"
   ```
   matches the pattern `IncorrectLastStanza`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   paradelle "not_a_paradelle_empty_file.txt"
   ```
   matches the pattern `IncorrectNumLines 0`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   paradelle "not_a_paradelle_susan_1.txt"
   ```
   matches the pattern `IncorrectLines [(1, 2); (11, 12); (17, 18)]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   paradelle "not_a_paradelle_susan_2.txt"
   ```
   matches the pattern `IncorrectLines [(11, 12); (17, 18)]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   paradelle "not_a_paradelle_susan_3.txt"
   ```
   matches the pattern `IncorrectLines [(1, 2); (11, 12); (17, 18)]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   paradelle "not_a_paradelle_wrong_line_count.txt"
   ```
   matches the pattern `IncorrectNumLines 9`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   paradelle "paradelle_emma_1.txt"
   ```
   matches the pattern `OK`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   paradelle "paradelle_susan_1.txt"
   ```
   matches the pattern `OK`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   paradelle "paradelle_susan_2.txt"
   ```
   matches the pattern `OK`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   paradelle "abc.txt"
   ```
   matches the pattern `FileNotFound "abc.txt"`.

   




+  _5_ / _5_ : Pass: paradelle: uses drop and take in helper function(s) or body of paradelle function (no points here if ‘rec’ keyword is ever used or module function not on approved list is ever used)

    

+  _2_ / _2_ : Pass: paradelle: uses ‘match … with’ (no points here if ‘rec’ keyword is ever used or module function not on approved list is ever used)

    

+  _1_ / _1_ : Pass: paradelle: correct type annotations

    

+  _2_ / _2_ : Pass: paradelle: defines primary helper functions separately, not nested in ‘let … in’ expressions

    

+  _1_ / _1_ : Pass: paradelle: good enough indentation

    

#### Total score: _119_ / _119_

