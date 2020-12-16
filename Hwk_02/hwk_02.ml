(* This file contains a few helper functions and type declarations
   that are to be used in Homework 2. *)

(* Place part 1 functions 'take', 'drop', 'length', 'rev',
   'is_elem_by', 'is_elem', 'dedup', and 'split_by' here. *)

let rec take n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then x::take (n-1) xs else []

let rec drop n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then drop (n-1) xs else l

let rev lst = List.fold_left(fun accum x -> x :: accum)[] lst

let length lst = List.fold_left(fun accum x -> accum + 1) 0 lst

(*is_elem_by (=) 4 [1; 2; 3; 4; 5; 6; 7]*)
let is_elem_by (func :'a ->'b -> bool)(ele : 'b)(lst : 'a list): bool = List.fold_left(fun accum x -> if(func x ele) then true else accum)false lst

let is_elem (ele:'a) (lst:'a list) : bool = is_elem_by (=) ele lst

let dedup lst = List.fold_right(fun x accum -> if (is_elem x accum) then accum else x::accum)lst [] 

(*split_by (>) [4; 2; 5; 4] [3]*)
let split_by (f:'a -> 'b -> bool) (lst:'a list) (sep:'b list): 'a list list =
  let f ele ls = (*check the ele if (=) to seperators*)
        if(is_elem_by f ele sep) (*if ele match sep*)
        then [[]] @ ls (*append [] into array, and add ele into array later*)
        else match ls with
            |hd :: tail -> (ele :: hd) :: tail (*else ele append into array*)
            |[] -> []
        in
        List.fold_right f lst [[]] 




(* Some functions for reading files. *)
let read_file (filename:string) : char list option =
  let rec read_chars channel sofar =
    try 
      let ch = input_char channel
      in read_chars channel (ch :: sofar)
    with
    | _ -> sofar
  in
  try 
    let channel = open_in filename
    in 
    let chars_in_reverse = read_chars channel []
    in Some (rev chars_in_reverse)
  with
    _ -> None

type word = char list
type line = word list

let remove_blank_space(lst:'a list list) : 'a list list = List.fold_right(fun x accum -> if x = [] then accum else x :: accum)lst []

(*call split_by, find out all the space and line breakers,once line breakers found it, append a [] and seperate the previous one,*)
let convert_to_non_blank_lines_of_words(input:char list): line list = 
  let split_line word (line : line list): line list = remove_blank_space (split_by (=) word [' ';',';'-';'!';';';'.';'?';':']) :: line
  in
  List.fold_right split_line (remove_blank_space (split_by (=) input ['\n'])) []

(*[‘a’; ‘ ‘; ‘b’; ‘c’; ‘\n’; ‘d’; ‘e’; ‘f’]*)
(*return [‘a’; ‘ ‘; ‘b’; ‘c’; ‘\n’; ‘d’; ‘e’; ‘f’] -> [[[‘a’];[‘b’;’c’]][[‘d’;’se’;’f’]]]*)


type result = OK 
	    | FileNotFound of string
	    | IncorrectNumLines of int 
	    | IncorrectLines of (int * int) list
	    | IncorrectLastStanza

(*check the line one by one if its not parallel then put it in array, and skip the line from 19 - 24 coz they are not parallel *)
(*pivot will be 1 3 7 9 13 15 based on the comment on piazza. if 1 2 or 3 4 are wrong then assume 5 6 is*)
(*helper function to loop the paragraph to get each word, compare those words to fifith sixth line*)
let sort_word(lst:'a list) : 'a list = List.sort(fun cur nex -> if cur < nex then -1 else if  cur = nex then 0 else 1)  lst


let check input = let line_1_2 = if(take 1 input = take 1 (drop 1 input)) then [] else ([1,2])
		  in
		  let line_3_4 = if(take 1(drop 2 input) = take 1(drop 3 input)) then [] else([3,4])
		  in
		  let line_7_8 = if(take 1(drop 6 input) = take 1(drop 7 input)) then [] else([7,8])
		  in
		  let line_9_10 = if(take 1(drop 8 input) = take 1(drop 9 input)) then [] else([9,10])
		  in
		  let line_13_14 = if(take 1(drop 12 input) = take 1(drop 13 input)) then [] else ([13,14])
		  in
		  let line_15_16 = if (take 1(drop 14 input) = take 1(drop 15 input)) then [] else([15,16])
		  in (*now check the 5 6 11 12 17 18*) (*if first line and second line are not parallel, compare the the fifth and sixth lines are 			  having the same word as the line 1 and line 3*)	(*[["b"]]*)      (*[["a"]*)
		  let line_5_6 = if (line_1_2 @ line_3_4 = []) 
		  then 
		  (if(sort_word(List.concat((take 1 input) @ (take 1 (drop 2 input)))) = sort_word(List.concat((take 1 (drop 4 input)) @ (take 1 (drop 5 input))))) then [] else ([5,6]))
		  else (line_1_2 @ line_3_4)
		  in
		  let line_11_12 = if (line_7_8 @ line_9_10 = []) 
		  then 
		  (if(sort_word(List.concat((take 1 (drop 6 input)) @ (take 1 (drop 8 input)))) = sort_word(List.concat((take 1 (drop 10 input)) @ (take 1 (drop 11 input))))) then [] else ([11,12]))
		  else (line_7_8 @ line_9_10)
		  in
		  let line_17_18 = if (line_13_14 @ line_15_16 = []) 
		  then
		  (if(sort_word(List.concat((take 1 (drop 12 input)) @ (take 1 (drop 14 input)))) = sort_word(List.concat((take 1 (drop 16 input)) @ (take 1 (drop 17 input))))) then [] else ([17,18]))
		  else (line_13_14 @ line_15_16)
		  in
		  line_5_6 @ line_11_12 @ line_17_18 @ []

(*let check_one_paragraph l1 l2 l3 l4 l5 l6 number_of_line = 
	let line_1_2 =
		if l1 = l2
			then []
			else[(number_of_line+1,number_of_line+2)]		  
	in
	let line_3_4 =
		if l3 = l4
			then line_1_2
			else line_1_2 :: [(number_of_line+1,number_of_line+2)]
	in
	if line_3_4 == [] (*if [] then which means that line1 2 3 4 are parallel then split into word and check line 5 6*)
			then line_3_4
			else
	let line_1_2_3_4 = sort_word(l1@l3)
	in
	if line_1_2_3_4 = sort_word(l5@l6)
		then line_3_4
		else line_3_4 ::[(number_of_line+5,number_of_line+6)]*)

(*let test_tanzas s1 s2 s3 s4 = *)
let list_all_lines input = List.fold_right(fun x y -> x @ y)input []

let test_tanzas input = dedup(sort_word(list_all_lines(take 18 input))) = dedup(sort_word(list_all_lines(drop 18 input)))
	 		  
		   

let paradelle(filename:string):result =
    match read_file filename with
    |None -> FileNotFound filename
    |Some input ->
      let number_of_lines = List.map(List.map(fun y -> List.map Char.lowercase_ascii y)) (convert_to_non_blank_lines_of_words(input)) 
      in
      let total_number_lines = length number_of_lines
      in
      if total_number_lines != 24 
      then IncorrectNumLines total_number_lines
      else if check number_of_lines !=[]
      then IncorrectLines (check number_of_lines)
      else if test_tanzas number_of_lines = false
      then IncorrectLastStanza
      else
      OK
      (*match number_of_lines with
      |a :: b :: c :: d :: e :: f :: tl ->
      (
      	let s1 = check_one_paragraph a b c d e f 0
      	in
      	match tl with
      	|g :: h :: i :: j:: k :: l :: tail ->
      	(
      	  let s2 = s1 @ (check_one_paragraph g h i j k l 6)
      	  in
      	  macth tail with
      	  |m :: n :: o :: p :: q :: r ::last ->
      	  (
      	    let s3 = s2 @ (check_one_paragraph m n o p q r 12)
      	    in
      	    if length s3 = 0
      	    then OK
      	    else IncorrectLines s3*)
      	    
      	    
      

