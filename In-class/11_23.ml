let s = [1;3;-2;5;-6]

let rec gen_subsets(lst : 'a list) : 'a list list
  = match lst with 
  |[] -> [[]]
  | x :: xs -> let rest_subsets : 'a list list = gen_subsets xs
                in rest_subsets @ List.map (fun ss -> x :: ss) rest_subsets
