(* Searching using exceptions *)

module P = PrintLists
(* Recall, you must use #mod_use "printLists.ml" if opening this
   file in utop. *)

let s = [1; 3; -2; 5; -6]
      
let sum lst = List.fold_left (+) 0 lst

(* We can also use exceptions in searching.  This goes against the
   general principle of only throwing an exception for truly
   unexpected results, but it does make writing the code a bit more
   convenient, so we will use them in this non-traditional way.

   In our first usage, an exception is thrown when we've found the
   value that we want and this quickly returns us to the top level
   where we can then report success.

   We now execute the two recursive calls to 'try_subset' in sequence,
   not needing to inspect the output of the first one.  If the first
   call finds a solution then it will raise an exception.  So we
   don't care about the value returned by that first call.  If it
   returns it only does so if it didn't find a solution, in which case
   we want to just keep searching.
 *)


(* The subsetsum function that raises an exception on finding a
   solution. 

   Compare this to the ``gen_subsets_alternate`` in the
   ``search_options.ml`` file.  It has a similar structure.
   Below, what replaces the ``@`` operator found there?
 *)
            
exception FoundSet of int list
                    
let subsetsum_exn_on_found (lst: int list) : int list option =
  let rec try_subset partial_subset rest : unit =
    if sum partial_subset = 0 &&
         partial_subset <> [] &&
           rest = []
    then
      raise (FoundSet partial_subset)
    else
      (match rest with
       | [] -> ()
       | x::xs -> try_subset (x::partial_subset) xs;
                  try_subset partial_subset xs
      )
  in try try_subset [] lst; None with
     | FoundSet result -> Some result



(* Another way to use exceptions in searching is to raise an exception
   when we the search process has reached a deadend or the found
   solution is not acceptable.

   In both cases we want to keep looking.  Thus we create a
   "KeepLooking" exception.
 *)

exception KeepLooking

(* In this example, we raise an exception when we reach a deadend in
   the search process.  This exception is caught in one of two places.

   The first is at the point where there are more possibilities to
   explore, and thus another call to try_subset is made.

   The second is at the point where there are no more possibilities
   and thus we catch the exeption and return None.
 *)

let subsetsum_exn (lst: int list) : int list option =
  let rec try_subset  partial_subset rest : int list =
    if sum partial_subset = 0 &&
         partial_subset <> [] &&
           rest = []
    then partial_subset
    else (match rest with
          | [] -> raise KeepLooking
          | x::xs -> (try try_subset (x::partial_subset) xs with
                      | KeepLooking -> 
                         try_subset partial_subset xs
                     )
         )
  in try Some (try_subset [] lst) with
     | KeepLooking -> None





(* In this example we again raise an exception to indicate that the
   search process should keep looking for more solutions, but now we
   use a version of the procss_solution function from above to have
   some process (the user) that can reject found solutions causing the
   function to keep searching.
 *)

let rec process_solution_exn show s =
  print_endline ("Here is a solution: " ^ show (List.rev s)) ;
  print_endline ("Do you like it ?" ) ;
  match String.sub (read_line ()) 0 1 with
  | "Y" | "y" -> print_endline "Thanks for playing!"; s
  | _ -> raise KeepLooking


let subsetsum_exn (lst: int list) : int list option =
  let rec try_subset partial_subset rest =
    if sum partial_subset = 0 && 
       partial_subset <> [] && 
       rest = []
    then
      process_solution_exn (P.show_list string_of_int) partial_subset
    else 
      match rest with
      | [] -> raise KeepLooking
      | x::xs -> try try_subset (x::partial_subset) xs with
                | KeepLooking ->
                   try_subset partial_subset xs
  in try Some (List.rev (try_subset [] lst)) with
       | KeepLooking -> None


                      
let subsetsum_exn_continuation (lst: int list)
      (success: int list -> int list) : int list option =
  let rec try_subset partial_subset rest =
    if sum partial_subset = 0 &&
         partial_subset <> [] &&
           rest = []
    then
      success partial_subset
    else
      match rest with
      | [] -> raise KeepLooking
      | x::xs -> try try_subset (x::partial_subset) xs with
                 | KeepLooking ->
                    try_subset partial_subset xs
  in try Some (List.rev (try_subset [] lst)) with
       | KeepLooking -> None
        
let subsetsum_exn_v1 lst =
  subsetsum_exn_continuation
    lst
    (process_solution_exn (P.show_list string_of_int))
    
let subsetsum_exn_first (lst: int list) : int list option =
  subsetsum_exn_continuation
    lst
    (fun s -> s)

let subsetsum_exn_all (lst: int list) : int list option =
  subsetsum_exn_continuation
    lst
    (fun s ->
      P.print_int_list (List.rev s);
      raise KeepLooking
    )


let subsetsum_exn_ref_all (lst: int list) : int list list = 
  let r = ref []
  in
  let _ =
  subsetsum_exn_continuation 
    lst
    (fun s -> 
      (r := (List.rev s) :: !r);
      raise KeepLooking
    )
  in 
  !r


let () =
  assert (List.mem [1; 5; -6]
            (subsetsum_exn_ref_all [1; 3; -2; 5; -6]));
  assert (List.mem [3; -2; 5;-6] 
            (subsetsum_exn_ref_all [1; 3; -2; 5; -6]))
 
