(* Person, Wolf, Goat, Cabbage 
   Consider the problem of a person needing to move his wolf, goat,
   and cabbage across a river in his canoe, under the following
   restrictions:

   - The canoe holds only the person and one of the wolf, goat, or 
     cabbage. 
   - The goat and cabbage cannot be left unattended or the goat will 
     eat the cabbage. 
   - The wolf and the goat cannot be left unattended or the wolf will
     eat the goat.
   - Only the person can operate the canoe.

   Is there a sequence of moves in which the person can safely transport
   all across the river with nothing being eaten?
*)

(* Types and functions for the crossing challenge. *)

(* Location: things are on the left (L) or right (R) side of the river. *)
type loc = L | R

(* A state in our search space is a configuration describing on which
   side of the river the person, wolf, goat, and cabbage are. *)
type state = loc * loc * loc * loc

(* A state is safe, or OK, when the goat and cabbage are together only
   when the person is also on the same side of the river and when the
   wolf and the goat are together only when person is on the same side of
   the river. 
 *)
let ok_state ( (p,w,g,c) : state ) : bool =
  p = g || (g <> c && w <> g)

(* The final state, or gaol state, is when everything is on the right (R)
   side of the river.
 *)
let final s = s = (R,R,R,R)

let other_side = function
  | L -> R
  | R -> L

(* The function `moves` generates a list of valid moves.  If computes
   all possible moves and then filters out the moves that are not "ok"
   - that is, moves that would result in something being eaten.
 *)
let moves (s:state) : state list =
 let move_person (p,w,g,c) 
   = [ ( other_side p, w, g, c ) ] 
 in
 let move_wolf (p,w,g,c) 
   = if p = w
     then [ ( other_side p, other_side w, g, c ) ]
     else [ ]
 in
 let move_goat (p,w,g,c) 
   = if p = g
     then [ ( other_side p, w, other_side g, c ) ]
     else [ ]
 in
 let move_cabbage (p,w,g,c) 
   = if p = c
     then [ ( other_side p, w, g, other_side c ) ]
     else [ ]
 in
 List.filter ok_state ( move_person s @ move_wolf s @ 
                        move_goat s @ move_cabbage s )
  
let rec is_not_elem set v = not (List.mem v set)

(* A solution using options that returns the first safe sequence 
   of moves. 

   We keep track of the current state and the path that got us
   there, making sure to not go back to a state already in the
   path. 
   
   An invariant: the `state` is the last element of the `path`.
 *)
let crossing_v1 () : state list option =
  let rec go_from (state: state) (path: state list) =
    if final state
    then Some path
    else
      match List.filter (is_not_elem path) (moves state) with
      | [] -> None
      | [a] -> go_from a (path @ [a])
      | [a;b] -> (match go_from a (path @ [a]) with
                  | None -> go_from b (path @ [b])
                  | Some path' -> Some path'
                 )
      | _ -> failwith "No way to move 3 ways"
  in go_from (L,L,L,L) [ (L,L,L,L) ]

  
(* Here is a solution that raises an exception when we've found a safe
   sequence of moves.  It then stops.
 *)
exception FoundPath of state list

let crossing_v2 () : state list option = 
  let rec go_from (state: state) (path: state list) : unit =
    if final state
    then raise (FoundPath path)
    else
      match List.filter (is_not_elem path) (moves state) with
      | [] -> ()
      | [a] -> go_from a (path @ [a])
      | [a;b] -> go_from a (path @ [a]);
                 go_from b (path @ [b])
      | [a;b;c] -> go_from a (path @ [a]);
                   go_from b (path @ [b]);
                   go_from c (path @ [c])
      | _ -> failwith "No way to move 4 ways"
  in try go_from (L,L,L,L) [ (L,L,L,L) ]; None with
     | FoundPath p -> Some p

(* Another that raises exceptions to keep looking for solutions. *)
exception KeepLooking

let crossing_v3 () : state list option =
  let rec go_from (state: state) (path: state list) : state list =
    if final state
    then path
    else
      match List.filter (is_not_elem path) (moves state) with
      | [] -> raise KeepLooking
      | [a] -> go_from a (path @ [a])
      | [a;b] -> (try go_from a (path @ [a]) with
                 | KeepLooking -> go_from b (path @ [b])
                 )
      | [a;b;c] -> (try go_from a (path @ [a]) with
                    | KeepLooking -> try go_from b (path @ [b]) with
                                     | KeepLooking -> go_from c (path @ [c])
                   )
      | _ -> failwith "No way to move 4 ways"
  in try Some (go_from (L,L,L,L) [ (L,L,L,L) ]) with
     | KeepLooking -> None



(* What if we don't know how many moves can be made?  Or if the number
   of moves is large enough that the above pattern becomes even more 
   tedious? *)

let crossing_many_possible_moves () : state list option =
  let rec go_from (state: state) (path: state list) : unit =
    if final state
    then raise (FoundPath path)
    else
      let valid_moves = List.filter (is_not_elem path) (moves state)
      in
      let go_func () (m: state) : unit = go_from m (path @ [m])
      in
      List.fold_left go_func () valid_moves

  in try go_from (L,L,L,L) [ (L,L,L,L) ]; None with
     | FoundPath p -> Some p

                         

(* List.fold_left works fine, but there is a more specific function,
   List.iter: ('a -> unit) -> 'a list -> unit
   that more specifically fits this problem.
 *)
let crossing_many_possible_moves' () = 
  let rec go_from (state: state) (path: state list) : unit =
    if final state
    then raise (FoundPath path)
    else
      let valid_moves = List.filter (is_not_elem path) (moves state)
      in
      let go_func (m: state) : unit = go_from m (path @ [m])
      in
      List.iter go_func valid_moves

  in try go_from (L,L,L,L) [ (L,L,L,L) ]; None with
     | FoundPath p -> Some p


let crossing_continuation (succ: state list -> 'a) : 'a = 
  let rec go_from (state: state) (path: state list) =
    if final state
    then succ path 
    else
      match List.filter (is_not_elem path) (moves state) with
      | [] -> raise KeepLooking
      | [a] -> go_from a (path @ [a])
      | [a;b] -> (try go_from a (path @ [a]) with
                  | KeepLooking -> go_from b (path @ [b])
      )
      | [a;b;c] -> (try go_from a (path @ [a]) with
                    | KeepLooking -> try go_from b (path @ [b]) with
                                     | KeepLooking -> go_from c (path @ [c])      
                                               )
      | _ -> failwith "No way to move 3 ways"
  in go_from (L,L,L,L) [ (L,L,L,L) ]


 (* [(L, L, L, L); (R, L, R, L); (L, L, R, L); (R, L, R, R); 
 (L, L, L, R); (R, R, L, R); (L, R, L, R); (R, R, R, R)] *)

let crossing_all () : state list list = 
  let r = ref []
  in
  try
  crossing_continuation 
    (fun s -> 
      (r := s :: !r);
      raise KeepLooking
    )
  with _ -> !r



  
