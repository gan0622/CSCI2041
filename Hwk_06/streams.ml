(* Constructing lazy values in OCaml, using them in streams *)

let print (n:int) = print_string "Your number is ";
                    print_int n;
                    print_endline "!"

let delayed_print (n:int) = fun () -> print n                      

type 'a hidden = Value of 'a
               | Thunk of (unit -> 'a)

type 'a lazee = 'a hidden ref

let delay (unit_to_x: unit -> 'a) : 'a lazee =
  ref ( Thunk unit_to_x )

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let demand (l: 'a lazee) : 'a =
  force l;
  match !l with
  | Value v -> v
  | Thunk _ -> failwith "This should not happen."

type 'a stream = Cons of 'a * 'a stream lazee

let rec from n =
  (* print_endline ("multiplying " ^ string_of_int n ^ " and " ^ string_of_int n); *)
  Cons (n, delay (fun () -> from (n+1) ) )

let nats = from 1

let head (s: 'a stream) : 'a = match s with
    | Cons (h, _) -> h
let tail (s: 'a stream) : 'a stream = match s with
    | Cons (_, t) -> demand t
                   
let rec take (n: int) (s: 'a stream) : 'a list =
  match n with
  | 0 -> []
  | _ -> (match s with
          | Cons (h, t) -> h :: take (n-1) (demand t)
         )

let rec cubes_from (n: int) : int stream = Cons (n*n*n, delay (fun () -> cubes_from (n+1)) )

    
let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (h, t) -> Cons (f h, delay (fun () -> map f (demand t) ) )

  
let cubes_from_map (n: int) : int stream = map(fun x -> x * x * x) (from n)


let rec zip (f: 'a -> 'b ->'c) (s1: 'a stream) (s2: 'b stream) :
'c stream =
  match s1, s2 with
  | Cons (h1, t1), Cons (h2, t2) ->
      Cons (f h1 h2, delay (fun () -> zip f (demand t1) (demand t2)))

let cubes_from_zip (n: int) : int stream = zip(fun h1 h2 -> h1 * h2 * h1)(from n)(from n)


let mul_p (x:int) (y:int) = print_endline ("multiplying " ^ string_of_int x ^ " and " ^
string_of_int y);
    x * y

let rec factorial () : int stream =
    Cons (1, delay (fun () -> zip mul_p nats (factorial ()) ) )

let fact = factorial ()

let facts' = 
  let dummy = ref nats
  in
  let mul = Cons(1, delay(fun () -> zip mul_p nats (!dummy)))
  in
  let () = dummy := mul 
  in
  mul 

(*
In fact function, we keep calling the fact function at the end which will reuse what we've done, that means,
we start from 1 over and over again.

we are doing for second one is building up a circular data structure so that whenever we evaluate the new value,
we step forward and start at the next value instead of calling the function and restatring everything again 

*)

let rec filter (f: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (h, t) ->
     let rest = delay (fun () -> filter f (demand t)) in
     if f h
     then Cons (h, rest)
     else demand rest

let not_modable (a:int) (b:int) : bool = not (b mod a = 0) (* num % 2 == 0 then it is not primes number *)

let sift (n:int)(s:int stream): int stream  = filter (not_modable n) s
(* let rec sift(n:int)(s:int stream):int stream = 
    match s with
    |Cons(h,t) -> if h mod n = 0 then sift n (demand t)
                  else Cons(h,delay(fun () -> sift n (demand t))) *)


let rec sieve(n:int stream) : int stream =  
  match n with
  | Cons(h,t) -> Cons (h, delay (fun () -> sieve (sift h (demand t) ) ) )

let primes = sieve (from 2)

let () =
  print_string "Testing part 1 ... " ;
  try
    assert (take 5 (cubes_from 1) = [1; 8; 27; 64; 125]);
    assert (take 5 (cubes_from_map 1) = [1; 8; 27; 64; 125]);
    assert (take 5 (cubes_from_zip 1) = [1; 8; 27; 64; 125]);
    assert (take 3 (cubes_from 3) = [27; 64; 125]);
    assert (take 3 (cubes_from_map 3) = [27; 64; 125]);
    assert (take 3 (cubes_from_zip 3) = [27; 64; 125]);
    assert (take 10 primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29] );
    assert (take 5 primes = [2; 3; 5; 7; 11]);
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg


