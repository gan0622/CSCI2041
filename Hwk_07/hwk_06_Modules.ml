open LazeeModules
open StreamModules

module type Hwk_06_Sig = sig
  type 'a stream
  
  val take: int -> 'a stream -> 'a list
  val from: int -> int stream
  val nats: int stream
  val cubes_from: int -> int stream
  val cubes_from_zip: int -> int stream
  val cubes_from_map: int -> int stream
  val facts: int stream
  val facts': int stream
  val primes: int stream
end

module Hwk_06(S: StreamSig) : Hwk_06_Sig = struct
  (* add elements here to complete the functor *)
  type 'a stream = 'a S.t
  (* type 'a lazee = 'a S.t
  type 'a t = Cons of 'a * 'a t S.lazee *)
  let delay = S.delay
  let demand = S.demand
  let take = S.take
  let map = S.map

  let rec from n = S.Cons (n, delay (fun () -> from (n+1) ) )
  let nats = from 1 

  let rec cubes_from (n: int) : int stream = S.Cons (n*n*n, delay (fun () -> cubes_from (n+1)) )


  let cubes_from_zip (n : int) : int stream = S.zip(fun h1 h2 -> h1 * h2 * h1)(from n)(from n)

  let cubes_from_map (n: int) : int stream = S.map(fun x -> x * x * x) (from n)
  
  let mul_p (x:int) (y:int) = print_endline ("multiplying " ^ string_of_int x ^ " and " ^
string_of_int y);
    x * y

  let rec factorial () : int stream =
    S.Cons (1, S.delay (fun () -> S.zip mul_p nats (factorial ()) ) )

  let facts = factorial ()

  let facts' = 
  let dummy = ref nats
  in
  let mul = S.Cons(1, delay(fun () -> S.zip mul_p nats (!dummy)))
  in
  let () = dummy := mul 
  in
  mul
  

  let not_modable (a:int) (b:int) : bool = not (b mod a = 0) (* num % 2 == 0 then it is not primes number *)

  let sift (n:int)(s:int stream): int stream  = S.filter (not_modable n) s

  let rec sieve(n:int stream) : int stream =  
    match n with
    | S.Cons(h,t) -> Cons (h, delay (fun () -> sieve (sift h (demand t) ) ) )

  let primes = sieve(from 2)
end