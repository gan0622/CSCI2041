open LazeeModules

module type StreamSig = sig

  type 'a lazee
  val delay: (unit -> 'a) -> 'a lazee
  val demand: 'a lazee -> 'a 

  type 'a t = Cons of 'a * 'a t lazee
  val head: 'a t -> 'a 
  val tail: 'a t -> 'a t
  val take: int -> 'a t -> 'a list
  val filter: ('a -> bool) -> 'a t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t 
  val zip: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t 
  (* Add more elements here. *)
end

module Stream (L: LazeeSig) : StreamSig = struct
  type 'a lazee = 'a L.t
  let delay = L.delay
  let demand = L.demand
  type 'a t = Cons of 'a * 'a t lazee
  (* add more elements here *)

  let head (s: 'a t) : 'a  = match s with
    | Cons (h, _) -> h

  let tail (s: 'a t) : 'a t = match s with
    | Cons (_, t) -> demand t
  
  let rec take (n: int) (s: 'a t) : 'a list =
    match n with
    | 0 -> []
    | _ -> (match s with
            | Cons (h, t) -> h :: take (n-1) (demand t)
            )

  let rec filter (f: 'a -> bool) (s: 'a t) : 'a t =
    match s with
    | Cons (h, t) ->
      let rest = delay (fun () -> filter f (demand t)) in
      if f h
      then Cons (h, rest)
      else demand rest
  
  let rec map (f: 'a -> 'b) (s: 'a t) : 'b t =
    match s with
    | Cons (h, t) -> Cons (f h, delay (fun () -> map f (demand t) ) )
  
  let rec zip (f: 'a -> 'b ->'c) (s1: 'a t) (s2: 'b t) :
          'c t =
  match s1, s2 with
  | Cons (h1, t1), Cons (h2, t2) ->
     Cons (f h1 h2, delay (fun () -> zip f (demand t1) (demand t2)))

 
end

module Stream_Lazy = Stream(Lazee_v1)
module Stream_Slow = Stream(Lazee_v2)