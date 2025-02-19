module type LazeeSig = sig
  type 'a t
  val delay: (unit -> 'a) -> 'a t
  val demand: 'a t -> 'a 
end

module Lazee_v1 : LazeeSig = struct

  type 'a hidden = Value of 'a
               | Thunk of (unit -> 'a)

  type 'a t = 'a hidden ref
  
let delay (unit_to_x: unit -> 'a) : 'a t = ref ( Thunk unit_to_x )

let force (l: 'a t) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let demand (l: 'a t) : 'a =
  force l;
  match !l with
  | Value v -> v
  | Thunk _ -> failwith "This should not happen."

end

module Lazee_v2 : LazeeSig = struct
  type 'a t = unit -> 'a
  let delay (unit_to_x: unit -> 'a) : 'a t = unit_to_x
  let demand (l: 'a t) : 'a = l ()
end