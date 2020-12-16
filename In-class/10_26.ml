type expr = 
  |Int of int 
  |Add of expr * expr
  |Mul of expr * expr
  |Sub of expr * expr
  |Div of expr * expr
  |Let of (n, e1, e2)
  |Id n -> lookup env n


let e1 = Add (Mul(Int 2, Int 4), Int 5)

let e2 = Let("x", Int 4, Add(Int 4, Int 5))

let e3 = Div(Sub(Int 14, Int 3), Int 5)

let rec lookup(env:(string * int) list)(n: string) : int =
    match env with
    |[] -> failwith("The name" ^ n ^ "is not bound by a let.")
    |(n',v) :: rest -> when n' == n -> val
    |(_,_) :: rest -> lookup n rest

(* A sample evaluation of `eval`
 "let x = 3 in (let y = x + 2 in y - x) * (let z = x + 3 in z / x)" 
  eval [] e4
= eval [("x", 3)] "(let y = x + 2 in y - x) * (let z = x + 3 in z / x)"
= eval [("x", 3)] "(let y = x + 2 in y - x)"   *
  eval [("x", 3)] "(let z = x + 3 in z / x)"
= eval [("y", 5), ("x", 3)] "y - x"   *
  eval [("z", 6), ("x", 3)] "z / x"
= 2 * 2
= 4
*)


let rec eval(e: expr) : int =  match e with
  |Int i -> i
  |Add(e1, e2) -> eval env e1 + eval env e2
  |Mul(e1, e2) -> eval env e1 * eval env e2
  |Sub(e1, e2) -> eval env e1 - eval env e2
  |Div(e1, e2) -> eval env e1 / eval env e2


(*
5 * 6

    *
    /\
  5   6

3+4* 5

    +
    /\
    * 3
    /\
    5 4

(2+4) * 5

      *
      /\
      + 5
      /\
      2 4s
*)


type value 
  = Int of int
  | Bool of bool

type expr 
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr 

  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr
  | Not of expr

  | Let of string * expr * expr
  | Id of string

type typ
  = Int_type
  | Bool_type

type result
  = OK of typ
  | Errs of (expr * string) list

(* infer types 
  1 + 3   : integer 
  false && (1 > 3) : bool
  let x = 4 in x + 5 : int
  let x = 4 in x && false :  [ (x && false, "bools required for &&"  ) ]
     or as we implemented [ (x, "bool value expected, got an integer" ) ]
 *)

let expect_Int (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK Int_type -> []
  | OK Bool_type ->  [(e, "expected Int, got Bool") ]
  | Errs errs -> errs

type typ_env = (string * typ option) list
             
let rec lookup_typ (x: string)  (t_env: typ_env) : result =
  match t_env with
  | [] -> Errs ( [(Id x, "identifier not bound")] )
  | (y, Some ty)::ys -> if x = y then OK ty else lookup_typ x ys
                
let rec type_check (e: expr) (t_env: typ_env) : result =
  match e with
  | Val (Int _) -> OK Int_type
  | Val (Bool _) -> OK Bool_type
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) ->
     ( match type_check e1 t_env, type_check e2 t_env with
       | OK Int_type, OK Int_type -> OK Int_type
       | r1, r2 -> Errs (expect_Int r1 e1 @ expect_Int r2 e2)
     )
  | Id x -> lookup_typ x t_env
  | Let (x, e1, e2) -> failwith "finish this"

let check (e: expr) = type_check e []
                    
(* Some sample expressions
  "let x = 4 + false in x + 5"
  "let x = 4 in y + 3"
*)


let free_vars (e: expr) : string list = 
 let rec fv (e:expr) (bound_vars: string list) : string list =
   match e with
   | Val (Int _) -> []
   | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) 
     -> fv e1 bound_vars @ fv e2 bound_vars
   | Let (x, e1, e2) -> fv e1 bound_vars @ fv e2 (x::bound_vars) 
   | Id x -> if List.mem x bound_vars then [] else [x]
   | _ -> failwith "finish free_vars"
 in
 fv e []
        

(* Some sample expressions and their values *)
let e1 = Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))
let v1 = eval e1

let e2 = Sub (Val (Int 10), Div (e1, Val (Int 2)))
let v2 = eval e2

let e3 = Eq (e1, e2)
let e4 = Lt (e1, e2)

let e5 = Not e4

(* ``let y = 5 in let x = y + 5 in x + y'' *)
let e6 = Let ("y", 
              Val (Int 5), 
              Let ("x", 
                   Add (Id "y", Val (Int 5)), 
                   Add (Id "x", Id "y")
                  )
             )

(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e7 = Let ("x",
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "x",
                   Let ("x",
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )

(* Assert expressions to test the evaluate function. *)
let () =
  assert (evaluate e1 = Int 7);
  assert (evaluate e2 = Int 7);
  assert (evaluate e3 = Bool true);
  assert (evaluate e4 = Bool false);
  assert (evaluate e5 = Bool true);
(*
  assert (evaluate e6 = Int 15);
  assert (evaluate e7 = Bool true);
 *)
  print_endline ("Success! All tests passed.")