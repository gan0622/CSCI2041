(* Hwk 05.  Extend the construts below as specified.
 *) 

 type value 
 = Int of int
 | Bool of bool
 | Closure of string * expr * value_environment
 | Ref of value ref
 | PairV of value * value
 | ListV of value list
and value_environment = (string * value) list
                              
and expr 
 = Val of value

 | Add of expr * expr
 | Sub of expr * expr
 | Mul of expr * expr
 | Div of expr * expr 

 | Lt  of expr * expr
 | Eq  of expr * expr
 | And of expr * expr
 | Not of expr

 | Let of string * typ * expr * expr
 | Id  of string

 | App of expr * expr
 | Lam of string * typ * expr

 | LetRec of string * typ * expr * expr
 | If of expr * expr * expr

 | Pair of expr * expr
 | Fst of expr
 | Snd of expr

 | Nil 
 | Cons of expr * expr
 | Head of expr
 | Tail of expr
and typ = Int_type 
       | Bool_type
       | Func_type of typ * typ
       | Pair_type of typ * typ
       | List_type of typ
       | Nil_type 
type type_environment = (string * typ option) list 


let rec check_type (t: typ) : string = 
     match t with
     | Int_type -> ("int")
     | Bool_type -> ("bool")
     | Func_type(a,b) -> "(" ^ check_type a ^ " -> " ^ check_type b ^ ")"
     | Pair_type(t1,t2) -> "(" ^ check_type t1 ^ " * " ^ check_type t2 ^ ")"
     | List_type t1 -> "(" ^ check_type t1 ^ " list)"
     | Nil_type -> ("list")

let show_list (show:'a -> string) (lst: 'a list) : string  = String.concat ";" (List.map show lst)

let rec unparse_list_type (e: value): string = 
     match e with 
     | (Int i) -> string_of_int i
     | (Bool b) -> string_of_bool b
     | (Closure (_,_,_)) -> "<fun>"
     | (Ref _) -> "reference" 
     (* | (ListV l) -> (match l with
                    | [] -> "[]"
                    | (x :: []) -> "[" ^ unparse_list_type x ^ "]"
                    | (x :: xs) -> "[" ^ unparse_list_type x ^ ";" ^ unparse_list_type xs ^ "]" 
                    ) *)
     | ListV l -> "[" ^ show_list unparse_list_type l ^ "]"
     | (PairV (v1,v2)) -> "(" ^ unparse_list_type v1 ^ " , " ^ unparse_list_type v2 ^ ")"
(* Part 1. Complete unparse *)
let rec unparse (e: expr) : string =
 match e with  
 | Val a -> unparse_list_type a
 (* | Val PairV(a,b) -> string_of_int *)
 | Add(e1, e2) -> "(" ^ unparse e1 ^" + " ^unparse e2 ^")"
 | Sub(e1, e2) -> "(" ^ unparse e1 ^" - " ^unparse e2 ^")"
 | Mul(e1, e2) -> "(" ^ unparse e1 ^" * " ^unparse e2 ^")"
 | Div(e1, e2) -> "(" ^ unparse e1 ^" / " ^unparse e2 ^")"
 | Lt(e1, e2) -> "(" ^ unparse e1 ^" < " ^unparse e2 ^")"
 | Eq(e1, e2) -> "(" ^ unparse e1 ^" = " ^unparse e2 ^")"
 | And(e1, e2) -> "(" ^ unparse e1 ^" && " ^unparse e2 ^")"
 | Not e -> "( not "^ unparse e ^ ")"
 | Let(s, typ, e1, e2) -> "(let " ^ s ^" : "^ check_type typ ^ " = " ^ unparse e1 ^ " in " ^ unparse e2^ ")" 
 | Id x -> x 
 | App(e1,e2) -> "(" ^ unparse e1 ^ " " ^ unparse e2 ^ ")"
 | Lam(x,typ, e1) -> "(fun (" ^ x ^ ": " ^ (check_type typ) ^ ")" ^ " -> " ^ unparse e1 ^ ")"
 | LetRec(s,typ, e1,e2) -> "(let rec " ^ s ^ " : " ^ (check_type typ) ^ " = " ^ unparse e1 ^ " in " ^ unparse e2 ^ ")"
 | If(e1,e2,e3) -> "(if " ^ unparse e1 ^ " then " ^ unparse e2 ^ " else " ^ unparse e3 ^ ")"
 | Pair(e1, e2) ->"(" ^ unparse e1 ^ ", " ^ unparse e2 ^ ")"
 | Fst e -> "(fst " ^ unparse e ^ ")"
 | Snd e -> "(snd " ^ unparse e ^ ")"
 | Nil -> "[]"
 | Cons(e1, e2) -> "(" ^ unparse e1 ^ " :: " ^ unparse e2 ^ ")"
 | Head e -> "(List.hd " ^ "(" ^ unparse e ^ "))"
 | Tail e -> "(List.tl " ^ "(" ^ unparse e ^ "))"
(* Part 2. Complete freevars *)       
let rec freevars (e: expr) : string list =
 match e with
 | Val _ -> []
 | Id x -> [x]
 | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2)
 | Lt (e1, e2) | Eq (e1, e2) | And (e1, e2) | App (e1,e2)
   -> freevars e1 @ freevars e2
 | Not e -> freevars e
 | If (e1,e2,e3) -> freevars e1 @ freevars e2 @ freevars e3
 | Let (s, _ , e1, e2) ->  freevars e1 @ List.filter(fun f -> f <> s) (freevars e2) 
 | LetRec(s, _ , e1,e2) -> List.filter(fun f -> f <> s) (freevars e1 @ (freevars e2))
 | Lam(x,_, e1) -> List.filter(fun f -> f <> x)(freevars e1)
 | Pair(e1, e2) -> freevars e1 @ freevars e2
 | Fst e -> freevars e 
 | Snd e -> freevars e 
 | Nil -> []
 | Cons(e1,e2) -> freevars e1 @ freevars e2
 | Head e -> freevars e 
 | Tail e -> freevars e     


(* Part 3. Type checking             *)
type result = OK of typ
           | Errs of (expr * string) list

let expect_Int (r: result) (e: expr) : (expr * string) list =
    match r with
    | OK Int_type -> []
    | OK Bool_type ->  [(e, "expected Int Bool") ]
    | Errs errs -> errs

let expect_Bool (r: result) (e: expr) : (expr * string) list =
    match r with
    | OK Bool_type -> []
    | OK Int_type ->  [(e, "expected Bool Int") ]
    | Errs errs -> errs    

let rec lookup_typ (x: string)  (t_env: type_environment) : result =
    match t_env with
    | [] -> Errs ( [(Id x, "identifier not bound")] )
    | (y, Some ty)::ys -> if x = y then OK ty else lookup_typ x ys
    | _ -> failwith "UNREACHABLE"

let rec type_check (e:expr) (env: type_environment) : result =
 match e with
 | Val (Int _) -> OK Int_type
 | Val (Bool _) -> OK Bool_type
 | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) ->
     ( match type_check e1 env, type_check e2 env with
       | OK Int_type, OK Int_type -> OK Int_type
       | r1, r2 -> Errs (expect_Int r1 e1 @ expect_Int r2 e2)
     )
 | Lt(e1, e2) | Eq(e1, e2)->
     ( match type_check e1 env, type_check e2 env with
       | OK Int_type, OK Int_type -> OK Bool_type
       | OK (List_type t1), OK (List_type t2 )-> OK  Bool_type
       | r1, r2 -> Errs (expect_Int r1 e1 @ expect_Int r2 e2)
     )
 | And (e1, e2) ->
     ( match type_check e1 env, type_check e2 env with
       | OK Bool_type, OK Bool_type -> OK Bool_type
       | r1, r2 -> Errs (expect_Bool r1 e1 @ expect_Bool r2 e2)
     )
 | Not e1 ->
     (match type_check e1 env with
       | OK Bool_type -> OK Bool_type
       | r1 -> Errs (expect_Bool r1 e1)
     )
 | Let (n, x, e1, e2) ->
     let r1 = type_check e1 env
     in
     (match r1 with
     | Errs errs -> Errs errs
     | OK ty -> if ty = x then type_check e2 ( (n,Some x) :: env )
                else Errs [(Let (n,x,e1, e2),"type mismatch in the first expression")])

  | Id n -> lookup_typ n env

  | App(e1, e2) -> 
    let r1 = type_check e1 env
    in
    let r2 = type_check e2 env
    in
    (match r1 with
    | Errs errs -> Errs errs
    | OK Func_type(ty1, ty2) -> (match r2 with
                                | OK ty3 -> if ty1 = ty3 then OK ty2 
                                            else Errs [(App(e1 , e2),"type mismatch in then and else branches of if")]
                                | Errs errs -> Errs errs
                                )
    | OK ty -> failwith "UNREACHABLE"
    )
  | Lam(x,t,e1) ->       
     let r1 = type_check e1 ((x, Some t) :: env)
     in 
     (match r1 with 
     | Errs errs -> Errs errs
     | OK ty -> OK (Func_type (t, ty)))

  | LetRec(x, t, e1, e2) ->
     let r1 = type_check e1 ((x, Some t) :: env)           
     in 
     (match r1 with 
     | Errs errs -> Errs errs
     | OK ty -> if ty = t then type_check e2 ((x, Some t) :: env)   
                else Errs [(LetRec (x, t, e1, e2), "type mismatch the first part of expression")])
  (* | If(e1, e2, e3) -> (match type_check e1 env, type_check e2 env, type_check e3 env with
                      | OK ty1, OK ty2, OK ty3 -> if ty1 = ty2 = ty3 = Bool_type then OK ty1
                                                  else if ty2 = ty3 then Errs (expected_Bool ty1 e1)
                                                  else Errs[(If(e1, e2, e3), "type mismatch in then and else branches of if")]
                      | OK ty1, OK ty2, Errs t *)
   | If(e1,e2,e3) -> 
      let r1 = type_check e1 env
      in
      (match r1 with
     | Errs errs -> Errs errs
     | OK Bool_type -> (match type_check e2 env, type_check e3 env with
                       | Errs er, Errs err -> Errs (er @ err)
                       | OK ty, OK ty2 -> if ty = ty2 then OK ty 
                                          else Errs[(If(e1, e2, e3), "type mismatch in then and else branches of if")]
                       | Errs er, OK ty2 -> Errs er
                       | OK ty, Errs err -> Errs err 
                       )
      | OK ty -> Errs[(e1, "expected bool type")]                          
      )
   | Pair(e1, e2) ->
     let r1 = type_check e1 env
     in
     let r2 = type_check e2 env
     in
     (match r1, r2 with
          | Errs errs, _ -> Errs [Pair(e1,e2),"expect pair type"]
          | OK ty1, OK ty2 -> OK (Pair_type(ty1,ty2))
          | _ , Errs errs -> Errs [Pair(e1,e2),"expect pair type"]
     )
   | Fst e -> (match type_check e env with
               | Errs errs -> Errs errs
               | OK Pair_type(ty1, ty2) -> OK ty1
               | _ -> Errs [(Fst e,"expected Pair type")])
   | Snd e -> (match type_check e env with
               | Errs errs -> Errs errs
               | OK Pair_type(ty1, ty2) -> OK ty2
               | _ -> Errs [(Snd e,"expected Pair type")])
   | Nil -> OK (List_type Nil_type)
   | Cons(e1, e2) ->
    (match type_check e1 env, type_check e2 env with 
    | OK ty1, OK (List_type ty2) -> if ty1 = ty2 || ty2 = Nil_type then OK (List_type ty1)
                                    else Errs[(e,"cannot operate")]
    | OK ty1, OK ty2 -> Errs[(e,"cannot operate")]   
    | Errs e1, _ -> Errs e1
    | _, Errs e2 -> Errs e2                        
    )
   

   | Head e -> (match type_check e env with
               | Errs errs -> Errs errs
               | OK (List_type t1 ) -> OK t1
               | _ -> Errs[(e,"cannot operate")]
               )
   | Tail e -> 
               (match type_check e env with
               | Errs errs -> Errs errs
               | OK (List_type t1) -> OK (List_type t1)
               | _ -> Errs[(e,"cannot operate")]
               )
   
     
      
let check e = type_check e [] 

(*Credit to professor Van Wyk*)
let rec lookup_val (e:string)  (env: value_environment) : value =
     match env with
     | [] -> raise (Failure ("identifier not found") )
     | (y, ty)::ys -> if e = y then ty else lookup_val e ys
(* Part 4. Evaluation *)

let rec eval (env: value_environment) (e:expr) : value =
 match e with
 | Val v -> v
 | Add (e1, e2) ->
    ( match eval env e1, eval env e2 with
      | Int v1, Int v2 -> Int (v1 + v2)
      | _ -> raise (Failure "incompatible types, Add")
    )
 | Sub (e1, e2) ->
    ( match eval env e1, eval env e2 with
      | Int v1, Int v2 -> Int (v1 - v2)
      | _ -> raise (Failure "incompatible types, Sub")
    )
 | Mul (e1, e2) ->
    ( match eval env e1, eval env e2 with
      | Int v1, Int v2 -> Int (v1 * v2)
      | _ -> raise (Failure "incompatible types, Mul")
    )
 | Div (e1, e2) ->
    ( match eval env e1, eval env e2 with
      | Int v1, Int v2 -> Int (v1 / v2)
      | _ -> raise (Failure "incompatible types, Div")
    )
 | Lt (e1, e2) -> 
     ( match eval env e1, eval env e2 with
          | Int v1, Int v2 -> Bool (v1 < v2)
          | _ -> raise (Failure "incompatible types, Lt")
     )
 | Eq (e1, e2) -> 
     ( match eval env e1, eval env e2 with
          | Int v1, Int v2 -> Bool (v1 = v2)
          | Bool b1, Bool b2 -> Bool (b1 = b2)
          | PairV (e1,e2), PairV (e3,e4) -> Bool (e1 = e3 && e2 = e4)
          | ListV l1, ListV l2 -> Bool (l1 = l2)
          | _ -> raise (Failure "incompatible types, Eq")
     )
 | And (e1, e2) -> 
     ( match eval env e1, eval env e2 with
          | Bool b1, Bool b2 -> Bool (b1 && b2)
          | _ -> raise (Failure "incompatible types, And")
     )
 | Not e ->
     (match eval env e with
          | Bool b -> Bool (not b)
          | _ -> raise (Failure "incompatible types, Not")
     )
 | If (e1,e2,e3) -> 
     (match eval env e1 with
          | Bool b -> if b = true then eval env e2 else eval env e3
          | _ -> raise (Failure "incompatible types, If")
     )
 | Let (n, t, dexpr, body) ->
     let v = eval env dexpr in
     eval ( (n,v)::env ) body
 | Lam (x, _, e1) -> Closure(x, e1, env)
 | App(e1,e2) ->
     let v = match eval env e1 with
             |  Ref var -> !var 
             |  var -> var
     in
     let v2 = eval env e2 
     in
     (match v with 
             | Closure (str, body, env) -> eval ((str,v2) :: env) body 
             | _ -> raise (Failure "Error!")
     )
 | Id x -> lookup_val x env
 | LetRec(s, _ , e1,e2) -> (*Name, value of given to that name, evaluate with that information about that biding *)
     (* (match e1 with *)
                    let recRef = ref (Int 999) 
                    in
                    let c = eval((s, Ref recRef) :: env) e1 (*extract biding with the value of e1 *)
                    in
                    let () = recRef := c (*point to the value in C instead of dummy value 999*)
                    in
                    eval ((s, c) :: env) e2
          (* |_ -> raise (Failure "Error!") *)
     (* )     *)
  | Pair(e1, e2) -> PairV (eval env e1, eval env e2)
  | Fst e1 -> (match eval env e1 with
              | PairV (v,_) -> v
              | _ -> failwith "incompetable type"
  )  
  | Snd e2 -> (match eval env e2 with
              | PairV (_,v) -> v
              | _ -> failwith "incompetable type"
  )
  | Nil -> ListV []
  | Cons(e1, e2) -> (match eval env e1, eval env e2 with
     | hd, ListV lst-> ListV (hd :: lst)
     | _ -> failwith "incompetable type"
  ) 
  | Head e1 -> (match eval env e1 with
               | ListV (hd :: lst)-> hd 
               | _ -> failwith "incompetable type"
               )
  | Tail e2 ->  (match eval env e2 with
               | ListV (hd :: lst)-> ListV lst
               | _ -> failwith "incompetable type"
  )
let evaluate e = eval [] e

let inc = Lam ("n", Int_type, Add(Id "n", Val (Int 1)))

let add = Lam ("x", Int_type,
               Lam ("y", Int_type, Add (Id "x", Id "y"))
              ) 

(* some sample expressions *)

let a1 = Pair (Val (Int 3), Val (Bool false))
let a2 = Pair (Add (Val (Int 3), Val (Int 4)), Val (Bool false))
let a3 = Let ("p", Pair_type (Int_type, Int_type), Pair (Val (Int 3), Val (Int 4)), Add (Fst (Id "p"), Snd (Id "p")))
let a4 = Fst (Val (Int 3))
let a5 = Snd (Val (Int 3))
let a6 = Cons (Add (Val (Int 3), Val (Int 4)), Nil)
let a7 = Let ("lst", List_type (Int_type), Cons (Val (Int 3), Cons (Val (Int 4), Nil)), Id "lst")
let a8 = Eq (Nil, Nil)
let a9 = Eq (Cons (Val (Int 3), Nil), Nil)
let a10 = Head (Cons (Val (Int 3), Nil))
let a11 = Tail (Cons (Val (Int 3), Nil))
let a12 = Tail (Cons (Val (Int 3), Cons (Val (Int 4), Nil)))
let a13 = LetRec ("sum", Func_type (List_type (Int_type), Int_type),Lam ("lst", List_type (Int_type), If (Eq (Id "lst", Nil), Val (Int 0), Add (Head (Id "lst"), App (Id "sum", Tail (Id "lst"))))), App (Id "sum", Cons (Val (Int 1), Cons (Val (Int 2), Cons (Val (Int 3), Nil)))))
(*--------------------------------------------------*)
let e1 = Add (Val (Int 3), Val (Int 5))
let e2 = Add (Val (Int 3), Val (Bool true))
let e3 = Mul (Val (Bool true), Val (Int 5))
let e4 = Add (e2, e3)

let e5 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
             Add (Id "x", Val (Int 5))
          )
      
let e6 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
             Lt (Id "x", Val (Int 5))
          )
      
(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e7 = Let ("x", Bool_type,
             Lt (Val (Int 3), Val (Int 5)),
             And (Id "x",
                  Let ("x", Int_type,
                       Add (Val (Int 1), Val (Int 2)),
                       Eq (Id "x", Val (Int 3))
                      )
                 )
            )


(* ``let x = 3 < 5 in y && let x = 1 + 2 in x = 3 *)
let e8 = Let ("x", Bool_type,
             Lt (Val (Int 3), Val (Int 5)),
             And (Id "y",
                  Let ("x", Int_type,
                       Add (Val (Int 1), Val (Int 2)),
                       Eq (Id "x", Val (Int 3))
                      )
                 )
            )
let e9 = Let ("inc", Func_type (Int_type, Int_type), Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Int 3)))

let e10 = If (Eq (Val (Int 4), Val (Int 0)), Val (Int 0), Val (Int 1)) 

let e11 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)), And (Id "x", Val (Bool true)))

let e12 = Let ("x", Int_type, Add (Id "x", Val (Int 4)), And (Id "y", Val (Bool true)))

let e13 = Let ("x", Bool_type, Mul (Val (Int 3), Val (Int 5)), Add (Id "x", Val (Int 3)))

let e14 = Let ("inc", Func_type (Int_type, Int_type), Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Bool true)))

let e15 = If (Val (Int 4), Val (Int 0), Val (Int 1))

let e16 = If (Eq (Val (Int 4), Val (Int 4)), Val (Int 0), Val (Bool false))

let e17 = LetRec ("sumToN", Func_type (Int_type, Int_type),Lam ("n", Int_type, If (Eq (Id "n", Val (Int 0)), Val (Int 0), Add (Id "n", App (Id "sumToN", Sub (Id "n", Val (Int 1)))))), Id "sumToN")

let e18 = Eq (Val (Bool false), Val (Bool true))

let err_1 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
                And (Id "x", Val (Bool true))
             )

let err_2 = Let ("x", Int_type, Add (Id "x", Val (Int 4)),
                And (Id "y", Val (Bool true))
             )

let inc_use = Let ("inc", Func_type (Int_type, Int_type), 
                  Lam ("n", Int_type, Add (Id "n", Val (Int 1))),
                  App (Id "inc", Val (Int 3))
               )

let sumToN : expr =
     LetRec ("sumToN", Func_type (Int_type, Int_type),
               Lam ("n", Int_type,
                    If (Eq (Id "n", Val (Int 0)),
                         Val (Int 0),
                         Add (Id "n", 
                              App (Id "sumToN", 
                                   Sub (Id "n", Val (Int 1))
                                   )
                              )
                         )
                    ),
               Id "sumToN"
               )
    

let sumTo3 = App (sumToN, Val (Int 4))


let() = 
print_string "Testing unparse ... " ;
try
     assert(unparse e11 = "(let x : int = (3 + 4) in (x && true))");
     assert(unparse e12 =  "(let x : int = (x + 4) in (y && true))");
     assert(unparse e13 = "(let x : bool = (3 * 5) in (x + 3))");
     assert(unparse e14 =  "(let inc : (int -> int) = (fun (n: int) -> (n + 1)) in (inc true))");
     assert(unparse e15 = "(if 4 then 0 else 1)" );
     assert(unparse e16 = "(if (4 = 4) then 0 else false)");
     assert(unparse e17 = "(let rec sumToN : (int -> int) = (fun (n: int) -> (if (n = 0) then 0 else (n + (sumToN (n - 1))))) in sumToN)");
     assert(unparse e18 = "(false = true)");
     assert(unparse a1 =  "(3, false)");
     assert(unparse a2 = "((3 + 4), false)");
     assert(unparse a3 = "(let p : (int * int) = (3, 4) in ((fst p) + (snd p)))");
     assert(unparse a4 = "(fst 3)");
     assert(unparse a5 = "(snd 3)");
     assert(unparse a6 = "((3 + 4) :: [])");
     assert(unparse a7 = "(let lst : (int list) = (3 :: (4 :: [])) in lst)");
     assert(unparse a8 =  "([] = [])");
     assert(unparse a9 =  "((3 :: []) = [])");
     assert(unparse a10 = "(List.hd ((3 :: [])))");
     assert(unparse a11 = "(List.tl ((3 :: [])))");
     assert(unparse a12 =  "(List.tl ((3 :: (4 :: []))))");
     assert(unparse a13 = "(let rec sum : ((int list) -> int) = (fun (lst: (int list)) -> (if (lst = []) then 0 else ((List.hd (lst)) + (sum (List.tl (lst)))))) in (sum (1 :: (2 :: (3 :: [])))))");
     print_string "tests passed.\n"
with
     Assert_failure (file, line, column) -> 
     let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                    ", column " ^ string_of_int column ^ "\n\n\n\n"
     in print_string msg

let() = 
     print_string "Testing evaluate ... " ;
     try
          assert(evaluate (Add (Val (Int 3), Val (Int 5))) = Int 8);
          assert(evaluate (Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), Add (Id "x", Val (Int 5)))) = Int 12);
          assert(evaluate (Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), Lt (Id "x", Val (Int 5)))) = Bool false);
          assert(evaluate (Let ("x", Bool_type,Lt (Val (Int 3), Val (Int 5)), And (Id "x", Let ("x", Int_type,Add (Val (Int 1), Val (Int 2)), Eq (Id "x", Val (Int 3)))))) = Bool true);
          assert(evaluate (Let ("inc", Func_type (Int_type, Int_type),Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Int 3)))) = Int 4);
          assert(evaluate (If (Eq (Val (Int 4), Val (Int 0)), Val (Int 0), Val (Int 1))) = Int 1);
          (* assert(evaluate (LetRec ("sumToN", Func_type (Int_type, Int_type),Lam ("n", Int_type, If (Eq (Id "n", Val (Int 0)), Val (Int 0), Add (Id "n", App (Id "sumToN", Sub (Id "n", Val (Int 1)))))), Id "sumToN")) = "<fun>"); *)
          assert(evaluate (App (sumToN, Val (Int 10)))= Int 55);
          assert(evaluate sumTo3 = Int 10);
          assert(evaluate a1 = PairV (Int 3, Bool false));
          assert(evaluate a2 = PairV (Int 7, Bool false));
          assert(evaluate a3 = Int 7);
          assert(evaluate a6 = ListV [Int 7]);
          assert(evaluate a7 = ListV [Int 3; Int 4]);
          assert(evaluate a8 = Bool true);
          assert(evaluate a9 = Bool false);
          assert(evaluate a10 = Int 3);
          assert(evaluate a11 = ListV []);
          assert(evaluate a12 = ListV [Int 4]);
          assert(evaluate a13 = Int 6);
          print_string "tests passed.\n"
     with
          Assert_failure (file, line, column) -> 
          let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                         ", column " ^ string_of_int column ^ "\n\n\n\n"
          in print_string msg
