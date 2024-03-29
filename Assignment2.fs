// T-501-FMAL, Spring 2021, Assignment 2

(*
STUDENT NAMES HERE: 
Bergur Tareq Tamimi Einarsson & Kristofer Gauti Þórhallsson
*)

module Assignment2


(* Various type and function definitions, do not edit *)
type iexpr =
    | IVar of string
    | INumI of int
    | INumF of float
    | IPlus of iexpr * iexpr
    | ITimes of iexpr * iexpr
    | INeg of iexpr
    | IIfPositive of iexpr * iexpr * iexpr

type expr =
    | Var of string
    | NumI of int
    | NumF of float
    | Plus of expr * expr
    | Times of expr * expr
    | Neg of expr
    | IfPositive of expr * expr * expr
    | IntToFloat of expr
    | Match of expr * string * expr * string * expr

type value =
    | I of int
    | F of float

type envir = (string * value) list

type typ =
    | Int
    | Float

type tyenvir = (string * typ) list

let rec lookup (x : string) (env : (string * 'a) list) : 'a =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env

 

let paren b s = if b then  "(" + s + ")" else s

let iprettyprint (e : iexpr) : string =
    let rec iprettyprint' e acc =
        match e with
        | IVar x -> x
        | INumI i -> string i
        | INumF f -> sprintf "%A" f
        | IPlus  (e1, e2) ->
              paren (4 <= acc) (iprettyprint' e1 3 + " + " + iprettyprint' e2 4)
        | ITimes (e1, e2) ->
              paren (7 <= acc) (iprettyprint' e1 6 + " * " + iprettyprint' e2 7)
        | INeg e ->
              paren (10 <= acc) ("-" + iprettyprint' e 9)
        | IIfPositive (e, et, ef) ->
              paren (2 <= acc) ("if " + iprettyprint' e 3 + " > 0 then " + iprettyprint' et 2 + " else " + iprettyprint' ef 1)
    iprettyprint' e 0

let prettyprint (e : expr) : string =
    let rec prettyprint' e acc =
        match e with
        | Var x -> x
        | NumI i -> string i
        | Plus  (e1, e2) ->
             paren (4 <= acc) (prettyprint' e1 3 + " + " + prettyprint' e2 4)
        | Times (e1, e2) ->
             paren (7 <= acc) (prettyprint' e1 6 + " * " + prettyprint' e2 7)
        | Neg e ->
             paren (10 <= acc) ("-" + prettyprint' e 9)
        | IfPositive (e, et, ef) ->
             paren (2 <= acc) ("if " + prettyprint' e 3 + " > 0 then " + prettyprint' et 2 + " else " + prettyprint' ef 1)
        | NumF f -> sprintf "%A" f
        | IntToFloat e ->
             paren (10 <= acc) ("float " + prettyprint' e 10)
        | Match (e, xi, ei, xf, ef) ->
             paren (2 <= acc) ("match " + prettyprint' e 1 + " with"
               + " I " + xi + " -> " + prettyprint' ei 2
               + " | F " + xf + " -> " + prettyprint' ef 1)
    prettyprint' e 0

let plus_value (v1 : value, v2 : value) : value =
    match v1, v2 with
    | I x1, I x2 -> I (x1 + x2)
    | F x1, I x2 -> F (x1 + float x2)
    | I x1, F x2 -> F (float x1 + x2)
    | F x1, F x2 -> F (x1 + x2)

let times_value (v1 : value, v2 : value) : value =
    match v1, v2 with
    | I x1, I x2 -> I (x1 * x2)
    | F x1, I x2 -> F (x1 * float x2)
    | I x1, F x2 -> F (float x1 * x2)
    | F x1, F x2 -> F (x1 * x2)

let neg_value (v : value) : value =
    match v with
    | I x -> I (-x)
    | F x -> F (-x)

let is_positive_value (v : value) : bool =
    match v with
    | I x -> x > 0
    | F x -> x > 0.

type rinstr =
    | RLoad of int            // load from environment
    | RStore                  // move value from top of stack to
                              // 0th pos of the environment,
                              // shifting all others down
    | RErase                  // remove 0th value from environment,
                              // shifting all others up
    | RNum of int
    | RAdd
    | RSub
    | RMul
    | RPop
    | RDup
    | RSwap

type rcode = rinstr list
type stack = int list          // intermediate values
type renvir = int list         // values of numbered variables

let rec reval (inss : rcode) (stk : stack) (renv : renvir) =
    match inss, stk with
    | [], i :: _ -> i
    | [], []     -> failwith "reval: No result on stack!"
    | RLoad n :: inss,             stk ->
          reval inss (List.item n renv :: stk) renv
    | RStore  :: inss,        i :: stk -> reval inss stk (i :: renv)
    | RErase  :: inss,             stk -> reval inss stk (List.tail renv)
    | RNum i  :: inss,             stk -> reval inss (i :: stk) renv
    | RAdd    :: inss, i2 :: i1 :: stk -> reval inss ((i1+i2) :: stk) renv
    | RSub    :: inss, i2 :: i1 :: stk -> reval inss ((i1-i2) :: stk) renv
    | RMul    :: inss, i2 :: i1 :: stk -> reval inss ((i1*i2) :: stk) renv
    | RPop    :: inss,        i :: stk -> reval inss stk renv
    | RDup    :: inss,        i :: stk -> reval inss ( i ::  i :: stk) renv
    | RSwap   :: inss, i2 :: i1 :: stk -> reval inss (i1 :: i2 :: stk) renv
    | _ -> failwith "reval: too few operands on stack"

// Problem 1
let rec lookup2 (x : string) (env : (string * 'a) list) : 'a =
    match env with
    | []          -> I 0
    | (y, v)::env -> if x = y then v else lookup2 x env

let rec ieval (e : iexpr) (env : envir) : value =
    match e with
    | IVar x -> lookup2 x env                     // to modify
    | INumI i -> I i
    | INumF f -> F f
    | IPlus (e1, e2) -> plus_value (ieval e1 env, ieval e2 env)
    | ITimes (e1, e2) -> times_value (ieval e1 env, ieval e2 env)
    | INeg e -> neg_value (ieval e env)
    | IIfPositive (e, et, ef) ->
        if is_positive_value (ieval e env)
        then ieval et env
        else ieval ef env

// Problem 2
let rec eval (e : expr) (env : envir) : value =
    match e with
    | Var x -> lookup2 x env // possibly have to change this to V2
    | NumI i -> I i
    | NumF f -> F f
    | Plus (e1, e2) ->                             // to complete
        match eval e1 env, eval e2 env with
        | I i1, I i2 -> I (i1 + i2)
        | F i1, F i2 -> F (i1 + i2)
        | _ -> failwith "wrong operand type"
    | Times (e1, e2) ->                            // to complete
        match eval e1 env, eval e2 env with
        | I i1, I i2 -> I (i1 * i2)
        | F i1, F i2 -> F (i1 * i2)
        | _ -> failwith "wrong operand type"
    | Neg e ->                                     // to complete
        match eval e env with
        | I i -> I (- i)
        | F i -> F (- i)
        | _ -> failwith "wrong operand type"
    | IntToFloat e ->
        match eval e env with
        | I i -> F (float i)
        | _ -> failwith "wrong operand type"
    | IfPositive (e, et, ef) -> 
        match eval e env, eval et env, eval ef env with
        | i1, I i2, I i3 -> if is_positive_value (i1) then I i2 else I i3
        | i1, F i2, F i3 -> if is_positive_value (i1) then F i2 else F i3
        | i1, I i2, F i3 -> if is_positive_value (i1) then I i2 else F i3
        | i1, F i2, I i3 -> if is_positive_value (i1) then F i2 else I i3
        | _ -> failwith "wrong operand type"
    | Match (e, xi, ei, xf, ef) -> 
        match eval e env with
        | I(e) -> eval ei ([xi, I(e)] @ env)
        | F(e) -> eval ef ([xf, F(e)] @ env)
        | _ -> failwith "wrong operand type"

// Problem 3
let to_float (v : value) : float =
    match v with
    | F x -> float x
    | I x -> float x

// Problem 4
let to_float_expr (e : expr) : expr =
    Match (e, "x", (IntToFloat (Var "x")), "y", Var "y")
 
let plus_expr (e1 : expr, e2 : expr) : expr = 
    Match(e1,
        "integer",Match(e2,
            "integer", Plus(e1,e2),
            "float", Plus(to_float_expr (e1), e2)
        ),
        "float", Plus(e1, to_float_expr e2)
    )


let times_expr (e1 : expr, e2 : expr) : expr = 
    Match(e1,
        "integer",Match(e2,
            "integer", Times(e1,e2),
            "float", Times(to_float_expr(e1), e2)
        ),
        "float", Times(e1, to_float_expr e2)
    )

// Problem 5
let rec add_matches (e : iexpr) : expr = 
    match e with
    | IVar e -> Var e
    | INumI e -> NumI e
    | INumF e -> NumF e
    | IPlus(e1, e2) -> plus_expr(add_matches e1, add_matches e2)
    | ITimes(e1, e2) -> times_expr(add_matches e1, add_matches e2)
    | INeg e -> Neg (add_matches e)
    | IIfPositive(e, e1, e2) -> IfPositive(add_matches e, add_matches e1, add_matches e2)

// Problem 6
let rec infer (e : expr) (tyenv : tyenvir) : typ =
    match e with
    | Var x -> lookup x tyenv
    | NumI i -> Int
    | NumF f -> Float
    | Plus (e1, e2) ->                    
        match infer e1 tyenv, infer e2 tyenv with
        | Int, Int -> Int
        | Float, Float -> Float
        | _ -> failwith "wrong operand type"
    | Times (e1, e2) ->                        
        match infer e1 tyenv, infer e2 tyenv with
        | Int, Int -> Int
        | Float, Float -> Float
        | _ -> failwith "wrong operand type"
    | Neg e ->                                
        match infer e tyenv with
        | Int -> Int
        | Float -> Float
        | _ -> failwith "wrong operand type"
    | IfPositive (e, et, ef) -> 
        match infer e tyenv, infer et tyenv, infer ef tyenv with
        | Int, Int, Int -> Int
        | Float, Float, Float -> Float
        | Int, Float, Float -> Float
        | Float, Int, Int -> Int
        | _, _, _ -> failwith "branches of different types"
    | Match (e, xi, ei, xf, ef) -> 
        match infer e tyenv with
        | Int -> infer ei ([xi, Int] @ tyenv)
        | Float -> infer ef ([xf, Float] @ tyenv)
        | _ -> failwith "wrong operand type"
    | IntToFloat (n) ->
        match infer n tyenv with
        | Int -> Float
        | Float -> failwith "wrong operand type"

// Problem 7
let rec add_casts (e : iexpr) (tyenv : tyenvir) : expr =
    match e with
    | IVar x -> Var x // possibly have to change this to V2
    | INumI i -> NumI i
    | INumF f -> NumF f
    | IPlus(e1, e2) ->
        match (infer(add_casts e1 tyenv) tyenv, infer(add_casts e2 tyenv) tyenv) with
        | Int, Int -> Plus(add_casts e1 tyenv, add_casts e2 tyenv)
        | Int, Float -> Plus(IntToFloat(add_casts e1 tyenv), add_casts e2 tyenv)
        | Float, Int -> Plus(add_casts e1 tyenv, IntToFloat(add_casts e2 tyenv))
        | Float, Float -> Plus(add_casts e1 tyenv, add_casts e2 tyenv)
    | ITimes(e1, e2) ->
        match (infer(add_casts e1 tyenv) tyenv, infer(add_casts e2 tyenv) tyenv) with
        | Int, Int -> Times(add_casts e1 tyenv, add_casts e2 tyenv)
        | Int, Float -> Times(IntToFloat(add_casts e1 tyenv), add_casts e2 tyenv)
        | Float, Int -> Times(add_casts e1 tyenv, IntToFloat(add_casts e2 tyenv))
        | Float, Float -> Times(add_casts e1 tyenv, add_casts e2 tyenv)
    | INeg e ->
        Neg(add_casts e tyenv)
    | IIfPositive (e, et, ef) -> 
        IfPositive(add_casts e tyenv, add_casts et tyenv, add_casts ef tyenv)

// Problem 8
// ANSWER 8 HERE:
// Question for eval (add_matches e) [] and eval (add_casts e []) [];;
// The functions eval (add_matches e) [] and eval (add_casts e []) [];; will return the same thing 
// but the main behavior of those two functions is not the same. The function add_matches does not need
// an environment to store its variable types whereas the function add_casts needs an environment to store
// its variable types. For instance, add_casts has an environment which contains all the variable types for 
// the add_casts function in order to convert the implicit expressions to expressions. On the other hand, 
// the function add_matches does not have an environment to store its variable types, but the function 
// takes an implicit expression and converts it straigth to an expression without needing the variable types.
// That implies that the function add_casts behaves like a strongly typed programming language and the function
// add_matches behaves like a weekly typed programming language.

//Question for infer (add_matches e) [] and infer (add_casts e []) [];; 
//Same reason as state here above

// Problem 9
let rec rlower (inss : rcode) : rcode = 
    match inss with
    | [] -> []
    | RPop :: tail -> RStore :: RErase :: rlower tail
    | RDup :: tail -> RStore :: RLoad 0 :: RLoad 0 :: RErase :: rlower tail
    | RSwap :: tail -> RStore :: RStore :: RLoad 1 :: RLoad 0 :: RErase :: RErase :: rlower tail
    | head :: tail ->  head :: rlower tail

