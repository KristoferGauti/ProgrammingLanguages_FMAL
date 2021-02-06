// T-501-FMAL, Spring 2021, Assignment 1


"""STUDENT NAMES HERE: Bergur Tareq Tamimi Einarsson & Kristofer Gauti Þórhallsson"""


module Assignment1

// Problem 1
// nf : int -> int
let rec nf n = 
    match n with
    | _ when n < 1 -> 1
    | _ when n = 1 -> 2
    | _ when n > 1 -> 2 * nf(n - 1) + 3 * nf(n - 2)

// Problem 2
// (i)
// lastTrue : (int -> bool) -> int -> int
let rec lastTrue f n =
    let i = n-1
    match f i, i with
    | _,_ when n < 1 -> -1
    | true,_ -> i
    | false,_ -> lastTrue f (i)

// (ii)
// lastEqual : 'a -> (int -> 'a) -> int -> int when 'a : equality
let lastEqual x f n =
    lastTrue (fun n -> f n = x) n 



// (iii)
// firstTrue : (int -> bool) -> int -> int

let rec helper f n x = 
    match f x, n with
    | _,_ when n < 1 -> -1
    | _,_ when x = n -> -1
    | true,_ -> x
    | _,_ -> helper f n (x+1)
    

let firstTrue f n =
    helper f n 0

// (iv)
// If  lastTrue (fun x -> f x > f (x + 1)) 100  evaluates to -1,
// what can you say about  f?
(*
ANSWER 2(iv)(a) HERE: ...
    By defining a function let f x = x and pass it into lastTrue (fun x -> f x > f (x + 1)) 100 we will get -1.
    That means in our case that for every n from 0 to 99 there does not exist a number in that range that satisfies the
    condition f x > f(x + 1). Therfore the function f is decreasing as x is increasing.
*)

// How about if  lastTrue f 100 = firstTrue f 100  is  true?
(*
ANSWER 2(iv)(b) HERE: ...
    Since lastTrue finds the last number in a range from 0 to n and firstTrue finds the first number in a range from 0 to n 
    to satisfy the boolean function. There has to be only one value that satisfies the condition given, whereas lastTrue f 100 = firstTrue f 100 is true.
*)


// Problem 3
// repeat_map : ('a -> 'a) -> 'a list -> 'a list
let rec repeat_map f xs =
    match xs with
    | [] -> []
    | x::xs -> f x :: repeat_map f (List.map f xs)
    

// Problem 4
// (i)
// sum_some : int option list -> int
let rec sum_some xs =
    match xs with
    | [] -> 0
    | x::xs -> match x with
                | None -> 0 + sum_some xs
                | Some x -> x + sum_some xs

// (ii)  (uncomment the definition below when you've completed it)

let sum_some2 xs =
    List.fold (fun s o ->
        match o with
        | None -> s
        | Some o -> s+o) 0 xs

// (iii)  (uncomment the definition below when you've completed it)

let sum_some3 xs =
    let f o = match o with
    | None -> 0
    | Some o -> o  
    List.fold (+) 0 (List.map f xs)


// Problem 5
type 'a nelist =
    | One of 'a
    | Cons of 'a * 'a nelist


// (i)
// ne_product : int nelist -> int
let rec ne_product xs =
    match xs with
    | One(x) -> x
    | Cons(x, Cons(y, z)) -> x * ne_product(Cons(y, z))
    | Cons(x, One(y)) -> x * ne_product(One(y))

// (ii)
// ne_append : 'a nelist -> 'a nelist -> 'a nelist     [b;a] + [x;y] -> [b;a;x;y]
let rec ne_append xs ys =
    match xs with
    | One(x) -> Cons(x, ys)                      //the last element in the linked list
    | Cons(x, xs) -> Cons (x, (ne_append xs ys)) //the first element in the linked list
    

// (iii)
// to_list : 'a nelist -> 'a list
let rec to_list (xs : 'a nelist) : 'a list = 
    match xs with
    | One(x) -> [x]
    | Cons(x, xs) -> x :: to_list(xs)


// (iv)
// ne_map : ('a -> 'b) -> 'a nelist -> 'b nelist
let rec ne_map f xs =
    match xs with
    | One(x) -> One(f x)
    | Cons(x, xs) -> Cons(f x, (ne_map f xs))

// (v)
// to_pair : 'a nelist -> 'a * 'a list

let to_pair xs =
    match xs with 
    | One x -> (x, [])
    | Cons (x, xs) -> (x, to_list xs)

// from_pair : 'a * 'a list -> 'a nelist
let rec from_pair(x, xs) =
    match x, xs with
    | _,[] -> One(x)
    | x, xx::xxs -> Cons(x, from_pair(xx,xxs))

from_pair ("x", []);;
from_pair ("x", ["y"; "z"; "w"]);;
from_pair (10, [1..5]);;
from_pair ([1], [[2..3]; [4..7]]);;


// (vi)
// Is it possible to write a function  from_list : 'a list -> 'a nelist
// such that the expressions  to_list (from_list xs) = xs
// and  from_list (to_list ys) = ys  evaluate to  true?
// Explain why.       to_list takes 'a nelist -> 'a list   from_list takes 'a list -> 'a nelist
(*
ANSWER 5(vi) HERE: ...
    That is possible whereas to_list takes as an argument 'a nelist and returns 'a list and from_list takes 'a list and returns 'a nelist.
    With that in mind, we can call to_list (from_list 'a list xs) = 'a list xs;; which will return true for every 'a list xs. If we run 
    from_list (to_list 'a nelist ys) = 'a nelist ys;; which returns true for every 'a nelist ys.
    Thus to_list (from_list xs) = xs and from_list (to_list ys) = ys  evaluates to true.
*)

// Problem 6
type product_tree =
    { value: int
    ; children: product_tree list
    ; product: int option }

// (i)
// are_same : product_tree -> product_tree -> bool

//psuedocode for the function are_same
// let are same t1 t2 =
//     if t1.children.lenght != t2.children.lenght -> false
//     if t1.children.lenght = 0 -> true
//     else 
//         for i in range(t1.children.length):
//             if t1.children[i] = t2.children[i] then are same t1.children[i] t2.children[i]
//             else return false
let rec are_same t1 t2 =
    if t1.value = t2.value then List.forall2 are_same t1.children t2.children else false



// (ii)
// get_product : product_tree -> int
let rec get_product t = 
    match t.children with
    | [] -> t.value
    | _ -> t.value * List.fold(fun acc subtree -> acc * get_product subtree) 1 t.children


// (iii)
// fill_products : product_tree -> product_tree
let rec fill_products t =
    match t.value,t.children,t.product with
    | _,_,None -> let x = { value = t.value; children = t.children; product = Some t.value } //tree which has all its branches set to Some x not None
                  List.fold(fun acc element -> fill_products element) 0 t.children 
    | _,_,Some y -> List.map (get_product t) t.children //x.children virkar ekki
                  
                  
//create a tree and calculate the product 
//calculate the products of the children with map
//create a new tree with the new product and return it
                  
    

