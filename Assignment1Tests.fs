// T-501-FMAL, Spring 2021, Assignment 1

// Test cases for Problem 1

// > nf 0;;
// val it : int = 1
// > nf 1;;
// val it : int = 2
// > nf 2;;
// val it : int = 7
// > nf 3;;
// val it : int = 20
// > nf 4;;
// val it : int = 61



// Test cases for Problem 2

// > lastTrue (fun x -> 10 < x && x < 40) 100;;
// val it : int = 39
// > lastTrue (fun x -> 10 < x && x < 40) 30;;
// val it : int = 29
// > lastTrue (fun x -> 10 < x && x < 40) 10;;
// val it : int = -1
// > lastTrue (fun x -> true) (-3);;
// val it : int = -1

// > lastEqual 3 (fun x -> x / 10) 100;;
// val it : int = 39
// > lastEqual 3 (fun x -> x / 10) 20;;
// val it : int = -1
// > lastEqual 3 (fun x -> x % 10) 100;;
// val it : int = 93
// > lastEqual 3 (fun x -> x % 10) 20;;
// val it : int = 13

// > firstTrue (fun x -> 10 < x && x < 40) 100;;
// val it : int = 11
// > firstTrue (fun x -> 10 < x && x < 40) 30;;
// val it : int = 11
// > firstTrue (fun x -> 10 < x && x < 40) 10;;
// val it : int = -1
// > firstTrue (fun x -> true) (-3);;
// val it : int = -1



// Test cases for Problem 3

// > repeat_map (fun x -> x + 1) [0..10];;
// val it : int list = [1; 3; 5; 7; 9; 11; 13; 15; 17; 19; 21]
// > repeat_map (fun x -> - x) [1..10];;
// val it : int list = [-1; 2; -3; 4; -5; 6; -7; 8; -9; 10]
// > repeat_map (fun x -> x + x) ["x"; "y"; "z"; "w"];;
// val it : string list = ["xx"; "yyyy"; "zzzzzzzz"; "wwwwwwwwwwwwwwww"]


// Test cases for Problem 4

// > sum_some [];;
// val it : int = 0
// > sum_some [None; Some 2; None];;
// val it : int = 2
// > sum_some [None; Some 2; None; Some 4; Some -1];;
// val it : int = 5
// > sum_some [None; None; None];;
// val it : int = 0
// > sum_some [Some 1; Some 2; Some 3];;
// val it : int = 6

// > sum_some2 [];;
// val it : int = 0
// > sum_some2 [None; Some 2; None];;
// val it : int = 2
// > sum_some2 [None; Some 2; None; Some 4; Some -1];;
// val it : int = 5
// > sum_some2 [None; None; None];;
// val it : int = 0
// > sum_some2 [Some 1; Some 2; Some 3];;
// val it : int = 6

// > sum_some3 [];;
// val it : int = 0
// > sum_some3 [None; Some 2; None];;
// val it : int = 2
// > sum_some3 [None; Some 2; None; Some 4; Some -1];;
// val it : int = 5
// > sum_some3 [None; None; None];;
// val it : int = 0
// > sum_some3 [Some 1; Some 2; Some 3];;
// val it : int = 6


// Test cases for Problem 5

// > ne_product (One 2);;
// val it : int = 2
// > ne_product (Cons (3, One 2));;
// val it : int = 6
// > ne_product (Cons (5, Cons (3, One 2)));;
// val it : int = 30
// > ne_product (Cons (6, Cons (5, Cons (3, One 2))));;
// val it : int = 180

// > ne_append (Cons ("b", One "a")) (Cons ("x", One "y"));; 
// val it : string nelist = Cons ("b",Cons ("a",Cons ("x",One "y")))
// > ne_append (One "a") (Cons ("x", One "y"));;
// val it : string nelist = Cons ("a",Cons ("x",One "y"))
// > ne_append (Cons ("b", One "a")) (Cons ("x", One "y"));;
// val it : string nelist = Cons ("b",Cons ("a",Cons ("x",One "y")))
// > ne_append (Cons ("c", Cons ("b", One "a"))) (Cons ("x", One "y"));;
// val it : string nelist = Cons ("c",Cons ("b",Cons ("a",Cons ("x",One "y"))))
// > ne_append (Cons (1, Cons (2, Cons (3, One 4)))) (One 6);;
// val it : int nelist = Cons (1,Cons (2,Cons (3,Cons (4,One 6))))

// > to_list (Cons (1, Cons (2, Cons (3, One 4))));;
// val it : int list = [1; 2; 3; 4]
// > to_list (One "x");;
// val it : string list = ["x"]
// > to_list (Cons ("y", One "x"));;
// val it : string list = ["y"; "x"]

// > ne_map (fun x -> x * 2) (Cons (1, Cons (2, Cons (3, One 4))));;
// val it : int nelist = Cons (2,Cons (4,Cons (6,One 8)))
// > ne_map (fun x -> "a" + x) (Cons ("x", Cons ("y", One "z")));;
// val it : string nelist = Cons ("ax",Cons ("ay",One "az"))
// > ne_map (fun x -> x + x) (Cons ("x", Cons ("y", Cons ("z", One "w"))));;
// val it : string nelist = Cons ("xx",Cons ("yy",Cons ("zz",One "ww")))

// > from_pair ("x", []);;
// val it : string nelist = One "x"
// > from_pair ("x", ["y"; "z"; "w"]);;
// val it : string nelist = Cons ("x",Cons ("y",Cons ("z",One "w")))
// > from_pair (10, [1..5]);;
// val it : int nelist = Cons (10,Cons (1,Cons (2,Cons (3,Cons (4,One 5)))))
// > from_pair ([1], [[2..3]; [4..7]]);;
// val it : int list nelist = Cons ([1],Cons ([2; 3],One [4; 5; 6; 7]))


// Test cases for Problem 6

let t1  = { value = 2; children = []; product = None }
let t1' = { value = 2; children = []; product = Some 2 }
let t2 =
  { value = 3
  ; children =
    [ { value = 4; children = []; product = None }
    ; { value = 5; children = []; product = None }
    ]
  ; product = None }
let t2' =
  { value = 3
  ; children =
    [ { value = 4; children = []; product = Some 4 }
    ; { value = 5; children = []; product = Some 5 }
    ]
  ; product = Some 60 }
let t3 =
  { value = 6
  ; children =
    [ { value = 7
      ; children =
        [ { value = 8; children = []; product = None }
        ; { value = 9; children = []; product = None }
        ]
      ; product = None }
    ; { value = 10; children = []; product = None }
    ]
  ; product = None }
let t3' =
  { value = 6
  ; children =
    [ { value = 7
      ; children =
        [ { value = 8; children = []; product = Some 8 }
        ; { value = 9; children = []; product = None }
        ]
      ; product = None }
    ; { value = 10; children = []; product = None }
    ]
  ; product = Some 30240 }
let t3'' =
  { value = 6
  ; children =
    [ { value = 7
      ; children =
        [ { value = 8; children = []; product = Some 8 }
        ; { value = 9; children = []; product = Some 9 }
        ]
      ; product = Some 504 }
    ; { value = 10; children = []; product = None }
    ]
  ; product = None }
let t3''' =
  { value = 6
  ; children =
    [ { value = 7
      ; children =
        [ { value = 8; children = []; product = Some 8 }
        ; { value = 9; children = []; product = Some 9 }
        ]
      ; product = Some 504 }
    ; { value = 10; children = []; product = Some 10 }
    ]
  ; product = Some 30240 }
let t4 =
  { value = 6
  ; children =
    [ { value = 7; children = []; product = None }
    ; { value = 8; children = []; product = None }
    ; { value = 9; children = []; product = None }
    ; { value = 10; children = []; product = None }
    ]
  ; product = None }
let t4' =
  { value = 6
  ; children =
    [ { value = 7; children = []; product = Some 7 }
    ; { value = 8; children = []; product = None }
    ; { value = 9; children = []; product = Some 9 }
    ; { value = 10; children = []; product = None }
    ]
  ; product = None }
let t4'' =
  { value = 6
  ; children =
    [ { value = 7; children = []; product = Some 7 }
    ; { value = 8; children = []; product = Some 8 }
    ; { value = 9; children = []; product = Some 9 }
    ; { value = 10; children = []; product = Some 10 }
    ]
  ; product = Some 30240 }

// > are_same t1 t1;;
// val it : bool = true
// > are_same t1 t1';;
// val it : bool = true
// > are_same t1 t2;;
// val it : bool = false
// > are_same t2 t2;;
// val it : bool = true
// > are_same t2 t2';;
// val it : bool = true
// > are_same t2 t3;;
// val it : bool = false
// > are_same t3 t3;;
// val it : bool = true
// > are_same t3 t3';;
// val it : bool = true

// > get_product t1;;
// val it : int = 2
// > get_product t1';;
// val it : int = 2
// > get_product t2;;
// val it : int = 60
// > get_product t2';;
// val it : int = 60
// > get_product t3';;
// val it : int = 30240
// > get_product t3'';;
// val it : int = 30240
// > get_product t3''';;
// val it : int = 30240
// > get_product t4;;
// val it : int = 30240
// > get_product t4';;
// val it : int = 30240

// > fill_products t1;;
// val it : product_tree = { value = 2
//                           children = []
//                           product = Some 2 }
// > fill_products t1';;
// val it : product_tree = { value = 2
//                           children = []
//                           product = Some 2 }
// > fill_products t2;;
// val it : product_tree =
//   { value = 3
//     children = [{ value = 4
//                   children = []
//                   product = Some 4 }; { value = 5
//                                         children = []
//                                         product = Some 5 }]
//     product = Some 60 }
// > fill_products t3 = t3''';;
// val it : bool = true
// > fill_products t3' = t3''';;
// val it : bool = true
// > fill_products t3'' = t3''';;
// val it : bool = true
// > fill_products t4 = t4'';;
// val it : bool = true


