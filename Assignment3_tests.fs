// T-501-FMAL, Spring 2021, Assignment 3

module Assignment3_tests

open System
open Assignment3
open Tests

// Test cases for Problem 4
let test_part4 args =
    printf "\nPart 4: "
    startTest()

    let n = ref (NoLink "'n", 0)
    let o = ref (NoLink "'o", 0)
    let unifyTest t1 t2 =
        n := (NoLink "'n", 0);
        o := (NoLink "'o", 0);
        unify t1 t2;
        showType t1

    Assert (fun _ -> unifyTest Float Float) "Float"
    Assert (fun _ -> unifyTest Float (Vector (LNum 2))) "cannot unify Float and Vector(2)"
    Assert (fun _ -> unifyTest Float (Vector (LVar n))) "cannot unify Float and Vector('n)"
    Assert (fun _ -> unifyTest (Vector (LVar n)) (Vector (LNum 5))) "Vector(5)"
    Assert (fun _ -> unifyTest (Vector (LVar n)) (Vector (LVar o))) "Vector('n)"
    Assert (fun _ -> unifyTest (Vector (LVar n)) (Vector (LVar n))) "Vector('n)"
    Assert (fun _ -> unifyTest (Fun (LVar n, Vector (LVar n))) (Fun (LVar o, Vector (LVar o)))) "Vector('n) -> Vector('n)"
    Assert (fun _ -> unifyTest (Fun (LVar n, Vector (LVar o))) (Fun (LVar o, Vector (LVar n)))) "Vector('n) -> Vector('n)"
    Assert (fun _ -> unifyTest (Fun (LVar n, Vector (LVar o))) (Fun (LNum 4, Vector (LVar n)))) "Vector(4) -> Vector(4)"
    Assert (fun _ -> unifyTest (Fun (LVar n, Vector (LNum 7))) (Fun (LNum 4, Vector (LNum 8)))) "lengths 7 and 8 differ"
    Assert (fun _ -> unifyTest (Vector (LNum 7)) (Fun (LVar n, Vector (LVar o)))) "cannot unify Vector(7) and Vector('n) -> Vector('o)"
    Assert (fun _ -> unifyTest (Fun (LNum 7, Float)) (Fun (LVar n, Vector (LVar o)))) "cannot unify Float and Vector('o)"

    concludeTest()
    
// Test cases for Problem 5
let test_part5() =
    printf "\nPart 5: "
    startTest()

    Assert (fun _ -> inferTop (Plus (NumF 2., NumF 3.))) "Float"
    Assert (fun _ -> inferTop (Plus (Vect [2.;3.], Vect [3.;4.]))) "Vector(2)"
    Assert (fun _ -> inferTop (Plus (NumF 2., Vect [1.]))) "cannot unify Float and Vector(1)"
    Assert (fun _ -> inferTop (Plus (Vect [1.; 2.], Vect [3.; 4.; 5.]))) "lengths 2 and 3 differ"
    Assert (fun _ -> inferTop (Scale (NumF 1., Vect [2.;3.]))) "Vector(2)"
    Assert (fun _ -> inferTop (Scale (Vect [1.], Vect [2.;3.]))) "expected a float"
    Assert (fun _ -> inferTop (IfPositive (NumF 1., Vect [2.;3.], NumF 3.))) "cannot unify Vector(2) and Float"
    Assert (fun _ -> inferTop (Average (Vect [2.;3.]))) "Float"
    Assert (fun _ -> inferTop (Average (NumF 2.))) "expected a vector"
    Assert (fun _ -> inferTop (LetFun ("f", "x", Var "x", Var "f"))) "Vector('o) -> Vector('o)"
    Assert (fun _ -> inferTop (LetFun ("f", "x", LetFun ("g", "y", Plus (Var "x", Var "y"), Var "g"), Var "f"))) "Vector('p) -> Vector('p) -> Vector('p)"
    Assert (fun _ -> inferTop (LetFun ("f", "x", Plus (Var "x", Vect [4.; 5.]), Var "f"))) "Vector(2) -> Vector(2)"
    Assert (fun _ -> inferTop (LetFun ("f", "x", LetFun ("g", "y", Average (Plus (Var "x", Var "y")), Var "g"), Var "f"))) "Vector('p) -> Vector('p) -> Float"

    let neg = LetFun ("neg", "x", Scale (NumF -1.0, Var "x"), Var "neg")
    let zero = LetFun ("zero", "x", Plus (Var "x", Call (neg, Var "x")), Var "zero")
    let abs = LetFun ("abs", "x", IfPositive (Average (Var "x"), Var "x", Call (neg, Var "x")), Var "abs")

    Assert (fun _ -> inferTop neg) "Vector('o) -> Vector('o)"
    Assert (fun _ -> inferTop zero) "Vector('q) -> Vector('q)"
    Assert (fun _ -> inferTop abs) "Vector('q) -> Vector('q)"
    
    concludeTest()

let main args =
    test_part4()
    test_part5()

    PrintFinal()
