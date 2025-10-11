module RectifyTest exposing (suite)

import Expect
import Expr exposing (Expr(..))
import Rectify exposing (rectify)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Terms rectify test"
        [ boolRectifyTest
        , natRectifyTest
        , varRectifyTest
        , absRectifyTest
        ]


minRectifyKeepsSameTerm : Expr -> () -> Expect.Expectation
minRectifyKeepsSameTerm t =
    \_ ->
        Expect.equal (rectify t) t


varRectifyTest : Test
varRectifyTest =
    describe "Var"
        [ test "Simple var x" (minRectifyKeepsSameTerm (Var "x"))
        , test "Long var" (minRectifyKeepsSameTerm (Var "xyz"))
        , test "Long var with underscore" (minRectifyKeepsSameTerm (Var "x_aaa"))
        ]


natRectifyTest : Test
natRectifyTest =
    describe "natParser"
        [ test "zero" (minRectifyKeepsSameTerm ConstZero)
        , test "succ" (minRectifyKeepsSameTerm (Succ ConstZero))
        , test "pred" (minRectifyKeepsSameTerm (Pred ConstZero))
        , test "succ of some applications" (minRectifyKeepsSameTerm (Succ (App (App (Var "x") (Var "y")) (Var "z"))))
        , test "pred of succ of var" (minRectifyKeepsSameTerm (Pred (Succ (Var "y_1"))))
        ]


boolRectifyTest : Test
boolRectifyTest =
    describe "boolParser"
        [ test "true" (minRectifyKeepsSameTerm ConstTrue)
        , test "false" (minRectifyKeepsSameTerm ConstFalse)
        , test "isZero" (minRectifyKeepsSameTerm (IsZero ConstZero))
        , test "isZero of a wrong application" (minRectifyKeepsSameTerm (IsZero (App ConstFalse (Var "x1"))))
        ]


absRectifyTest : Test
absRectifyTest =
    describe "absParser"
        [ test "Identity function: (\\x.x)" (minRectifyKeepsSameTerm (Abs "x" (Var "x")))
        , test "Identity function with same free var" <|
            \_ ->
                Expect.equal
                    (rectify (App (Abs "x" (Var "x")) (Var "x")))
                    (App (Abs "x1" (Var "x1")) (Var "x"))
        , test "Multiple abstractions with same bound var" <|
            \_ ->
                Expect.equal
                    (rectify (App (Abs "x" (Var "x")) (Abs "x" (Var "x"))))
                    (App (Abs "x1" (Var "x1")) (Abs "x" (Var "x")))
        , test "Nested abstractions with same bound var" <|
            \_ ->
                Expect.equal
                    (rectify (Abs "x" (Abs "x" (Var "x"))))
                    (Abs "x1" (Abs "x" (Var "x")))
        , test "Nested abstractions with same bound var and free var" <|
            \_ ->
                Expect.equal
                    (rectify (App (Var "x") (Abs "x" (Abs "x" (Var "x")))))
                    (App (Var "x") (Abs "x1" (Abs "x2" (Var "x2"))))
        ]
