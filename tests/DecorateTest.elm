module DecorateTest exposing (suite)

import Dict
import Expect
import Expr exposing (Expr(..))
import Test exposing (Test, describe, test)
import Type exposing (Type(..))
import TypedExpr exposing (TypedExpr(..), decorate)


suite : Test
suite =
    describe "Terms decoration test"
        [ boolDecorateTest
        , natDecorateTest
        , varDecorateTest
        , absDecorateTest
        ]


varDecorateTest : Test
varDecorateTest =
    describe "Var"
        [ test "Simple var x" <|
            \_ ->
                Expect.equal (decorate (Var "x")) ( Dict.singleton "x" (TVar 1), TEVar "x", 2 )
        , test "Long var" <|
            \_ ->
                Expect.equal (decorate (Var "xyz")) ( Dict.singleton "xyz" (TVar 1), TEVar "xyz", 2 )
        , test "Long var with underscore" <|
            \_ ->
                Expect.equal (decorate (Var "x_aaa")) ( Dict.singleton "x_aaa" (TVar 1), TEVar "x_aaa", 2 )
        ]


natDecorateTest : Test
natDecorateTest =
    describe "natParser"
        [ test "zero" <|
            \_ ->
                Expect.equal (decorate ConstZero) ( Dict.empty, TEConstZero, 1 )
        , test "succ" <|
            \_ ->
                Expect.equal (decorate (Succ ConstZero)) ( Dict.empty, TESucc TEConstZero, 1 )
        , test "pred" <|
            \_ ->
                Expect.equal (decorate (Pred ConstZero)) ( Dict.empty, TEPred TEConstZero, 1 )
        , test "succ of some applications" <|
            \_ ->
                Expect.equal
                    (decorate (Succ (App (App (Var "x") (Var "y")) (Var "z"))))
                    ( Dict.fromList [ ( "x", TVar 3 ), ( "y", TVar 2 ), ( "z", TVar 1 ) ]
                    , TESucc (TEApp (TEApp (TEVar "x") (TEVar "y")) (TEVar "z"))
                    , 4
                    )
        , test "pred of succ of var" <|
            \_ ->
                Expect.equal
                    (decorate (Pred (Succ (Var "y_1"))))
                    ( Dict.singleton "y_1" (TVar 1)
                    , TEPred (TESucc (TEVar "y_1"))
                    , 2
                    )
        ]


boolDecorateTest : Test
boolDecorateTest =
    describe "boolParser"
        [ test "true" <|
            \_ ->
                Expect.equal (decorate ConstTrue) ( Dict.empty, TEConstTrue, 1 )
        , test "false" <|
            \_ ->
                Expect.equal (decorate ConstFalse) ( Dict.empty, TEConstFalse, 1 )
        , test "isZero" <|
            \_ ->
                Expect.equal (decorate (IsZero ConstZero)) ( Dict.empty, TEIsZero TEConstZero, 1 )
        , test "isZero of a wrong application" <|
            \_ ->
                Expect.equal
                    (decorate (IsZero (App ConstFalse (Var "x1"))))
                    ( Dict.singleton "x1" (TVar 1), TEIsZero (TEApp TEConstFalse (TEVar "x1")), 2 )
        ]


absDecorateTest : Test
absDecorateTest =
    describe "absParser"
        [ test "Identity function: (\\x.x)" <|
            \_ ->
                Expect.equal
                    (decorate (Abs "x" (Var "x")))
                    ( Dict.empty, TEAbs "x" (TVar 1) (TEVar "x"), 2 )
        , test "Identity function with same free var" <|
            \_ ->
                Expect.equal
                    (decorate (App (Abs "x1" (Var "x1")) (Var "x")))
                    ( Dict.singleton "x" (TVar 1), TEApp (TEAbs "x1" (TVar 2) (TEVar "x1")) (TEVar "x"), 3 )
        , test "Multiple abstractions with a free var inside" <|
            \_ ->
                Expect.equal
                    (decorate (App (Abs "x" (Var "x")) (Abs "x1" (Var "y"))))
                    ( Dict.singleton "y" (TVar 1), TEApp (TEAbs "x" (TVar 2) (TEVar "x")) (TEAbs "x1" (TVar 3) (TEVar "y")), 4 )
        , test "Nested abstractions with same bound var" <|
            \_ ->
                Expect.equal
                    (decorate (Abs "x1" (Abs "x" (Var "x"))))
                    ( Dict.empty, TEAbs "x1" (TVar 2) (TEAbs "x" (TVar 1) (TEVar "x")), 3 )
        , test "Nested abstractions with same bound var and free var" <|
            \_ ->
                Expect.equal
                    (decorate (App (Var "x") (Abs "x1" (Abs "x2" (Var "x2")))))
                    ( Dict.singleton "x" (TVar 1), TEApp (TEVar "x") (TEAbs "x1" (TVar 3) (TEAbs "x2" (TVar 2) (TEVar "x2"))), 4 )
        ]
