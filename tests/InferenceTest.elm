module InferenceTest exposing (suite)

import Dict
import Expect
import Expr exposing (Expr(..))
import Restrictions
import Test exposing (Test, describe, test)
import Type exposing (Type(..))
import TypedExpr exposing (TypedExpr(..), infer)


suite : Test
suite =
    describe "Terms decoration test"
        [ boolInferenceTest
        , natInferenceTest
        , varInferenceTest
        , absInferenceTest
        ]


varInferenceTest : Test
varInferenceTest =
    describe "Var"
        [ test "Simple var xyz" <|
            \_ ->
                Expect.equal
                    (infer (TEVar "xyz") (Dict.singleton "xyz" (TVar 1)) 2)
                    (Just ( TVar 1, Restrictions.empty, 2 ))
        ]


natInferenceTest : Test
natInferenceTest =
    describe "natParser"
        [ test "zero" <|
            \_ ->
                Expect.equal
                    (infer TEConstZero Dict.empty 1)
                    (Just ( TNat, Restrictions.empty, 1 ))
        , test "succ" <|
            \_ ->
                Expect.equal
                    (infer (TESucc TEConstZero) Dict.empty 1)
                    (Just ( TNat, [(TNat, TNat)], 1 ))
        , test "pred" <|
            \_ ->
                Expect.equal
                    (infer (TEPred TEConstZero) Dict.empty 1)
                    (Just ( TNat, [(TNat, TNat)], 1 ))
        , test "succ of some applications" <|
            \_ ->
                Expect.equal
                    (infer
                        (TESucc (TEApp (TEApp (TEVar "x") (TEVar "y")) (TEVar "z")))
                        (Dict.fromList [ ( "x", TVar 3 ), ( "y", TVar 2 ), ( "z", TVar 1 ) ])
                        4
                    )
                    (Just
                        ( TNat
                        , [ ( TVar 5, TNat )
                          , ( TVar 4, TAbs (TVar 1) (TVar 5) )
                          , ( TVar 3, TAbs (TVar 2) (TVar 4) )
                          ]
                        , 6
                        )
                    )
        ]


boolInferenceTest : Test
boolInferenceTest =
    describe "boolParser"
        [ test "true" <|
            \_ ->
                Expect.equal
                    (infer TEConstTrue Dict.empty 1)
                    (Just ( TBool, Restrictions.empty, 1 ))
        , test "false" <|
            \_ ->
                Expect.equal
                    (infer TEConstFalse Dict.empty 1)
                    (Just ( TBool, Restrictions.empty, 1 ))
        , test "isZero" <|
            \_ ->
                Expect.equal
                    (infer (TEIsZero TEConstZero) Dict.empty 1)
                    (Just ( TBool, Restrictions.singleton ( TNat, TNat ), 1 ))
        , test "isZero of a wrong application" <|
            \_ ->
                Expect.equal
                    (infer (TEIsZero (TEApp TEConstFalse (TEVar "x1"))) (Dict.singleton "x1" (TVar 1)) 2)
                    (Just ( TBool, [ ( TVar 2, TNat ), ( TBool, TAbs (TVar 1) (TVar 2) ) ], 3 ))
        ]


absInferenceTest : Test
absInferenceTest =
    describe "absParser"
        [ test "Identity function: (\\x.x)" <|
            \_ ->
                Expect.equal
                    (infer (TEAbs "x" (TVar 1) (TEVar "x")) Dict.empty 2)
                    (Just ( TAbs (TVar 1) (TVar 1), Restrictions.empty, 2 ))
        , test "Identity function with same free var" <|
            \_ ->
                Expect.equal
                    (infer (TEApp (TEAbs "x1" (TVar 2) (TEVar "x1")) (TEVar "x")) (Dict.singleton "x" (TVar 1)) 3)
                    (Just
                        ( TVar 3
                        , [ ( TAbs (TVar 2) (TVar 2), TAbs (TVar 1) (TVar 3) ) ]
                        , 4
                        )
                    )
        , test "Multiple abstractions with a free var inside" <|
            \_ ->
                Expect.equal
                    (infer
                        (TEApp (TEAbs "x" (TVar 2) (TEVar "x")) (TEAbs "x1" (TVar 3) (TEVar "y")))
                        (Dict.singleton "y" (TVar 1))
                        4
                    )
                    (Just
                        ( TVar 4
                        , [ ( TAbs (TVar 2) (TVar 2), TAbs (TAbs (TVar 3) (TVar 1)) (TVar 4) ) ]
                        , 5
                        )
                    )
        , test "Nested abstractions with same bound var" <|
            \_ ->
                Expect.equal
                    (infer
                        (TEAbs "x1" (TVar 2) (TEAbs "x" (TVar 1) (TEVar "x")))
                        Dict.empty
                        3
                    )
                    (Just ( TAbs (TVar 2) (TAbs (TVar 1) (TVar 1)), [], 3 ))
        , test "Nested abstractions with same bound var and free var" <|
            \_ ->
                Expect.equal
                    (infer
                        (TEApp (TEVar "x") (TEAbs "x1" (TVar 3) (TEAbs "x2" (TVar 2) (TEVar "x2"))))
                        (Dict.singleton "x" (TVar 1))
                        4
                    )
                    (Just
                        ( TVar 4
                        , [ ( TVar 1, TAbs (TAbs (TVar 3) (TAbs (TVar 2) (TVar 2))) (TVar 4) ) ]
                        , 5
                        )
                    )
        ]

