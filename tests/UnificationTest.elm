module UnificationTest exposing (suite)

import Dict
import Expect exposing (Expectation)
import Expr exposing (Expr(..))
import Restrictions exposing (Restrictions, mgu)
import Test exposing (Test, describe, test)
import Type exposing (Type(..))
import TypedExpr exposing (TypedExpr(..), infer)


suite : Test
suite =
    describe "Unification algorithm test"
        [ decomposeTest
        , deleteTest
        , swapTest
        , eliminateTest
        , clashTest
        , occursCheckTest
        , examplesTest
        ]


swapTest : Test
swapTest =
    describe "Swap test"
        [ test "Non var expr and Var" <|
            \_ ->
                expectMgu
                    [ ( TAbs (TVar 1) TNat, TVar 2 ) ]
                    [ 1, 2 ]
                    (Ok [ ( 1, TVar 1 ), ( 2, TAbs (TVar 1) TNat ) ])
        ]


examplesTest : Test
examplesTest =
    describe "Examples test"
        [ test "Example 1" <|
          \_ ->
              expectMgu
                  [ ( TAbs (TVar 1) (TAbs (TVar 2) (TVar 1))
                    , TAbs (TVar 2) (TAbs (TAbs (TVar 1) TNat) (TVar 1))
                    )
                  ]
                  []
                  (Err "Occurs check")
        , test "Example 2" <|
            \_ ->
                expectMgu
                    [ ( TAbs (TAbs TNat (TVar 1)) (TAbs (TVar 1) (TVar 3))
                      , TAbs (TVar 2) (TAbs (TAbs (TVar 4) (TVar 4)) (TVar 2))
                      )
                    ]
                    [ 1, 2, 3, 4 ]
                    (Ok
                        [ ( 1, TAbs (TVar 4) (TVar 4) )
                        , ( 2, TAbs TNat (TVar 1) )
                        , ( 3, TAbs TNat (TAbs (TVar 4) (TVar 4)) )
                        , ( 4, TVar 4 )
                        ]
                    )
        ]


clashTest : Test
clashTest =
    describe "Clash test"
        [ test "Nat with Bool" <|
            \_ ->
                expectMgu
                    [ ( TNat, TBool ) ]
                    []
                    (Err "Clash")
        , test "Bool with Nat" <|
            \_ ->
                expectMgu
                    [ ( TBool, TNat ) ]
                    []
                    (Err "Clash")
        , test "Abs with Bool" <|
            \_ ->
                expectMgu
                    [ ( TAbs TBool TBool, TBool ) ]
                    []
                    (Err "Clash")
        , test "Abs with Nat" <|
            \_ ->
                expectMgu
                    [ ( TAbs TBool TBool, TNat ) ]
                    []
                    (Err "Clash")
        ]


occursCheckTest : Test
occursCheckTest =
    describe "Occurs check test"
        [ test "1 level occurs check left" <|
            \_ ->
                expectMgu
                    [ ( TVar 1, TAbs (TVar 1) TBool ) ]
                    []
                    (Err "Occurs check")
        , test "1 level occurs check right" <|
            \_ ->
                expectMgu
                    [ ( TVar 1, TAbs TBool (TVar 1) ) ]
                    []
                    (Err "Occurs check")
        , test "1 level occurs check right and left" <|
            \_ ->
                expectMgu
                    [ ( TVar 1, TAbs (TVar 1) (TVar 1) ) ]
                    []
                    (Err "Occurs check")
        , test "2 levels occurs check left" <|
            \_ ->
                expectMgu
                    [ ( TVar 1, TAbs (TAbs TNat (TVar 1)) TBool ) ]
                    []
                    (Err "Occurs check")
        , test "2 levels occurs check right" <|
            \_ ->
                expectMgu
                    [ ( TVar 1, TAbs TBool (TAbs (TVar 1) TNat) ) ]
                    []
                    (Err "Occurs check")
        ]


deleteTest : Test
deleteTest =
    describe "Delete test"
        [ test "Var" <|
            \_ ->
                expectMgu
                    [ ( TVar 1, TVar 1 ) ]
                    [ 1 ]
                    (Ok [ ( 1, TVar 1 ) ])
        ]


expectMgu : Restrictions -> List Int -> Result String (List ( Int, Type )) -> Expectation
expectMgu restrictions varDom expectedRes =
    let
        resSust =
            mgu restrictions
    in
    Expect.equal
        (Result.map (\sust -> List.map (\n -> ( n, sust n )) varDom) resSust)
        expectedRes


decomposeTest : Test
decomposeTest =
    describe "Decompose rule test"
        [ test "Abs" <|
            \_ ->
                expectMgu
                    [ ( TAbs (TVar 1) TNat, TAbs TBool (TVar 2) ) ]
                    [ 1, 2 ]
                    (Ok [ ( 1, TBool ), ( 2, TNat ) ])
        , test "Nat" <|
            \_ ->
                expectMgu
                    [ ( TNat, TNat ) ]
                    []
                    (Ok [])
        , test "Bool" <|
            \_ ->
                expectMgu
                    [ ( TBool, TBool ) ]
                    []
                    (Ok [])
        ]


eliminateTest : Test
eliminateTest =
    describe "Eliminate (replace var) test"
        [ test "Identity function: (\\x.x)" <|
            \_ ->
                expectMgu
                    [ ( TVar 1, TAbs TBool (TVar 2) ) ]
                    [ 1, 2 ]
                    (Ok [ ( 1, TAbs TBool (TVar 2) ), ( 2, TVar 2 ) ])
        ]
