module UnificationTest exposing (suite)

import Expect exposing (Expectation)
import Restrictions exposing (MguError(..), Restrictions, mgu)
import Substitution exposing (apply)
import Test exposing (Test, describe, test)
import Type exposing (Type(..))


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
                expectMguOk
                    [ ( TAbs (TVar 1) TNat, TVar 2 ) ]
                    [ 1, 2 ]
                    [ ( 1, TVar 1 ), ( 2, TAbs (TVar 1) TNat ) ]
        ]


examplesTest : Test
examplesTest =
    describe "Examples test"
        [ test "Example 1" <|
            \_ ->
                expectMguErr
                    [ ( TAbs (TVar 1) (TAbs (TVar 2) (TVar 1))
                      , TAbs (TVar 2) (TAbs (TAbs (TVar 1) TNat) (TVar 1))
                      )
                    ]
                    (OccursCheck (TVar 2) (TAbs (TVar 2) TNat))
        , test "Example 2" <|
            \_ ->
                expectMguOk
                    [ ( TAbs (TAbs TNat (TVar 1)) (TAbs (TVar 1) (TVar 3))
                      , TAbs (TVar 2) (TAbs (TAbs (TVar 4) (TVar 4)) (TVar 2))
                      )
                    ]
                    [ 1, 2, 3, 4 ]
                    [ ( 1, TAbs (TVar 4) (TVar 4) )
                    , ( 2, TAbs TNat (TAbs (TVar 4) (TVar 4)) )
                    , ( 3, TAbs TNat (TAbs (TVar 4) (TVar 4)) )
                    , ( 4, TVar 4 )
                    ]
        , test "Substitution composition" <|
            \_ ->
                expectMguOk
                    [ ( TVar 1, TVar 2 )
                    , ( TVar 2, TVar 3 )
                    , ( TVar 3, TVar 4 )
                    , ( TVar 4, TVar 5 )
                    ]
                    [ 1, 2, 3, 4, 5 ]
                    [ ( 1, TVar 5 )
                    , ( 2, TVar 5 )
                    , ( 3, TVar 5 )
                    , ( 4, TVar 5 )
                    , ( 5, TVar 5 )
                    ]
        , test "Substitution composition over nested type" <|
            \_ ->
                expectMguOk
                    [ ( TVar 1, TVar 2 )
                    , ( TVar 2, TAbs TBool TBool )
                    , ( TNat, TVar 4 )
                    , ( TVar 3, TAbs (TVar 4) (TVar 2) )
                    , ( TVar 3, TVar 5 )
                    ]
                    [ 1, 2, 3, 4, 5 ]
                    [ ( 1, TAbs TBool TBool )
                    , ( 2, TAbs TBool TBool )
                    , ( 3, TAbs TNat (TAbs TBool TBool) )
                    , ( 4, TNat )
                    , ( 5, TAbs TNat (TAbs TBool TBool) )
                    ]
        ]


clashTest : Test
clashTest =
    describe "Clash test"
        [ test "Nat with Bool" <|
            \_ ->
                expectMguErr
                    [ ( TNat, TBool ) ]
                    (Clash TNat TBool)
        , test "Bool with Nat" <|
            \_ ->
                expectMguErr
                    [ ( TBool, TNat ) ]
                    (Clash TBool TNat)
        , test "Abs with Bool" <|
            \_ ->
                expectMguErr
                    [ ( TAbs TBool TBool, TBool ) ]
                    (Clash (TAbs TBool TBool) TBool)
        , test "Abs with Nat" <|
            \_ ->
                expectMguErr
                    [ ( TAbs TBool TBool, TNat ) ]
                    (Clash (TAbs TBool TBool) TNat)
        ]


occursCheckTest : Test
occursCheckTest =
    describe "Occurs check test"
        [ test "1 level occurs check left" <|
            \_ ->
                expectMguErr
                    [ ( TVar 1, TAbs (TVar 1) TBool ) ]
                    (OccursCheck (TVar 1) (TAbs (TVar 1) TBool))
        , test "1 level occurs check right" <|
            \_ ->
                expectMguErr
                    [ ( TVar 1, TAbs TBool (TVar 1) ) ]
                    (OccursCheck (TVar 1) (TAbs TBool (TVar 1)))
        , test "1 level occurs check right and left" <|
            \_ ->
                expectMguErr
                    [ ( TVar 1, TAbs (TVar 1) (TVar 1) ) ]
                    (OccursCheck (TVar 1) (TAbs (TVar 1) (TVar 1)))
        , test "2 levels occurs check left" <|
            \_ ->
                expectMguErr
                    [ ( TVar 1, TAbs (TAbs TNat (TVar 1)) TBool ) ]
                    (OccursCheck (TVar 1) (TAbs (TAbs TNat (TVar 1)) TBool))
        , test "2 levels occurs check right" <|
            \_ ->
                expectMguErr
                    [ ( TVar 1, TAbs TBool (TAbs (TVar 1) TNat) ) ]
                    (OccursCheck (TVar 1) (TAbs TBool (TAbs (TVar 1) TNat)))
        ]


deleteTest : Test
deleteTest =
    describe "Delete test"
        [ test "Var" <|
            \_ ->
                expectMguOk
                    [ ( TVar 1, TVar 1 ) ]
                    [ 1 ]
                    [ ( 1, TVar 1 ) ]
        ]


expectMguOk : Restrictions -> List Int -> List ( Int, Type ) -> Expectation
expectMguOk restrictions varDom expectedRes =
    Expect.equal
        (Result.map (\sust -> List.map (\n -> ( n, apply sust n )) varDom) (mgu restrictions))
        (Ok expectedRes)


expectMguErr : Restrictions -> MguError -> Expectation
expectMguErr restrictions expectedError =
    Expect.equal (mgu restrictions) (Err expectedError)


decomposeTest : Test
decomposeTest =
    describe "Decompose rule test"
        [ test "Abs" <|
            \_ ->
                expectMguOk
                    [ ( TAbs (TVar 1) TNat, TAbs TBool (TVar 2) ) ]
                    [ 1, 2 ]
                    [ ( 1, TBool ), ( 2, TNat ) ]
        , test "Nat" <|
            \_ ->
                expectMguOk
                    [ ( TNat, TNat ) ]
                    []
                    []
        , test "Bool" <|
            \_ ->
                expectMguOk
                    [ ( TBool, TBool ) ]
                    []
                    []
        ]


eliminateTest : Test
eliminateTest =
    describe "Eliminate (replace var) test"
        [ test "Identity function: (\\x.x)" <|
            \_ ->
                expectMguOk
                    [ ( TVar 1, TAbs TBool (TVar 2) ) ]
                    [ 1, 2 ]
                    [ ( 1, TAbs TBool (TVar 2) ), ( 2, TVar 2 ) ]
        ]
