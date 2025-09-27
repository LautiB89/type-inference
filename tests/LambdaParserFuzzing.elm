module LambdaParserFuzzing exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import LambdaParser exposing (..)
import Test exposing (Test, describe, fuzz)
import Parser exposing (DeadEnd)


natFuzzer : Int -> Fuzzer NatExpr
natFuzzer n =
    if n <= 0 then
        Fuzz.constant ConstZero

    else
        Fuzz.oneOf
            [ Fuzz.map Succ (exprTest (n - 1))
            , Fuzz.map Pred (exprTest (n - 1))
            ]


boolFuzzer : Int -> Fuzzer BoolExpr
boolFuzzer n =
    if n <= 0 then
        Fuzz.oneOfValues [ ConstFalse, ConstTrue ]

    else
        Fuzz.map IsZero (exprTest (n - 1))


exprTest : Int -> Fuzzer Expr
exprTest n =
    if n <= 0 then
        Fuzz.oneOf
            [ Fuzz.map Bool (boolFuzzer 0)
            , Fuzz.map Nat (natFuzzer 0)
            , Fuzz.constant (Var "xyz")
            ]

    else
        let
            nprev =
                n - 1

            rec =
                exprTest nprev
        in
        Fuzz.oneOf
            [ Fuzz.map (Abs "xyz") rec
            , Fuzz.map Bool (boolFuzzer nprev)
            , Fuzz.map Nat (natFuzzer nprev)
            , Fuzz.map2 App rec rec
            , Fuzz.map3 If rec rec rec
            ]


suite : Test
suite =
    describe "asdasdsadasadas"
        [ fuzz (exprTest 10) "a ver" <|
            \randomlyGeneratedExpr ->
                Ok randomlyGeneratedExpr
                    |> viewExpr True
                    |> parse
                    |> Expect.equal (Ok randomlyGeneratedExpr)
        ]

viewExpr : Bool -> Result (List DeadEnd) Expr -> String
viewExpr showImplicitParens res =
    case res of
        Ok expr ->
            fromExpr showImplicitParens expr

        Err _ ->
            "Falló el parsing"


fromNat : Bool -> NatExpr -> String
fromNat showImplicitParens expr =
    case expr of
        ConstZero ->
            "zero"

        Succ expr2 ->
            "succ(" ++ fromExpr showImplicitParens expr2 ++ ")"

        Pred expr2 ->
            "pred(" ++ fromExpr showImplicitParens expr2 ++ ")"


fromBool : Bool -> BoolExpr -> String
fromBool showImplicitParens expr =
    case expr of
        ConstTrue ->
            "true"

        ConstFalse ->
            "false"

        IsZero expr1 ->
            "isZero(" ++ fromExpr showImplicitParens expr1 ++ ")"


isApp : Expr -> Bool
isApp expr =
    case expr of
        App _ _ ->
            True

        _ ->
            False


fromExpr : Bool -> Expr -> String
fromExpr showImplicitParens expr =
    let
        rec =
            fromExpr showImplicitParens
    in
    case expr of
        Var id ->
            id

        Abs id expr1 ->
            "(\\" ++ id ++ " . " ++ rec expr1 ++ ")"

        App expr1 expr2 ->
            maybeParens (rec expr1) showImplicitParens ++ " " ++ maybeParens (rec expr2) (isApp expr2)

        Bool boolExpr1 ->
            fromBool showImplicitParens boolExpr1

        Nat natExpr1 ->
            fromNat showImplicitParens natExpr1

        If expr1 expr2 expr3 ->
            "if "
                ++ rec expr1
                ++ " then "
                ++ rec expr2
                ++ " else "
                ++ rec expr3


maybeParens : String -> Bool -> String
maybeParens s b =
    if b then
        "(" ++ s ++ ")"

    else
        s
