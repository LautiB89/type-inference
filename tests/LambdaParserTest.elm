module LambdaParserTest exposing (..)

import Expect
import LambdaParser exposing (BoolExpr(..), Expr(..), NatExpr(..), parse)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lambda calculus expressions parser"
        [ boolParseTest
        , natParseTest
        , varParseTest
        ]


varParseTest : Test
varParseTest =
    describe "varParser"
        [ test "Simple var x" <|
            \_ ->
                Expect.equal (parse "x") (Ok (Var "x"))
        , test "Long var" <|
            \_ ->
                Expect.equal (parse "xyz") (Ok (Var "xyz"))
        , test "Long var with underscore" <|
            \_ ->
                Expect.equal (parse "x_aaa") (Ok (Var "x_aaa"))
        ]


natParseTest : Test
natParseTest =
    describe "natParser"
        [ test "zero" <|
            \_ ->
                Expect.equal (parse "zero") (Ok (Nat ConstZero))
        , test "succ" <|
            \_ ->
                Expect.equal (parse "succ(zero)") (Ok (Nat (Succ ConstZero)))
        , test "pred" <|
            \_ ->
                Expect.equal (parse "pred(zero)") (Ok (Nat (Pred ConstZero)))
        ]


boolParseTest : Test
boolParseTest =
    describe "boolParser"
        [ test "true" <|
            \_ ->
                Expect.equal (parse "true") (Ok (Bool ConstTrue))
        , test "false" <|
            \_ ->
                Expect.equal (parse "false") (Ok (Bool ConstTrue))
        , test "isZero" <|
            \_ ->
                Expect.equal (parse "isZero(zero)") (Ok (Bool (IsZero ConstZero)))
        ]



--             [ test "true" <|
--     \_ ->
--         let
--             palindrome =
--                 "hannah"
--         in
--         Expect.equal palindrome (String.reverse palindrome)
-- -- Expect.equal is designed to be used in pipeline style, like this.
-- , test "false" <|
--     \_ ->
--         "ABCDEFG"
--             |> String.reverse
--             |> Expect.equal "GFEDCBA"
-- ]
