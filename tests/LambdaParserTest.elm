module LambdaParserTest exposing (suite)

import Expect
import LambdaParser exposing (BoolExpr(..), Expr(..), NatExpr(..), parse)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lambda calculus expressions parser"
        [ boolParseTest
        , natParseTest
        , varParseTest
        , absParseTest
        , appParseTest
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
                Expect.equal (parse "false") (Ok (Bool ConstFalse))
        , test "isZero" <|
            \_ ->
                Expect.equal (parse "isZero(zero)") (Ok (Bool (IsZero ConstZero)))
        ]


absParseTest : Test
absParseTest =
    describe "absParser"
        [ test "Identity function: (\\x.x)" <|
            \_ ->
                Expect.equal (parse "(\\x.x)") (Ok (Abs "x" (Var "x")))
        , test "Identity function with many spaces" <|
            \_ ->
                Expect.equal (parse "(  \\x     .x   )") (Ok (Abs "x" (Var "x")))
        ]


appParseTest : Test
appParseTest =
    describe "appParser"
        [ test "Basic application of two variables: x y" <|
            \_ ->
                Expect.equal (parse "x y") (Ok (App (Var "x") (Var "y")))
        , test "Left-associativity when applying three variables: x y z" <|
            \_ ->
                Expect.equal (parse "x y z") (Ok (App (App (Var "x") (Var "y")) (Var "z")))
        , test "Respect associativity with parens to left: (x y) z" <|
            \_ ->
                Expect.equal (parse "(x y) z") (Ok (App (App (Var "x") (Var "y")) (Var "z")))
        , test "Respect associativity with parens to right: x (y z)" <|
            \_ ->
                Expect.equal (parse "x (y z)") (Ok (App (Var "x") (App (Var "y") (Var "z"))))
        , test "Parse application of abstraction to variable: (\\x.x) y" <|
            \_ ->
                Expect.equal (parse "(\\x.x) y") (Ok (App (Abs "x" (Var "x")) (Var "y")))
        ]
