module ParserTest exposing (suite)

import Expect
import Expr exposing (Expr(..))
import ExprParser exposing (parse)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lambda calculus expressions parser"
        [ boolParseTest
        , natParseTest
        , varParseTest
        , absParseTest
        , appParseTest
        , ifParseTest
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
        ]


natParseTest : Test
natParseTest =
    describe "natParser"
        [ test "zero" <|
            \_ ->
                Expect.equal (parse "zero") (Ok ConstZero)
        , test "succ" <|
            \_ ->
                Expect.equal (parse "succ(zero)") (Ok (Succ ConstZero))
        , test "pred" <|
            \_ ->
                Expect.equal (parse "pred(zero)") (Ok (Pred ConstZero))
        , test "succ of some applications" <|
            \_ ->
                Expect.equal (parse "succ(x y z)") (Ok (Succ (App (App (Var "x") (Var "y")) (Var "z"))))
        , test "pred of succ of var" <|
            \_ ->
                Expect.equal (parse "pred(succ(y1))") (Ok (Pred (Succ (Var "y1"))))
        ]


boolParseTest : Test
boolParseTest =
    describe "boolParser"
        [ test "true" <|
            \_ ->
                Expect.equal (parse "true") (Ok ConstTrue)
        , test "false" <|
            \_ ->
                Expect.equal (parse "false") (Ok ConstFalse)
        , test "isZero" <|
            \_ ->
                Expect.equal (parse "isZero(zero)") (Ok (IsZero ConstZero))
        , test "isZero of a wrong application" <|
            \_ ->
                Expect.equal (parse "isZero(false x1)") (Ok (IsZero (App ConstFalse (Var "x1"))))
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


ifParseTest : Test
ifParseTest =
    describe "ifParser"
        [ test "Basic if expression: if x then y else z" <|
            \_ ->
                Expect.equal (parse "if x then y else z") (Ok (If (Var "x") (Var "y") (Var "z")))
        ]
