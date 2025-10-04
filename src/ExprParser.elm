module ExprParser exposing (parse)

import Expr exposing (Expr(..))
import List exposing (foldl)
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Step(..)
        , end
        , keyword
        , lazy
        , oneOf
        , run
        , spaces
        , succeed
        , symbol
        , variable
        )
import Set



--- Nat


natParser : Parser Expr
natParser =
    oneOf
        [ succeed ConstZero
            |. symbol "zero"
        , succeed Succ
            |. keyword "succ"
            |= betweenParens (lazy (\_ -> lambdaParser))
        , succeed Pred
            |. keyword "pred"
            |= betweenParens (lazy (\_ -> lambdaParser))
        , Parser.map intToNatExpr Parser.int
        ]


intToNatExpr : Int -> Expr
intToNatExpr n =
    if n > 0 then
        Succ (intToNatExpr (n - 1))

    else
        ConstZero



--- Bool


boolParser : Parser Expr
boolParser =
    oneOf
        [ succeed ConstTrue
            |. keyword "true"
        , succeed ConstFalse
            |. keyword "false"
        , succeed IsZero
            |. keyword "isZero"
            |= betweenParens (lazy (\_ -> lambdaParser))
        ]


appParser : Parser Expr
appParser =
    succeed (foldl (\x y -> App y x))
        |= lazy (\_ -> nonAppParser)
        |. spaces
        |= Parser.loop [] appParserHelper


appParserHelper : List Expr -> Parser (Step (List Expr) (List Expr))
appParserHelper xs =
    oneOf
        [ succeed (\x -> Loop (x :: xs))
            |= lazy (\_ -> nonAppParser)
            |. spaces
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse xs))
        ]


nonAppParser : Parser Expr
nonAppParser =
    oneOf
        [ varParser
        , boolParser
        , natParser
        , ifParser
        , absParser
        , betweenParens (lazy (\_ -> lambdaParser))
        ]


betweenParens : Parser a -> Parser a
betweenParens p =
    succeed identity
        |. symbol "("
        |. spaces
        |= p
        |. spaces
        |. symbol ")"


varIdParser : Parser String
varIdParser =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "true", "false", "if", "then", "else", "zero", "succ", "pred", "isZero" ]
        }


varParser : Parser Expr
varParser =
    Parser.map Var varIdParser


absParser : Parser Expr
absParser =
    succeed Abs
        |. symbol "\\"
        |= varIdParser
        |. spaces
        |. symbol "."
        |. spaces
        |= lazy (\_ -> lambdaParser)


ifParser : Parser Expr
ifParser =
    succeed If
        |. keyword "if"
        |. spaces
        |= lazy (\_ -> lambdaParser)
        |. spaces
        |. keyword "then"
        |. spaces
        |= lazy (\_ -> lambdaParser)
        |. spaces
        |. keyword "else"
        |. spaces
        |= lazy (\_ -> lambdaParser)


lambdaParser : Parser Expr
lambdaParser =
    oneOf
        [ appParser
        , nonAppParser
        , betweenParens (lazy (\_ -> lambdaParser))
        ]


parse : String -> Result (List DeadEnd) Expr
parse =
    run (lambdaParser |. end)
