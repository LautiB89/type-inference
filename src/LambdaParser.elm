module LambdaParser exposing
    ( parse
    , viewExpr
    , viewTypedExpr
    )

import Expr exposing (Expr(..), fromExpr)
import List exposing (foldl)
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Step(..)
        , Trailing(..)
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
import TypedExpr exposing (TypedExpr, fromTypedExpr)



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
        ]



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


viewExpr : Bool -> Result (List DeadEnd) Expr -> String
viewExpr showImplicitParens res =
    case res of
        Ok expr ->
            fromExpr showImplicitParens expr

        Err _ ->
            "Parsing failed"


viewTypedExpr : Bool -> Result (List DeadEnd) TypedExpr -> String
viewTypedExpr showImplicitParens res =
    case res of
        Ok expr ->
            fromTypedExpr showImplicitParens expr

        Err _ ->
            "Parsing failed"



-- infer : Expr -> AlgorithmICtx -> (AlgorithmIRes, AlgorithmICtx)
-- infer e ctx =
--     let
--         freshVarNat = ctx.nextFreshVar
--     in
--         case e of
--             Var id ->
--                 ({ typedExpr = (TEVar id), exprType = TVar freshVarNat}, { ctx | context = insert id freshVarNat, nextFreshVar = freshVarNat + 1  })
--             Abs id expr ->
--                 let
--                     (recRes, recCtx) = infer expr { ctx | context = remove id context }
--                 in
--                     ({ typedExpr = TEAbs id (TVar freshVarNat) (recRes.typedExpr) }, recCtx)
--             App expr expr -> ({}, {ctx})
--             Bool expr -> ({ }, {ctx})
--             Nat expr -> ({}, {ctx})
--             If expr expr expr -> ({}, {ctx})
--- Type infer
