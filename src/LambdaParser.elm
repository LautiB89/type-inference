module LambdaParser exposing
    ( parse
    , viewExpr
    )

import List exposing (foldl)
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Step(..)
        , Trailing(..)
        , int
        , keyword
        , lazy
        , oneOf
        , run
        , spaces
        , succeed
        , symbol
        , variable
        )
import Set exposing (fromList)
import String exposing (fromInt)



--- Nat


type NatExpr
    = ConstZero
    | Succ NatExpr
    | Pred NatExpr


natParser : Parser NatExpr
natParser =
    oneOf
        [ succeed ConstZero
            |. symbol "zero"
        , succeed Succ
            |. keyword "succ"
            |= betweenParens (lazy (\_ -> natParser))
        , succeed Pred
            |. keyword "pred"
            |= betweenParens (lazy (\_ -> natParser))
        ]



--- Bool


type BoolExpr
    = ConstTrue
    | ConstFalse
    | IsZero NatExpr


boolParser : Parser BoolExpr
boolParser =
    oneOf
        [ succeed ConstTrue
            |. keyword "True"
        , succeed ConstFalse
            |. keyword "False"
        , succeed IsZero
            |. keyword "isZero"
            |= betweenParens (lazy (\_ -> natParser))
        ]


type alias Id =
    String


type Expr
    = Var Id
    | Abs Id Expr
    | App Expr Expr
    | Bool BoolExpr
    | Nat NatExpr
    | If Expr Expr Expr


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
        , absParser
        , Parser.map Bool boolParser
        , Parser.map Nat natParser
        , ifParser
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
        , reserved = Set.fromList [ "True", "False", "if", "then", "else", "zero", "succ", "pred", "isZero" ]
        }


varParser : Parser Expr
varParser =
    Parser.map Var varIdParser


absParser : Parser Expr
absParser =
    succeed Abs
        |. symbol "("
        |. symbol "\\"
        |= varIdParser
        |. spaces
        |. symbol "."
        |. spaces
        |= lazy (\_ -> lambdaParser)
        |. spaces
        |. symbol ")"


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


parse =
    run lambdaParser


viewExpr : Result (List DeadEnd) Expr -> String
viewExpr res =
    case res of
        Ok expr ->
            fromExpr expr

        Err _ ->
            "Falló el parsing"


fromNat : NatExpr -> String
fromNat expr =
    case expr of
        ConstZero ->
            "0"

        Succ expr2 ->
            "succ(" ++ fromNat expr2 ++ ")"

        Pred expr2 ->
            "pred(" ++ fromNat expr2 ++ ")"


fromBool : BoolExpr -> String
fromBool expr =
    case expr of
        ConstTrue ->
            "True"

        ConstFalse ->
            "False"

        IsZero natExpr1 ->
            "isZero(" ++ fromNat natExpr1 ++ ")"


fromExpr : Expr -> String
fromExpr expr =
    case expr of
        Var id ->
            id

        Abs id expr2 ->
            "(λ" ++ id ++ " . " ++ fromExpr expr2 ++ ")"

        App expr1 expr2 ->
            "(" ++ fromExpr expr1 ++ " " ++ fromExpr expr2 ++ ")"

        Bool boolExpr1 ->
            fromBool boolExpr1

        Nat natExpr1 ->
            fromNat natExpr1

        If expr1 expr2 expr3 ->
            "if "
                ++ fromExpr expr1
                ++ " then "
                ++ fromExpr expr2
                ++ " else "
                ++ fromExpr expr3
