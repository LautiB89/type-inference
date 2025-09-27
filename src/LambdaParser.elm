module LambdaParser exposing
    ( parse
    , viewExpr
    , Expr(..)
    , NatExpr(..)
    , BoolExpr(..) 
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
            |. keyword "true"
        , succeed ConstFalse
            |. keyword "false"
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
        -- , succeed (\x -> Loop (x :: xs))
        --     |= lazy (\_ -> appParser)
        --     |. spaces
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse xs))
        ]


nonAppParser : Parser Expr
nonAppParser =
    oneOf
        [ varParser
        , Parser.map Bool boolParser
        , Parser.map Nat natParser
        , ifParser
        -- absParser being last is important
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
            "true"

        ConstFalse ->
            "false"

        IsZero natExpr1 ->
            "isZero(" ++ fromNat natExpr1 ++ ")"


isApp : Expr -> Bool
isApp expr =
    case expr of
        App _ _ ->
            True

        _ ->
            False


fromExpr : Expr -> String
fromExpr expr =
    case expr of
        Var id ->
            id

        Abs id expr1 ->
            "(λ" ++ id ++ " . " ++ fromExpr expr1 ++ ")"

        App expr1 expr2 ->
            fromExpr expr1 ++ " " ++ maybeParens (fromExpr expr2) (isApp expr2)

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


maybeParens : String -> Bool -> String
maybeParens s b =
    if b then
        "(" ++ s ++ ")"

    else
        s
