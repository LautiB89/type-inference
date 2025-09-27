module LambdaParser exposing
    ( BoolExpr(..)
    , Expr(..)
    , NatExpr(..)
    , parse
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
    | Succ Expr
    | Pred Expr


natParser : Parser NatExpr
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


type BoolExpr
    = ConstTrue
    | ConstFalse
    | IsZero Expr


boolParser : Parser BoolExpr
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
        , Parser.map Bool boolParser
        , Parser.map Nat natParser
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
            "Falló el parsing"


fromNat : Bool -> NatExpr -> String
fromNat showImplicitParens expr =
    case expr of
        ConstZero ->
            "0"

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
            "(λ" ++ id ++ " . " ++ rec expr1 ++ ")"

        App expr1 expr2 ->
            maybeParens (rec expr1) (isApp expr1 && showImplicitParens) ++ " " ++ maybeParens (rec expr2) (isApp expr2)

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
