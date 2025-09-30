module LambdaParser exposing
    ( BoolExpr(..)
    , Expr(..)
    , NatExpr(..)
    , parse
    , rectify
    , viewExpr
    )

import Dict exposing (Dict)
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
import Set exposing (Set)
import String exposing (fromInt)



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
            "Parsing failed"


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


freeExprVars : Expr -> Set Id
freeExprVars e =
    case e of
        Var id ->
            Set.singleton id

        Abs id expr ->
            Set.remove id (freeExprVars expr)

        App expr1 expr2 ->
            Set.union (freeExprVars expr1) (freeExprVars expr2)

        Bool expr ->
            freeBoolVars expr

        Nat expr ->
            freeNatVars expr

        If expr1 expr2 expr3 ->
            Set.union (freeExprVars expr1) (Set.union (freeExprVars expr2) (freeExprVars expr3))


freeBoolVars : BoolExpr -> Set Id
freeBoolVars e =
    case e of
        ConstTrue ->
            Set.empty

        ConstFalse ->
            Set.empty

        IsZero expr ->
            freeExprVars expr


freeNatVars : NatExpr -> Set Id
freeNatVars e =
    case e of
        ConstZero ->
            Set.empty

        Succ expr ->
            freeExprVars expr

        Pred expr ->
            freeExprVars expr


until : (a -> Bool) -> (a -> a) -> a -> a
until p f z =
    if p z then
        z

    else
        until p f (f z)


rectifyHelper : Expr -> Set Id -> Dict Id Int -> Int -> ( Expr, Int )
rectifyHelper e freeVars renames n =
    let
        isFreeVar : Id -> Bool
        isFreeVar vId =
            Set.member vId freeVars

        rec : Expr -> Int -> ( Expr, Int )
        rec =
            \expr -> rectifyHelper expr freeVars renames
    in
    case e of
        Var id ->
            let
                newId =
                    Dict.get id renames
                        |> Maybe.map (\s -> "x" ++ fromInt s)
                        |> Maybe.withDefault
                            (if Set.member id freeVars then
                                id

                             else
                                "¿error?"
                            )
            in
            ( Var newId, n )

        Abs id expr ->
            let
                freshN =
                    until (\nId -> not (isFreeVar ("x" ++ fromInt nId))) (\nId -> nId + 1) n

                newRenames =
                    Dict.insert id freshN renames

                ( expr2, n2 ) =
                    rectifyHelper expr freeVars newRenames (freshN + 1)
            in
            ( Abs ("x" ++ fromInt freshN) expr2, n2 )

        App e1 e2 ->
            let
                ( ne1, n1 ) =
                    rec e1 n

                ( ne2, n2 ) =
                    rec e2 n1
            in
            ( App ne1 ne2, n2 )

        Bool expr ->
            let
                ( expr2, n2 ) =
                    rectifyHelperBool expr freeVars renames n
            in
            ( Bool expr2, n2 )

        Nat expr ->
            let
                ( expr2, n2 ) =
                    rectifyHelperNat expr freeVars renames n
            in
            ( Nat expr2, n2 )

        If expr1 expr2 expr3 ->
            let
                ( ne1, n1 ) =
                    rec expr1 n

                ( ne2, n2 ) =
                    rec expr2 n1

                ( ne3, n3 ) =
                    rec expr3 n2
            in
            ( If ne1 ne2 ne3, n3 )


rectifyHelperBool : BoolExpr -> Set Id -> Dict Id Int -> Int -> ( BoolExpr, Int )
rectifyHelperBool e freeVars renames n =
    case e of
        ConstTrue ->
            ( ConstTrue, n )

        ConstFalse ->
            ( ConstFalse, n )

        IsZero expr ->
            Tuple.mapFirst IsZero (rectifyHelper expr freeVars renames n)


rectifyHelperNat : NatExpr -> Set Id -> Dict Id Int -> Int -> ( NatExpr, Int )
rectifyHelperNat e freeVars renames n =
    case e of
        ConstZero ->
            ( ConstZero, n )

        Succ expr ->
            Tuple.mapFirst Succ (rectifyHelper expr freeVars renames n)

        Pred expr ->
            Tuple.mapFirst Pred (rectifyHelper expr freeVars renames n)


rectify : Expr -> Expr
rectify e =
    let
        fv =
            freeExprVars e
    in
    rectifyHelper e fv Dict.empty 1 |> Tuple.first




--- Type infer


type Type
    = TVar Int
    | TNat
    | TBool
    | TAbs Type Type


type TypedExpr
    = TEVar Id
    | TEAbs Id Type Expr
    | TEApp Expr Expr
    | TEBool BoolExpr
    | TENat NatExpr
    | TEIf Expr Expr Expr


type alias Context =
    Dict Id Type


type Restriction
    = Unifies Type Type


type alias AlgorithmIRes =
    { typedExpr : TypedExpr
    , exprType : Type
    }


type alias AlgorithmICtx =
    { context : Context
    , setRestriction : Set Restriction
    , nextFreshVar : Int
    }

-- decorate : Expr -> (TypedExpr)