module LambdaParser exposing
    ( BoolExpr(..)
    , Expr(..)
    , NatExpr(..)
    , parse
    , rectify
    , rectify2
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


freeAndBoundedVars : Expr -> ( Set Id, Dict Id Int )
freeAndBoundedVars e =
    case e of
        Var id ->
            ( Set.singleton id, Dict.empty )

        Abs id expr ->
            let
                ( free, bound ) =
                    freeAndBoundedVars expr
            in
            ( Set.remove id free
            , if Dict.member id bound then
                Dict.update id (Maybe.map (\x -> x + 1)) bound

              else
                Dict.insert id 1 bound
            )

        App expr1 expr2 ->
            let
                ( free1, bound1 ) =
                    freeAndBoundedVars expr1

                ( free2, bound2 ) =
                    freeAndBoundedVars expr2
            in
            ( Set.union free1 free2, combinee bound1 bound2 )

        Bool expr ->
            freeAndBoundedBoolVars expr

        Nat expr ->
            freeAndBoundedNatVars expr

        If expr1 expr2 expr3 ->
            let
                ( free1, bound1 ) =
                    freeAndBoundedVars expr1

                ( free2, bound2 ) =
                    freeAndBoundedVars expr2

                ( free3, bound3 ) =
                    freeAndBoundedVars expr3
            in
            ( Set.union free1 (Set.union free2 free3), combinee bound1 (combinee bound2 bound3) )


combinee : Dict Id Int -> Dict Id Int -> Dict Id Int
combinee =
    Dict.foldr
        (\k v rec ->
            if Dict.member k rec then
                Dict.update k (Maybe.map (\x -> x + v)) rec

            else
                Dict.insert k v rec
        )


freeAndBoundedBoolVars : BoolExpr -> ( Set Id, Dict Id Int )
freeAndBoundedBoolVars e =
    case e of
        ConstTrue ->
            ( Set.empty, Dict.empty )

        ConstFalse ->
            ( Set.empty, Dict.empty )

        IsZero expr ->
            freeAndBoundedVars expr


freeAndBoundedNatVars : NatExpr -> ( Set Id, Dict Id Int )
freeAndBoundedNatVars e =
    case e of
        ConstZero ->
            ( Set.empty, Dict.empty )

        Succ expr ->
            freeAndBoundedVars expr

        Pred expr ->
            freeAndBoundedVars expr


renameVar : Expr -> Id -> Id -> Expr
renameVar expr oldId newId =
    let
        rec e =
            renameVar e oldId newId

        renameBoolVar bExpr =
            case bExpr of
                ConstTrue ->
                    ConstTrue

                ConstFalse ->
                    ConstFalse

                IsZero e ->
                    IsZero (rec e)

        renameNatVar nExpr =
            case nExpr of
                ConstZero ->
                    ConstZero

                Succ e ->
                    Succ (rec e)

                Pred e ->
                    Pred (rec e)
    in
    case expr of
        Var id ->
            Var
                (if id == oldId then
                    newId

                 else
                    id
                )

        Abs id expr1 ->
            Abs id
                (if id == oldId then
                    expr1

                 else
                    renameVar expr1 oldId newId
                )

        App expr1 expr2 ->
            App (rec expr1) (rec expr2)

        Bool bExpr ->
            Bool (renameBoolVar bExpr)

        Nat nExpr ->
            Nat (renameNatVar nExpr)

        If expr1 expr2 expr3 ->
            If (rec expr1) (rec expr2) (rec expr3)


rectifyHelper2 : Expr -> Set Id -> Dict Id Int -> ( Expr, Dict Id Int )
rectifyHelper2 e freeVars boundedVars =
    let
        isFreeVar : Id -> Bool
        isFreeVar vId =
            Set.member vId freeVars

        rec : Expr -> Dict Id Int -> ( Expr, Dict Id Int )
        rec expr bvs =
            rectifyHelper2 expr freeVars bvs
    in
    case e of
        Var id ->
            ( Var id, boundedVars )

        Abs id expr ->
            if (Dict.get id boundedVars == Just 1) && not (isFreeVar id) then
                rectifyHelper2 expr freeVars boundedVars
                    |> Tuple.mapFirst (Abs id)

            else
                let
                    getId nId =
                        id ++ fromInt nId

                    newId =
                        id
                            ++ fromInt
                                (until (\nId -> not (isFreeVar (getId nId)) && not (Dict.member (getId nId) boundedVars)) (\nId -> nId + 1) 1)

                    newFvs =
                        Set.insert newId freeVars

                    newBvs =
                        if Dict.get id boundedVars == Just 1 then
                            Dict.remove id boundedVars
                                |> Dict.insert newId 1

                        else
                            Dict.update id (Maybe.map (\x -> x - 1)) boundedVars
                                |> Dict.insert newId 1

                    ( renamedExpr, bvs ) =
                        rectifyHelper2 (renameVar expr id newId) newFvs newBvs
                in
                ( Abs newId renamedExpr, bvs ) |> Debug.log (Debug.toString ( boundedVars, newBvs ))

        App e1 e2 ->
            let
                ( ne1, bv1 ) =
                    rec e1 boundedVars

                ( ne2, bv2 ) =
                    rec e2 bv1
            in
            ( App ne1 ne2, bv2 )

        Bool expr ->
            let
                ( expr2, bv1 ) =
                    rectifyHelper2Bool expr freeVars boundedVars
            in
            ( Bool expr2, bv1 )

        Nat expr ->
            let
                ( expr2, bv1 ) =
                    rectifyHelper2Nat expr freeVars boundedVars
            in
            ( Nat expr2, bv1 )

        If expr1 expr2 expr3 ->
            let
                ( ne1, bv1 ) =
                    rec expr1 boundedVars

                ( ne2, bv2 ) =
                    rec expr2 bv1

                ( ne3, bv3 ) =
                    rec expr3 bv2
            in
            ( If ne1 ne2 ne3, bv3 )


rectifyHelper2Bool : BoolExpr -> Set Id -> Dict Id Int -> ( BoolExpr, Dict Id Int )
rectifyHelper2Bool e freeVars boundedVars =
    case e of
        ConstTrue ->
            ( ConstTrue, Dict.empty )

        ConstFalse ->
            ( ConstFalse, Dict.empty )

        IsZero expr ->
            Tuple.mapFirst IsZero (rectifyHelper2 expr freeVars boundedVars)


rectifyHelper2Nat : NatExpr -> Set Id -> Dict Id Int -> ( NatExpr, Dict Id Int )
rectifyHelper2Nat e freeVars boundedVars =
    case e of
        ConstZero ->
            ( ConstZero, Dict.empty )

        Succ expr ->
            Tuple.mapFirst Succ (rectifyHelper2 expr freeVars boundedVars)

        Pred expr ->
            Tuple.mapFirst Pred (rectifyHelper2 expr freeVars boundedVars)


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


rectify2 : Expr -> Expr
rectify2 e =
    let
        ( fv, bv ) =
            freeAndBoundedVars e
    in
    rectifyHelper2 e fv bv
        |> Tuple.first
        |> Debug.log
            (Debug.toString
                ( fv
                , bv
                )
            )



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
