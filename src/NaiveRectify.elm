module NaiveRectify exposing (naiveRectify)

import LambdaParser
    exposing
        ( BoolExpr(..)
        , Expr(..)
        , Id
        , NatExpr(..)
        )
import Set exposing (Set)
import String exposing (fromInt)
import Utils exposing (until)
import Dict exposing (Dict)

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


naiveRectify : Expr -> Expr
naiveRectify e =
    let
        fv =
            freeExprVars e
    in
    rectifyHelper e fv Dict.empty 1 |> Tuple.first

