module NaiveRectify exposing (naiveRectify, freeExprVars)

import Dict exposing (Dict)
import Expr exposing (Expr(..), Id, foldrExpr)
import Set exposing (Set)
import String exposing (fromInt)
import Utils exposing (until)


freeExprVars : Expr -> Set Id
freeExprVars =
    foldrExpr
        Set.singleton
        (\id rec -> Set.remove id rec)
        (\rec1 rec2 -> Set.union rec1 rec2)
        Set.empty
        Set.empty
        identity
        Set.empty
        identity
        identity
        (\rec1 rec2 rec3 -> Set.union rec1 (Set.union rec2 rec3))


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

        ConstTrue ->
            ( ConstTrue, n )

        ConstFalse ->
            ( ConstFalse, n )

        IsZero expr ->
            Tuple.mapFirst IsZero (rectifyHelper expr freeVars renames n)

        ConstZero ->
            ( ConstZero, n )

        Succ expr ->
            Tuple.mapFirst Succ (rectifyHelper expr freeVars renames n)

        Pred expr ->
            Tuple.mapFirst Pred (rectifyHelper expr freeVars renames n)

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


naiveRectify : Expr -> Expr
naiveRectify e =
    let
        fv =
            freeExprVars e
    in
    rectifyHelper e fv Dict.empty 1 |> Tuple.first
