module MinRectify exposing (minRectify)

import LambdaParser
    exposing
        ( BoolExpr(..)
        , Expr(..)
        , Id
        , NatExpr(..)
        )
import Multiset exposing (Multiset)
import Set exposing (Set)
import String exposing (fromInt)
import Utils exposing (until)


freeAndBoundedVars : Expr -> ( Set Id, Multiset Id )
freeAndBoundedVars e =
    case e of
        Var id ->
            ( Set.singleton id, Multiset.empty )

        Abs id expr ->
            let
                ( free, bound ) =
                    freeAndBoundedVars expr
            in
            ( Set.remove id free
            , Multiset.insert id bound
            )

        App expr1 expr2 ->
            let
                ( free1, bound1 ) =
                    freeAndBoundedVars expr1

                ( free2, bound2 ) =
                    freeAndBoundedVars expr2
            in
            ( Set.union free1 free2, Multiset.union bound1 bound2 )

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
            ( Set.union free1 (Set.union free2 free3), Multiset.union bound1 (Multiset.union bound2 bound3) )


freeAndBoundedBoolVars : BoolExpr -> ( Set Id, Multiset Id )
freeAndBoundedBoolVars e =
    case e of
        ConstTrue ->
            ( Set.empty, Multiset.empty )

        ConstFalse ->
            ( Set.empty, Multiset.empty )

        IsZero expr ->
            freeAndBoundedVars expr


freeAndBoundedNatVars : NatExpr -> ( Set Id, Multiset Id )
freeAndBoundedNatVars e =
    case e of
        ConstZero ->
            ( Set.empty, Multiset.empty )

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


rectifyHelper2 : Expr -> Set Id -> Multiset Id -> ( Expr, Multiset Id )
rectifyHelper2 e freeVars boundedVars =
    let
        isFreeVar : Id -> Bool
        isFreeVar vId =
            Set.member vId freeVars

        rec : Expr -> Multiset Id -> ( Expr, Multiset Id )
        rec expr bvs =
            rectifyHelper2 expr freeVars bvs
    in
    case e of
        Var id ->
            ( Var id, boundedVars )

        Abs id expr ->
            if Multiset.count id boundedVars == 1 && not (isFreeVar id) then
                rectifyHelper2 expr freeVars boundedVars
                    |> Tuple.mapFirst (Abs id)

            else
                let
                    getId nId =
                        id ++ fromInt nId

                    newId =
                        id
                            ++ fromInt
                                (until (\nId -> not (isFreeVar (getId nId)) && not (Multiset.member (getId nId) boundedVars)) (\nId -> nId + 1) 1)

                    newFvs =
                        Set.insert newId freeVars

                    newBvs =
                        Multiset.remove id boundedVars
                            |> Multiset.insert newId

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


rectifyHelper2Bool : BoolExpr -> Set Id -> Multiset Id -> ( BoolExpr, Multiset Id )
rectifyHelper2Bool e freeVars boundedVars =
    case e of
        ConstTrue ->
            ( ConstTrue, Multiset.empty )

        ConstFalse ->
            ( ConstFalse, Multiset.empty )

        IsZero expr ->
            Tuple.mapFirst IsZero (rectifyHelper2 expr freeVars boundedVars)


rectifyHelper2Nat : NatExpr -> Set Id -> Multiset Id -> ( NatExpr, Multiset Id )
rectifyHelper2Nat e freeVars boundedVars =
    case e of
        ConstZero ->
            ( ConstZero, Multiset.empty )

        Succ expr ->
            Tuple.mapFirst Succ (rectifyHelper2 expr freeVars boundedVars)

        Pred expr ->
            Tuple.mapFirst Pred (rectifyHelper2 expr freeVars boundedVars)


minRectify : Expr -> Expr
minRectify e =
    let
        ( fv, bv ) =
            freeAndBoundedVars e
    in
    rectifyHelper2 e fv bv
        |> Tuple.first
