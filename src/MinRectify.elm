module MinRectify exposing (minRectify)

import Expr exposing (Expr(..), Id, foldrExpr, recrExpr)
import Multiset exposing (Multiset)
import Set exposing (Set)
import String exposing (fromInt)
import Utils exposing (until)


freeAndBoundVars : Expr -> ( Set Id, Multiset Id )
freeAndBoundVars =
    foldrExpr
        (\id -> ( Set.singleton id, Multiset.empty ))
        (\id ( free, bound ) ->
            ( Set.remove id free, Multiset.insert id bound )
        )
        (\( free1, bound1 ) ( free2, bound2 ) ->
            ( Set.union free1 free2, Multiset.union bound1 bound2 )
        )
        ( Set.empty, Multiset.empty )
        ( Set.empty, Multiset.empty )
        identity
        ( Set.empty, Multiset.empty )
        identity
        identity
        (\( free1, bound1 ) ( free2, bound2 ) ( free3, bound3 ) ->
            ( Set.union free1 (Set.union free2 free3), Multiset.union bound1 (Multiset.union bound2 bound3) )
        )


renameVar : Id -> Id -> Expr -> Expr
renameVar oldId newId =
    let
        ifIsOldId id a b =
            if id == oldId then
                a

            else
                b
    in
    recrExpr
        (\id -> Var (ifIsOldId id newId id))
        (\id e1 rec1 -> Abs id (ifIsOldId id e1 rec1))
        (\_ rec1 _ rec2 -> App rec1 rec2)
        ConstTrue
        ConstFalse
        (\_ -> IsZero)
        ConstZero
        (\_ -> Succ)
        (\_ -> Pred)
        (\_ rec1 _ rec2 _ rec3 -> If rec1 rec2 rec3)


rectifyHelper : Expr -> Set Id -> Multiset Id -> ( Expr, Multiset Id )
rectifyHelper e freeVars boundVars =
    let
        isFreeVar : Id -> Bool
        isFreeVar vId =
            Set.member vId freeVars

        rec : Expr -> Multiset Id -> ( Expr, Multiset Id )
        rec expr bvs =
            rectifyHelper expr freeVars bvs
    in
    case e of
        Var id ->
            ( Var id, boundVars )

        Abs id expr ->
            if Multiset.count id boundVars == 1 && not (isFreeVar id) then
                Tuple.mapFirst (Abs id) <|
                    rectifyHelper expr freeVars boundVars

            else
                let
                    getId nId =
                        id ++ fromInt nId

                    newId =
                        id
                            ++ fromInt
                                (until
                                    (\nId -> not (isFreeVar (getId nId)) && not (Multiset.member (getId nId) boundVars))
                                    (\nId -> nId + 1)
                                    1
                                )

                    newFvs =
                        Set.insert newId freeVars

                    newBvs =
                        Multiset.remove id boundVars
                            |> Multiset.insert newId

                    ( renamedExpr, bvs ) =
                        rectifyHelper (renameVar id newId expr) newFvs newBvs
                in
                ( Abs newId renamedExpr, bvs )

        App e1 e2 ->
            let
                ( ne1, bv1 ) =
                    rec e1 boundVars

                ( ne2, bv2 ) =
                    rec e2 bv1
            in
            ( App ne1 ne2, bv2 )

        ConstTrue ->
            ( ConstTrue, Multiset.empty )

        ConstFalse ->
            ( ConstFalse, Multiset.empty )

        IsZero expr ->
            Tuple.mapFirst IsZero (rectifyHelper expr freeVars boundVars)

        ConstZero ->
            ( ConstZero, Multiset.empty )

        Succ expr ->
            Tuple.mapFirst Succ (rectifyHelper expr freeVars boundVars)

        Pred expr ->
            Tuple.mapFirst Pred (rectifyHelper expr freeVars boundVars)

        If expr1 expr2 expr3 ->
            let
                ( ne1, bv1 ) =
                    rec expr1 boundVars

                ( ne2, bv2 ) =
                    rec expr2 bv1

                ( ne3, bv3 ) =
                    rec expr3 bv2
            in
            ( If ne1 ne2 ne3, bv3 )


minRectify : Expr -> Expr
minRectify e =
    let
        ( fv, bv ) =
            freeAndBoundVars e
    in
    Tuple.first <|
        rectifyHelper e fv bv
