module Type exposing (..)

import String exposing (fromInt)
import Utils exposing (maybeParens)


type Type
    = TVar Int
    | TNat
    | TBool
    | TAbs Type Type


foldType :
    (Int -> a)
    -> a
    -> a
    -> (a -> a -> a)
    -> Type
    -> a
foldType fVar fNat fBool fAbs t =
    let
        rec =
            foldType fVar fNat fBool fAbs
    in
    case t of
        TVar n ->
            fVar n

        TNat ->
            fNat

        TBool ->
            fBool

        TAbs t1 t2 ->
            fAbs (rec t1) (rec t2)


fromType : Type -> String
fromType t =
    case t of
        TVar n ->
            "X" ++ fromInt n

        TNat ->
            "Nat"

        TBool ->
            "Bool"

        TAbs t1 t2 ->
            maybeParens (fromType t1) (isAbs t1) ++ "⭢" ++ fromType t2

isAbs : Type -> Bool
isAbs t = case t of
    TAbs _ _ -> True
    _ -> False

replaceVar : Int -> Type -> Type -> Type
replaceVar n t =
    foldType
        (\varN ->
            if varN == n then
                t

            else
                TVar varN
        )
        TNat
        TBool
        TAbs


hasVar : Int -> Type -> Bool
hasVar n =
    foldType
        (\m -> n == m)
        False
        False
        (\rec1 rec2 -> rec1 || rec2)
