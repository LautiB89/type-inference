module Substitution exposing (..)

import Type
    exposing
        ( Type(..)
        , foldType
        , fromType
        )


type Substitution
    = Substitution (Int -> Type)


simplifySubstitution : Substitution -> Substitution
simplifySubstitution s =
    Substitution <|
        \n ->
            let
                t =
                    apply s n
            in
            case t of
                TNat ->
                    TNat

                TBool ->
                    TBool

                TVar m ->
                    if m == n then
                        TVar n

                    else
                        apply s m

                TAbs t1 t2 ->
                    TAbs (substitute s t1) (substitute s t2)


substitute : Substitution -> Type -> Type
substitute (Substitution s) =
    foldType s TNat TBool TAbs


apply : Substitution -> Int -> Type
apply (Substitution s) n =
    s n


insert : Int -> Type -> Substitution -> Substitution
insert n t (Substitution s) =
    Substitution
        (\i ->
            if i == n then
                t

            else
                s i
        )


fromSubstitution : Substitution -> Int -> String
fromSubstitution (Substitution s) n =
    let
        res =
            List.range 1 (n - 1)
                |> List.filter (\k -> TVar k /= s k)
                |> List.map (\k -> fromType (TVar k) ++ "≔" ++ fromType (s k))
                |> List.intersperse ", "
                |> List.foldr (\s1 s2 -> s1 ++ s2) ""
    in
    "{" ++ res ++ "}"
