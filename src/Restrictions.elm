module Restrictions exposing
    ( MguError(..)
    , Restriction
    , Restrictions
    , empty
    , fromMguError
    , fromRestrictions
    , insert
    , mgu
    , singleton
    , union
    )

import List
import Substitution exposing (Substitution(..), insert)
import Type exposing (Type(..), fromType, hasVar, replaceVar)


type alias Restriction =
    ( Type, Type )


type alias Restrictions =
    List Restriction


empty : Restrictions
empty =
    []


singleton : Restriction -> Restrictions
singleton r =
    [ r ]


member : Restriction -> Restrictions -> Bool
member =
    List.member


insert : Restriction -> Restrictions -> Restrictions
insert r c =
    if member r c then
        c

    else
        r :: c


union : Restrictions -> Restrictions -> Restrictions
union c1 c2 =
    c1 ++ List.filter (\x -> not <| List.member x c1) c2


type MguError
    = Clash Type Type
    | OccursCheck Type Type


fromMguError : MguError -> String
fromMguError mguErr =
    let
        errorMessage t1 t2 =
            fromType t1 ++ " y " ++ fromType t2
    in
    case mguErr of
        Clash t1 t2 ->
            "Clash/Colisión entre " ++ errorMessage t1 t2

        OccursCheck t1 t2 ->
            "OccursCheck entre " ++ errorMessage t1 t2


mgu : Restrictions -> Result MguError Substitution
mgu ys =
    case ys of
        [] ->
            Ok Substitution.empty

        ( s1, s2 ) :: xs ->
            let
                swap =
                    insert ( s2, s1 ) xs
            in
            case s1 of
                TAbs d1 d2 ->
                    case s2 of
                        TAbs t1 t2 ->
                            mgu (insert ( d1, t1 ) (insert ( d2, t2 ) xs))

                        TVar _ ->
                            mgu swap

                        _ ->
                            Err (Clash s1 s2)

                TBool ->
                    case s2 of
                        TBool ->
                            mgu xs

                        TVar _ ->
                            mgu swap

                        _ ->
                            Err (Clash s1 s2)

                TNat ->
                    case s2 of
                        TNat ->
                            mgu xs

                        TVar _ ->
                            mgu swap

                        _ ->
                            Err (Clash s1 s2)

                TVar n ->
                    let
                        replace =
                            Result.map (Substitution.insert n s2) <|
                                mgu (List.map (Tuple.mapBoth (replaceVar n s2) (replaceVar n s2)) xs)
                    in
                    case s2 of
                        TVar m ->
                            if n == m then
                                mgu xs

                            else
                                replace

                        _ ->
                            if hasVar n s2 then
                                Err (OccursCheck s1 s2)

                            else
                                replace


fromRestriction : Restriction -> String
fromRestriction ( t1, t2 ) =
    fromType t1 ++ "≟" ++ fromType t2


fromRestrictions : Restrictions -> String
fromRestrictions rs =
    let
        res =
            List.map fromRestriction rs
                |> List.intersperse ", "
                |> List.foldr (\s1 s2 -> s1 ++ s2) ""
    in
    "{" ++ res ++ "}"
