module Restrictions exposing
    ( Restrictions
    , Substitution
    , empty
    , fromRestrictions
    , insert
    , member
    , mgu
    , remove
    , singleton
    , union
    )

import List
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


remove : Restriction -> Restrictions -> Restrictions
remove r c =
    List.filter (\r2 -> r /= r2) c


union : Restrictions -> Restrictions -> Restrictions
union c1 c2 =
    c1 ++ List.filter (\x -> not <| List.member x c1) c2



-- Restrictions solver


type alias Substitution =
    Int -> Type


mgu : Restrictions -> Result String Substitution
mgu ys =
    case ys of
        [] ->
            Ok (\n -> TVar n)

        ( s1, s2 ) :: xs ->
            case s1 of
                TAbs d1 d2 ->
                    case s2 of
                        TAbs t1 t2 ->
                            mgu (insert ( d1, t1 ) (insert ( d2, t2 ) xs))

                        TVar _ ->
                            mgu (insert ( s2, s1 ) xs)

                        _ ->
                            Err "Clash"

                TBool ->
                    case s2 of
                        TBool ->
                            mgu xs

                        TVar _ ->
                            mgu (insert ( s2, s1 ) xs)

                        _ ->
                            Err "Clash"

                TNat ->
                    case s2 of
                        TNat ->
                            mgu xs

                        TVar _ ->
                            mgu (insert ( s2, s1 ) xs)

                        _ ->
                            Err "Clash"

                TVar n ->
                    case s2 of
                        TVar m ->
                            if n == m then
                                mgu xs

                            else
                                mgu (insert ( s2, s1 ) xs)

                        _ ->
                            if hasVar n s2 then
                                Err "Occurs check"

                            else
                                mgu (List.map (Tuple.mapBoth (replaceVar n s2) (replaceVar n s2)) xs)
                                    |> Result.map
                                        (\s ->
                                            \i ->
                                                if i == n then
                                                    s2

                                                else
                                                    s i
                                        )



-- Show


fromRestriction : Restriction -> String
fromRestriction ( t1, t2 ) =
    fromType t1 ++ "=" ++ fromType t2


fromRestrictions : Restrictions -> String
fromRestrictions rs =
    let
        res =
            List.map fromRestriction rs
                |> List.intersperse ", "
                |> List.foldr (\s1 s2 -> s1 ++ s2) ""
    in
    "{" ++ res ++ "}"
