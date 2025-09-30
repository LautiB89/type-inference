module Multiset exposing
    ( Multiset
    , count
    , empty
    , insert
    , member
    , remove
    , union
    )

import Dict exposing (Dict)


type Multiset comparable
    = Multiset (Dict comparable Int)


empty : Multiset comparable
empty =
    Multiset Dict.empty


member : comparable -> Multiset comparable -> Bool
member x (Multiset d) =
    Dict.member x d


insert : comparable -> Multiset comparable -> Multiset comparable
insert x (Multiset d) =
    if Dict.member x d then
        Multiset (Dict.update x (Maybe.map (\n -> n + 1)) d)

    else
        Multiset (Dict.insert x 1 d)


remove : comparable -> Multiset comparable -> Multiset comparable
remove x (Multiset d) =
    if Dict.member x d then
        if Dict.get x d == Just 1 then
            Multiset (Dict.remove x d)

        else
            Multiset (Dict.update x (Maybe.map (\n -> n - 1)) d)

    else
        Multiset d


count : comparable -> Multiset comparable -> Int
count x (Multiset d) =
    Dict.get x d
        |> Maybe.withDefault 0


union : Multiset comparable -> Multiset comparable -> Multiset comparable
union (Multiset d1) (Multiset d2) =
    Multiset
        (Dict.foldr
            (\k v rec ->
                if Dict.member k rec then
                    Dict.update k (Maybe.map (\x -> x + v)) rec

                else
                    Dict.insert k v rec
            )
            d1
            d2
        )
