module Utils exposing (lift, lift2, lift3, maybeParens, until)


until : (a -> Bool) -> (a -> a) -> a -> a
until p f z =
    if p z then
        z

    else
        until p f (f z)


maybeParens : String -> Bool -> String
maybeParens s b =
    if b then
        "(" ++ s ++ ")"

    else
        s


lift :
    (a -> Int -> ( Maybe b, Int ))
    -> (b -> c)
    -> (a -> Int -> ( Maybe c, Int ))
lift f g x n =
    let
        ( res1, n1 ) =
            f x n
    in
    ( Maybe.map g res1, n1 )


lift2 :
    (a -> Int -> ( Maybe b, Int ))
    -> (b -> b -> c)
    -> (a -> a -> Int -> ( Maybe c, Int ))
lift2 f g x y n =
    let
        ( res1, n1 ) =
            f x n

        ( res2, n2 ) =
            f y n1
    in
    ( Maybe.map2 g res1 res2, n2 )


lift3 :
    (a -> Int -> ( Maybe b, Int ))
    -> (b -> b -> b -> c)
    -> (a -> a -> a -> Int -> ( Maybe c, Int ))
lift3 f g x y z n =
    let
        ( res1, n1 ) =
            f x n

        ( res2, n2 ) =
            f y n1

        ( res3, n3 ) =
            f z n2
    in
    ( Maybe.map3 g res1 res2 res3, n3 )
