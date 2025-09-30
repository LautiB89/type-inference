module Utils exposing (until)

until : (a -> Bool) -> (a -> a) -> a -> a
until p f z =
    if p z then
        z

    else
        until p f (f z)