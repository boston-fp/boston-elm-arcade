module Games.Sheep.Eff exposing (..)


type alias Eff ro rw a =
    ro -> rw -> ( a, rw )


ask : Eff ro rw ro
ask ro rw =
    ( ro, rw )


bind : (a -> Eff ro rw b) -> Eff ro rw a -> Eff ro rw b
bind f m ro rw0 =
    let
        ( a, rw1 ) =
            m ro rw0
    in
    f a ro rw1


map : (a -> b) -> Eff ro rw a -> Eff ro rw b
map f m ro rw =
    Tuple.mapFirst f (m ro rw)


map2 : (a -> b -> c) -> Eff ro rw a -> Eff ro rw b -> Eff ro rw c
map2 f ma mb ro rw0 =
    let
        ( a, rw1 ) =
            ma ro rw0

        ( b, rw2 ) =
            mb ro rw1
    in
    ( f a b, rw2 )


readOnly : (ro -> a) -> Eff ro rw a
readOnly f ro rw =
    ( f ro, rw )


run : Eff ro rw a -> ro -> rw -> ( a, rw )
run m ro rw =
    m ro rw
