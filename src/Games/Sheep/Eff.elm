module Games.Sheep.Eff exposing (..)

import Random


{-| An "effect" abstraction with a read-only and read-write component.

    Use case for read-only component: threading a boring, boilerplate "context"
    through a function, such as the number of frames elapsed since the last
    update.

    Use case for read-write component: threading a boring, boilerplate,
    *stateful* "context" through a function, such as a random seed.

-}
type alias Eff ro rw a =
    ro -> rw -> ( a, rw )


ap : Eff ro rw a -> Eff ro rw (a -> b) -> Eff ro rw b
ap ma mf ro rw0 =
    let
        ( f, rw1 ) =
            mf ro rw0

        ( a, rw2 ) =
            ma ro rw1
    in
    ( f a, rw2 )


ask : Eff ro rw ro
ask ro rw =
    ( ro, rw )


asks : (ro -> a) -> Eff ro rw a
asks f ro rw =
    ( f ro, rw )


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


map3 :
    (a -> b -> c -> d)
    -> Eff ro rw a
    -> Eff ro rw b
    -> Eff ro rw c
    -> Eff ro rw d
map3 f ma mb mc ro rw0 =
    let
        ( a, rw1 ) =
            ma ro rw0

        ( b, rw2 ) =
            mb ro rw1

        ( c, rw3 ) =
            mc ro rw2
    in
    ( f a b c, rw3 )


pure : a -> Eff ro rw a
pure x _ rw =
    ( x, rw )


random : Random.Generator a -> Eff ro { rw | seed : Random.Seed } a
random gen ro rw =
    let
        ( a, seed ) =
            Random.step gen rw.seed
    in
    ( a, { rw | seed = seed } )


readOnly : (ro -> a) -> Eff ro rw a
readOnly f ro rw =
    ( f ro, rw )


readWrite : (rw -> ( a, rw )) -> Eff ro rw a
readWrite f _ rw =
    f rw


replicate : Int -> Eff ro rw a -> Eff ro rw (List a)
replicate n m =
    if n == 0 then
        pure []

    else
        map2 (::) m (replicate (n - 1) m)


sequenceList : List (Eff ro rw a) -> Eff ro rw (List a)
sequenceList xs =
    case xs of
        [] ->
            pure []

        y :: ys ->
            map2 (::) y (sequenceList ys)


run : Eff ro rw a -> ro -> rw -> ( a, rw )
run m ro rw =
    m ro rw
