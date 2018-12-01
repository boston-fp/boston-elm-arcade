module Radians exposing
    ( Radians(..)
    , abs
    , add
    , fromDegrees
    , gte
    , mult
    , signum
    , toDegrees
    , unwrap
    )


type Radians
    = Radians Float


abs : Radians -> Radians
abs (Radians r) =
    Radians (Basics.abs r)


add : Radians -> Radians -> Radians
add (Radians r1) (Radians r2) =
    Radians (r1 + r2)


fromDegrees : Float -> Radians
fromDegrees =
    degrees >> Radians


gte : Radians -> Radians -> Bool
gte (Radians r) (Radians s) =
    r >= s


signum : Radians -> Float
signum (Radians r) =
    if r >= 0 then
        1

    else
        0


mult : Radians -> Radians -> Radians
mult (Radians r1) (Radians r2) =
    Radians (r1 * r2)


toDegrees : Radians -> Float
toDegrees (Radians r) =
    r * 180 / pi


unwrap : Radians -> Float
unwrap (Radians r) =
    r
