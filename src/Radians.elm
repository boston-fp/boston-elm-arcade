module Radians exposing
    ( Radians(..)
    , add
    , fromDegrees
    , toDegrees
    , unwrap
    )


type Radians
    = Radians Float


fromDegrees : Float -> Radians
fromDegrees =
    degrees >> Radians


toDegrees : Radians -> Float
toDegrees (Radians r) =
    r * 180 / pi


unwrap : Radians -> Float
unwrap (Radians r) =
    r


add : Radians -> Radians -> Radians
add (Radians r1) (Radians r2) =
    Radians (r1 + r2)
