module Radians exposing (Radians, fromDegrees, signum, toDegrees)


type alias Radians =
    Float


fromDegrees : Float -> Radians
fromDegrees =
    degrees


signum : Radians -> Float
signum r =
    if r >= 0 then
        1

    else
        -1


toDegrees : Radians -> Float
toDegrees r =
    r * 180 / pi
