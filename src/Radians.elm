module Radians exposing (Radians(..), fromDegrees)


type Radians
    = Radians Float


fromDegrees : Float -> Radians
fromDegrees =
    degrees >> Radians
