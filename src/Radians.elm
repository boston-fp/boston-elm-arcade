module Radians exposing (Radians(..), fromDegrees, toDegrees, unwrap)


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
