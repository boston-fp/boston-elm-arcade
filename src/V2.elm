module V2 exposing (V2(..), add, diff, dot, fromDegrees, fromRadians, lerp, negate, norm, overX, overY, project, quadrance, scale, signorm, toDegrees, toRadians, x, y, zero)

import Radians exposing (Radians(..))


type V2
    = V2 Float Float


add : V2 -> V2 -> V2
add (V2 vx vy) (V2 wx wy) =
    V2 (vx + wx) (vy + wy)


diff : V2 -> V2 -> V2
diff (V2 vx vy) (V2 wx wy) =
    V2 (vx - wx) (vy - wy)


dot : V2 -> V2 -> Float
dot (V2 vx vy) (V2 wx wy) =
    vx * wx + vy * wy


fromDegrees : Float -> V2
fromDegrees =
    Radians.fromDegrees >> fromRadians


fromRadians : Radians -> V2
fromRadians (Radians r) =
    V2 (cos r) (sin r)


lerp : Float -> V2 -> V2 -> V2
lerp a v w =
    add (scale a v) (scale (1 - a) w)


negate : V2 -> V2
negate (V2 vx vy) =
    V2 -vx -vy


{-| Compute the norm (magnitude).
-}
norm : V2 -> Float
norm =
    quadrance >> sqrt


overX : (Float -> Float) -> V2 -> V2
overX f (V2 vx vy) =
    V2 (f vx) vy


overY : (Float -> Float) -> V2 -> V2
overY f (V2 vx vy) =
    V2 vx (f vy)


{-| 'project v w' computes the projection of 'w' onto 'v'.
-}
project : V2 -> V2 -> V2
project v w =
    scale (dot v w / quadrance v) v


{-| Compute the squared norm.
-}
quadrance : V2 -> Float
quadrance v =
    dot v v


{-| Rotate a vector.
-}
rotate : Radians -> V2 -> V2
rotate (Radians r) (V2 vx vy) =
    let
        c =
            cos r

        s =
            sin r
    in
    V2 (c * vx - s * vy) (s * vx - c * vy)


scale : Float -> V2 -> V2
scale s (V2 vx vy) =
    V2 (s * vx) (s * vy)


{-| Convert a non-zero vector to a unit vector.
-}
signorm : V2 -> V2
signorm v =
    scale (1 / norm v) v


toDegrees : V2 -> Float
toDegrees =
    let
        deg2rad deg =
            deg * 180 / pi
    in
    toRadians >> deg2rad


toRadians : V2 -> Float
toRadians (V2 vx vy) =
    atan2 vy vx


x : V2 -> Float
x (V2 vx _) =
    vx


y : V2 -> Float
y (V2 _ vy) =
    vy


zero : V2
zero =
    V2 0 0
