module V2 exposing (..)

import Radians exposing (Radians)


type V2
    = V2 Float Float


add : V2 -> V2 -> V2
add (V2 vx vy) (V2 wx wy) =
    V2 (vx + wx) (vy + wy)


aligned : V2 -> V2 -> Bool
aligned v1 v2 =
    dot v1 v2 >= 0


{-| 'angleBetween v w' calculates the angle that 'v' must rotate by to be
parallel to 'w'. Range: [-pi, pi]
-}
angleBetween : V2 -> V2 -> Radians
angleBetween (V2 vx vy) (V2 wx wy) =
    let
        angle =
            atan2 wy wx - atan2 vy vx
    in
    if angle < -pi then
        angle + 2 * pi

    else if angle > pi then
        angle - 2 * pi

    else
        angle


clampNorm : Float -> Float -> V2 -> V2
clampNorm min max =
    minNorm min >> maxNorm max


isBetween : V2 -> V2 -> V2 -> Bool
isBetween v1 v2 v =
    let
        v1u =
            signorm v1

        v2u =
            signorm v2
    in
    if v1u == v2u then
        signorm v == v1u

    else if v1u == negate v2u then
        True

    else
        let
            vv1 =
                dot v v1

            vv2 =
                dot v v2

            v1v2 =
                dot v1 v2
        in
        (vv1 * quadrance v2 >= vv2 * v1v2)
            && (vv2 * quadrance v1 >= vv1 * v1v2)


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
fromRadians r =
    V2 (cos r) (sin r)


isZero : V2 -> Bool
isZero (V2 vx vy) =
    vx == 0 && vy == 0


lerp : Float -> V2 -> V2 -> V2
lerp a v w =
    add (scale a v) (scale (1 - a) w)


{-| Scale a vector back if its norm is greater than the given value. (Useful to
implement e.g. maximum velocity).
-}
maxNorm : Float -> V2 -> V2
maxNorm n v =
    let
        q =
            quadrance v
    in
    if q > n * n then
        scale (n / sqrt q) v

    else
        v


{-| Scale a vector up if its norm is less than the given value. (Useful to
implement e.g. minimum velocity).
-}
minNorm : Float -> V2 -> V2
minNorm n v =
    let
        q =
            quadrance v
    in
    if q < n * n then
        scale (n / sqrt q) v

    else
        v


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


perpClockwise : V2 -> V2
perpClockwise (V2 vx vy) =
    V2 vy -vx


pow : Float -> V2 -> V2
pow p (V2 vx vy) =
    V2 (vx ^ p) (vy ^ p)


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
rotate r (V2 vx vy) =
    let
        c =
            cos r

        s =
            sin r
    in
    V2 (c * vx - s * vy) (s * vx + c * vy)


scale : Float -> V2 -> V2
scale s (V2 vx vy) =
    V2 (s * vx) (s * vy)


{-| Convert a non-zero vector to a unit vector.
-}
signorm : V2 -> V2
signorm v =
    scale (1 / norm v) v


sum : List V2 -> V2
sum =
    List.foldl add zero


toDegrees : V2 -> Float
toDegrees =
    toRadians >> Radians.toDegrees


toRadians : V2 -> Radians
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
