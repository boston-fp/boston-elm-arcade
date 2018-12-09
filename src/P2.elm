module P2 exposing
    ( P2(..)
    , add
    , asTuple
    , asVector
    , diff
    , distanceBetween
    , distanceSquaredBetween
    , x
    , y
    , zero
    )

import V2 exposing (V2(..))


type P2
    = P2 Float Float


add : P2 -> V2 -> P2
add (P2 px py) (V2 vx vy) =
    P2 (px + vx) (py + vy)


asVector : P2 -> V2
asVector (P2 px py) =
    V2 px py


asTuple : P2 -> ( Float, Float )
asTuple (P2 ex why) =
    ( ex, why )


diff : P2 -> P2 -> V2
diff (P2 px py) (P2 qx qy) =
    V2 (px - qx) (py - qy)


distanceBetween : P2 -> P2 -> Float
distanceBetween p q =
    V2.norm (diff p q)


distanceSquaredBetween : P2 -> P2 -> Float
distanceSquaredBetween p q =
    V2.quadrance (diff p q)


x : P2 -> Float
x (P2 px _) =
    px


y : P2 -> Float
y (P2 _ py) =
    py


zero : P2
zero =
    P2 0 0
