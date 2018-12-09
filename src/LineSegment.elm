module LineSegment exposing (..)

import P2 exposing (P2(..))
import V2 exposing (V2)


type alias LineSegment =
    ( P2, P2 )


length : LineSegment -> Float
length ( p1, p2 ) =
    P2.distanceBetween p1 p2


lengthSquared : LineSegment -> Float
lengthSquared ( p1, p2 ) =
    P2.distanceSquaredBetween p1 p2


{-| Calculate the vector from a point to a line segment.
-}
vectorFrom : P2 -> LineSegment -> V2
vectorFrom point ( p1, p2 ) =
    let
        vp =
            P2.diff point p1

        vw =
            P2.diff p2 p1
    in
    P2.diff
        (P2.add
            P2.zero
            (V2.add
                (P2.asVector p1)
                (V2.scale
                    (max 0 (min 1 (V2.dot vp vw / V2.quadrance vw)))
                    vw
                )
            )
        )
        point
