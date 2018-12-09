module Games.Sheep.Fence exposing (..)

import LineSegment exposing (LineSegment)
import P2 exposing (P2(..))
import V2 exposing (V2(..))


type alias Fence =
    LineSegment


forceOn : { r | pos : P2 } -> Fence -> V2
forceOn thing fence =
    let
        vec =
            V2.negate (LineSegment.vectorFrom thing.pos fence)

        quadrance =
            V2.quadrance vec
    in
    if quadrance > gFenceForceDistance * gFenceForceDistance then
        V2.zero

    else
        V2.scale (gFenceForce / quadrance) vec


{-| How much the fence repels.
-}
gFenceForce : Float
gFenceForce =
    100


{-| Maximum distance the fence force is felt at.
-}
gFenceForceDistance : Float
gFenceForceDistance =
    50
