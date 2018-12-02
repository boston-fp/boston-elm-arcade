module Games.Sheep.Sheep exposing (Sheep, State(..), update, view)

import Collage exposing (Collage)
import Color exposing (Color)
import P2 exposing (P2)
import Radians exposing (Radians)
import V2 exposing (V2)


type alias Sheep =
    { pos : P2
    , vel : V2
    , mass : Float
    , food : Float
    , state : State
    }


type State
    = Flocking
    | Grazing
    | Sleeping


update : Float -> List Sheep -> Sheep -> Sheep
update frames flock sheep =
    case sheep.state of
        Flocking ->
            updateFlocking frames flock sheep

        Grazing ->
            updateGrazing frames flock sheep

        Sleeping ->
            updateSleeping frames flock sheep


updateFlocking : Float -> List Sheep -> Sheep -> Sheep
updateFlocking frames flock sheep =
    let
        -- The flock within the sheep's awareness.
        nearbyFlock : List Sheep
        nearbyFlock =
            List.filter
                (\nearbySheep ->
                    P2.distanceBetween sheep.pos nearbySheep.pos
                        <= gAwarenessRadius
                )
                flock

        -- Average velocity of a sheep in the flock.
        flockVelocity : V2
        flockVelocity =
            nearbyFlock
                |> List.foldl (.vel >> V2.add) V2.zero
                |> V2.scale (1 / toFloat (List.length flock))

        flockForce : V2
        flockForce =
            nearbyFlock
                |> List.map
                    (\otherSheep ->
                        let
                            diff =
                                P2.diff otherSheep.pos sheep.pos

                            norm =
                                V2.norm diff
                        in
                        if norm <= gPersonalSpaceRadius then
                            (V2.negate (V2.signorm diff))
                            -- V2.scale (-gForce / (norm * norm)) diff

                        else if norm <= gComfortZoneRadius then
                            V2.zero

                        else
                            (V2.signorm diff)
                     -- V2.scale (gForce / (norm * norm)) diff
                    )
                |> V2.sum
                |> V2.scale (1 / toFloat (List.length flock))
    in
    { sheep
        | vel =
            V2.lerp 0.5 sheep.vel flockVelocity
                |> V2.add flockForce
                |> V2.maxNorm gMaxVelocity
        , pos = P2.add sheep.pos (V2.scale frames sheep.vel)
        , food = sheep.food - (gFoodLossRate * frames)
    }


updateGrazing : Float -> List Sheep -> Sheep -> Sheep
updateGrazing frames flock sheep =
    if sheep.food > 1 then
        { sheep | state = Flocking }

    else
        { sheep | food = sheep.food + frames * gFoodGainRate }


updateSleeping : Float -> List Sheep -> Sheep -> Sheep
updateSleeping frames flock sheep =
    sheep



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Sheep -> Collage msg
view sheep =
    let
        color =
            case sheep.state of
                Flocking ->
                    Color.rgb 255 0 0

                Grazing ->
                    Color.rgb 0 255 0

                Sleeping ->
                    Color.rgb 0 0 255

        radii c =
            Collage.group
                [ Collage.circle gAwarenessRadius
                    |> Collage.outlined (Collage.dot Collage.thin (Collage.uniform Color.black))
                , Collage.circle gComfortZoneRadius
                    |> Collage.outlined (Collage.dot Collage.thin (Collage.uniform Color.green))
                , Collage.circle gPersonalSpaceRadius
                    |> Collage.outlined (Collage.dot Collage.thin (Collage.uniform Color.red))
                , c
                ]
    in
    Collage.group
        [ Collage.rectangle
            4
            4
            |> Collage.filled (Collage.uniform color)
        , Collage.rectangle
            36
            24
            |> Collage.filled (Collage.uniform (Color.rgb 220 220 220))
        , Collage.rectangle
            8
            8
            |> Collage.filled (Collage.uniform (Color.rgb 20 20 20))
            |> Collage.shift ( 20, 0 )
        ]
        |> Collage.scale sheep.mass
        |> (if False then
                radii

            else
                identity
           )
        |> Collage.rotate (V2.toRadians sheep.vel)
        |> Collage.shift ( P2.x sheep.pos, P2.y sheep.pos )



--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------


{-| Velocity per frame
-}
gMaxVelocity : Float
gMaxVelocity =
    1


{-| Radians per frame
-}
gTurnRate : Radians
gTurnRate =
    0.01


{-| How far a sheep is aware of its surroundings.
-}
gAwarenessRadius : Float
gAwarenessRadius =
    400


{-| Sheep prefer to be within this many units of other sheep.
-}
gComfortZoneRadius : Float
gComfortZoneRadius =
    300


gForce : Float
gForce =
    0.1


{-| Sheep inside each others' personal space repel each other.
-}
gPersonalSpaceRadius : Float
gPersonalSpaceRadius =
    100


gFoodLossRate : Float
gFoodLossRate =
    0.002


gFoodGainRate : Float
gFoodGainRate =
    0.02
