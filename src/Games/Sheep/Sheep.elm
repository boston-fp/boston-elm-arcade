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
        -- The herd within the sheep's awareness.
        nearbyHerd : List Sheep
        nearbyHerd =
            List.filter
                (\nearbySheep ->
                    P2.distanceBetween sheep.pos nearbySheep.pos
                        <= gAwarenessRadius
                )
                flock

        herdVelocity : V2
        herdVelocity =
            List.foldl
                (.vel >> V2.add)
                V2.zero
                nearbyHerd

        angleToHerdVelocity : Radians
        angleToHerdVelocity =
            V2.angleBetween sheep.vel herdVelocity

        angleToRotate : Radians
        angleToRotate =
            let
                angle =
                    frames * gTurnRate
            in
            -- Avoid over-rotation: if the sheep can rotate to become
            -- parallel with the flock this frame, then do so
            if angle >= abs angleToHerdVelocity then
                angleToHerdVelocity

            else
                Radians.signum angleToHerdVelocity * angle
    in
    { sheep
        | vel =
            V2.rotate angleToRotate sheep.vel
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



-- {-| Calculate a sheep's velocity, as a pure of inputs. Currently,
-- that's just the doggo (but in the future will include the other shep).
-- -}
-- calculateSheepVelocity : Doggo -> Sheep -> V2
-- calculateSheepVelocity doggo shep =
--     repel doggo shep
--         |> V2.scale (100 / shep.mass)
--         |> V2.maxNorm maxSheepVelocity
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


gAwarenessRadius : Float
gAwarenessRadius =
    100


gFoodLossRate : Float
gFoodLossRate =
    0.002


gFoodGainRate : Float
gFoodGainRate =
    0.02
