module Games.Sheep.Sheep exposing
    ( Sheep
    , SheepColor(..)
    , State(..)
    , gMaxVelocity
    , update
    , view
    )

import Collage exposing (Collage)
import Color exposing (Color)
import P2 exposing (P2)
import Radians exposing (Radians)
import Random
import V2 exposing (V2(..))


type alias Sheep =
    { pos : P2
    , vel : V2
    , mass : Float
    , food : Float
    , state : State
    , color : SheepColor
    }


type SheepColor
    = Black
    | Brown
    | White


type State
    = Flocking
    | Grazing
    | Sleeping


update : Random.Seed -> Float -> { r | pos : P2 } -> List Sheep -> Sheep -> Sheep
update seed frames doggo flock sheep =
    case sheep.state of
        Flocking ->
            updateFlocking seed frames doggo flock sheep

        Grazing ->
            updateGrazing frames doggo flock sheep

        Sleeping ->
            updateSleeping frames doggo flock sheep


updateFlocking : Random.Seed -> Float -> { r | pos : P2 } -> List Sheep -> Sheep -> Sheep
updateFlocking seed0 frames doggo flock sheep =
    let
        -- The flock within the sheep's awareness, paired with the vector to
        -- each sheep, and its quadrance.
        nearbyFlock : List ( Sheep, V2, Float )
        nearbyFlock =
            List.filterMap
                (\nearbySheep ->
                    let
                        vec =
                            P2.diff nearbySheep.pos sheep.pos

                        quadrance =
                            V2.quadrance vec
                    in
                    if quadrance <= gAwarenessRadius * gAwarenessRadius then
                        Just ( nearbySheep, vec, quadrance )

                    else
                        Nothing
                )
                flock

        -- Average velocity of a sheep in the flock.
        flockVelocity : V2
        flockVelocity =
            -- case List.head (List.sortBy (\( _, _, quadrance ) -> quadrance) nearbyFlock) of
            --     Nothing ->
            --         V2.zero
            --     Just ( nearbySheep, _, _ ) ->
            --         nearbySheep.vel
            nearbyFlock
                |> List.map (\( nearbySheep, _, _ ) -> nearbySheep.vel)
                |> V2.sum
                |> V2.scale (1 / toFloat (List.length flock))

        -- The force the flock has on a sheep. It's a bit tricky, not every
        -- nearby sheep necessarily has a force on the current one.
        --
        -- * A sheep is considered "content" if, of the sheep it is aware of
        --   (in its "awareness radius"), at least one of them is in its
        --   "comfort zone" (even if that sheep is too close for comfort, i.e.
        --   in its "personal space").
        --
        -- * If a sheep is "content", then it feels part of a flock, and is not
        --   explicitly attracted to any nearby sheep. However, it is still
        --   repelled by sheep in its "personal space".
        --
        -- * Otherwise, if a sheep is not "content", then it is attracted to any
        --   sheep it is aware of (since, per the above logic, they all must be
        --   within its "awareness radius" but outside its "personal space").
        --
        -- This logic keeps a flock of sheep from being pulled towards the
        -- center. Sheep on the edge of a flock feel just as a part of it as
        -- sheep in the center.
        flockForce : V2
        flockForce =
            let
                ( veryNearbyFlock, fringeFlock ) =
                    List.partition
                        (\( _, _, quadrance ) ->
                            quadrance <= gComfortZoneRadius * gComfortZoneRadius
                        )
                        nearbyFlock
            in
            if List.isEmpty veryNearbyFlock then
                nearbyFlock
                    |> List.map (sheepForce sheep)
                    |> V2.sum

            else
                veryNearbyFlock
                    |> List.filter
                        (\( _, _, quadrance ) ->
                            quadrance <= gPersonalSpaceRadius * gPersonalSpaceRadius
                        )
                    |> List.map (sheepForce sheep)
                    |> V2.sum

        vectorToDoggo : V2
        vectorToDoggo =
            P2.diff doggo.pos sheep.pos

        doggoForce : V2
        doggoForce =
            let
                quadrance =
                    V2.quadrance vectorToDoggo
            in
            if quadrance <= gAwarenessRadius * gAwarenessRadius then
                vectorToDoggo
                    -- TODO name this magic number
                    |> V2.scale (300 / quadrance)
                    |> V2.negate

            else
                V2.zero

        newVelocity : V2
        newVelocity =
            let
                noise : V2
                noise =
                    Tuple.first
                        (Random.step
                            (Random.map2 V2
                                (Random.float -0.25 0.25)
                                (Random.float -0.25 0.25)
                            )
                            seed0
                        )
            in
            sheep.vel
                |> V2.add flockVelocity
                |> V2.add flockForce
                |> V2.add doggoForce
                |> V2.add noise
                |> V2.clampNorm gMinVelocity gMaxVelocity

        newVelocity2 =
            let
                theta =
                    V2.angleBetween sheep.vel newVelocity
            in
            -- Unhappy path: the sheep's new velocity is too great an angle away
            -- from its previous velocity. So un-rotate the new velocity, then
            -- re-rotate it in the same direction, but only 'gTurnRate' radians.
            if abs theta > gTurnRate then
                V2.rotate
                    (-theta + Radians.signum theta * gTurnRate)
                    newVelocity

            else
                newVelocity
    in
    { sheep
        | vel = newVelocity2
        , pos = P2.add sheep.pos (V2.scale frames sheep.vel)
        , food = sheep.food - (gFoodLossRate * frames)
    }


{-| Calculate the force one sheep has on another:

    * If the sheep are within the "personal space", they repel
    * Otherwise, they attract

-}
sheepForce : Sheep -> ( Sheep, V2, Float ) -> V2
sheepForce sheep ( otherSheep, diff, quadrance ) =
    if quadrance <= gPersonalSpaceRadius * gPersonalSpaceRadius then
        diff
            |> V2.scale (20 / quadrance)
            |> V2.negate

    else
        diff
            |> V2.scale (20 / quadrance)


updateGrazing : Float -> { r | pos : P2 } -> List Sheep -> Sheep -> Sheep
updateGrazing frames doggo flock sheep =
    if sheep.food > 1 then
        { sheep | state = Flocking }

    else
        { sheep | food = sheep.food + frames * gFoodGainRate }


updateSleeping : Float -> { r | pos : P2 } -> List Sheep -> Sheep -> Sheep
updateSleeping frames doggo flock sheep =
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
            |> Collage.filled
                (Collage.uniform
                    (case sheep.color of
                        White ->
                            Color.rgb 220 220 220

                        Black ->
                            Color.rgb 40 40 40

                        Brown ->
                            Color.rgb 139 69 19
                    )
                )
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
    3


gMinVelocity : Float
gMinVelocity =
    0.5


{-| Radians per frame
-}
gTurnRate : Radians
gTurnRate =
    0.05


{-| How far a sheep is aware of its surroundings.
-}
gAwarenessRadius : Float
gAwarenessRadius =
    400


{-| Sheep prefer to be within this many units of at least one other sheep.
-}
gComfortZoneRadius : Float
gComfortZoneRadius =
    300


{-| Sheep inside each others' personal space repel each other.
-}
gPersonalSpaceRadius : Float
gPersonalSpaceRadius =
    40


gFoodLossRate : Float
gFoodLossRate =
    0.002


gFoodGainRate : Float
gFoodGainRate =
    0.02
