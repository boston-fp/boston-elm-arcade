module Games.Sheep.Sheep exposing
    ( Sheep
    , SheepColor(..)
    , State(..)
    , update
    , view
    )

import Collage exposing (Collage)
import Color exposing (Color)
import Games.Sheep.Eff as Eff exposing (Eff)
import Games.Sheep.Fence as Fence exposing (Fence)
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


type alias Config r =
    { r
        | maxSheepVelocity : Float
        , minSheepVelocity : Float
        , sheepTurnRate : Float
    }


update :
    Config r1
    -> List Fence
    -> { r2 | pos : P2 }
    -> List Sheep
    -> Sheep
    -> Eff { ro | frames : Float } { rw | seed : Random.Seed } Sheep
update config fences doggo flock sheep =
    case sheep.state of
        Flocking ->
            updateFlocking config fences doggo flock sheep

        Grazing ->
            Debug.todo ""

        Sleeping ->
            Debug.todo ""


updateFlocking :
    Config r1
    -> List Fence
    -> { r2 | pos : P2 }
    -> List Sheep
    -> Sheep
    -> Eff { ro | frames : Float } { rw | seed : Random.Seed } Sheep
updateFlocking config fences doggo flock sheep =
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
    in
    updateFlocking_ config fences doggo nearbyFlock sheep


updateFlocking_ :
    Config r1
    -> List Fence
    -> { r2 | pos : P2 }
    -> List ( Sheep, V2, Float )
    -> Sheep
    -> Eff { ro | frames : Float } { rw | seed : Random.Seed } Sheep
updateFlocking_ config fences doggo flock sheep =
    let
        -- Average velocity of a sheep in the flock.
        flockVelocity : V2
        flockVelocity =
            flock
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
                        flock
            in
            if List.isEmpty veryNearbyFlock then
                flock
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

        fenceForce : V2
        fenceForce =
            V2.sum (List.map (Fence.forceOn sheep) fences)

        newVelocityEff : Eff any { rw | seed : Random.Seed } V2
        newVelocityEff =
            Eff.map
                (\noise ->
                    flockVelocity
                        |> V2.add flockForce
                        |> V2.add doggoForce
                        |> V2.add fenceForce
                        |> V2.add noise
                        |> V2.clampNorm config.minSheepVelocity config.maxSheepVelocity
                        |> limitTurnRate config sheep
                )
                (Eff.random
                    (Random.map2
                        V2
                        (Random.float -0.25 0.25)
                        (Random.float -0.25 0.25)
                    )
                )

        newPosEff : Eff { ro | frames : Float } any P2
        newPosEff =
            Eff.readOnly
                (\env -> P2.add sheep.pos (V2.scale env.frames sheep.vel))

        newFoodEff : Eff { ro | frames : Float } any Float
        newFoodEff =
            Eff.readOnly
                (\env -> sheep.food - gFoodLossRate * env.frames)
    in
    Eff.map3
        (\vel pos food -> { sheep | vel = vel, pos = pos, food = food })
        newVelocityEff
        newPosEff
        newFoodEff


limitTurnRate : Config r1 -> { r2 | vel : V2 } -> V2 -> V2
limitTurnRate config sheep newVelocity =
    let
        theta =
            V2.angleBetween sheep.vel newVelocity

        adjust =
            -- Unhappy path: the sheep's new velocity is too great an angle away
            -- from its previous velocity. So un-rotate the new velocity, then
            -- re-rotate it in the same direction, but only 'sheepTurnRate'
            -- radians.
            if abs theta > config.sheepTurnRate then
                V2.rotate
                    (-theta + Radians.signum theta * config.sheepTurnRate)

            else
                identity
    in
    adjust newVelocity


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


updateGrazing :
    Float
    -> List Fence
    -> { r | pos : P2 }
    -> List Sheep
    -> Sheep
    -> Sheep
updateGrazing frames fences doggo flock sheep =
    if sheep.food > 1 then
        { sheep | state = Flocking }

    else
        { sheep | food = sheep.food + frames * gFoodGainRate }


updateSleeping :
    Float
    -> List Fence
    -> { r | pos : P2 }
    -> List Sheep
    -> Sheep
    -> Sheep
updateSleeping frames fences doggo flock sheep =
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
