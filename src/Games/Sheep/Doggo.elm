module Games.Sheep.Doggo exposing (..)

import Games.Sheep.Controller exposing (Controller)
import Games.Sheep.Eff as Eff exposing (Eff)
import P2 exposing (P2)
import Radians exposing (Radians)
import V2 exposing (V2)


type alias Doggo =
    { pos : P2
    , angle : Radians
    }


type alias Config r =
    { r
        | crawlingDoggoTurnRate : Float
        , crawlingDoggoVelocity : Float
        , maxSheepVelocity : Float
        , runningDoggoTurnRate : Float
        , runningDoggoVelocity : Float
        , walkingDoggoTurnRate : Float
        , walkingDoggoVelocity : Float
    }


type Bearing
    = Forward
    | Halt
    | Back


update : Config r -> Controller -> Doggo -> Eff { ro | frames : Float } rw Doggo
update config controller doggo =
    let
        angleDt : Eff { ro | frames : Float } rw Radians
        angleDt =
            Eff.readOnly
                (\env ->
                    turntRate config controller
                        * env.frames
                        * (case ( controller.left, controller.right ) of
                            ( True, False ) ->
                                1

                            ( False, True ) ->
                                -1

                            _ ->
                                0
                          )
                )

        newPos : Eff { ro | frames : Float } rw P2
        newPos =
            Eff.readOnly
                (\env ->
                    integratePos env.frames
                        { pos = doggo.pos
                        , vel = doggoVel config controller doggo
                        }
                )
    in
    Eff.map2
        (\ang paws ->
            { doggo
                | pos = paws
                , angle = doggo.angle + ang
            }
        )
        angleDt
        newPos


turntRate : Config r -> Controller -> Float
turntRate config controller =
    case bearingDoggo controller of
        Forward ->
            config.runningDoggoTurnRate

        Halt ->
            config.walkingDoggoTurnRate

        Back ->
            config.crawlingDoggoTurnRate


bearingDoggo : Controller -> Bearing
bearingDoggo d =
    case ( d.up, d.down ) of
        ( True, True ) ->
            Halt

        ( True, False ) ->
            Forward

        ( False, True ) ->
            Back

        ( False, False ) ->
            Halt


doggoVel : Config r -> Controller -> Doggo -> V2
doggoVel config controller doggo =
    let
        vec =
            V2.fromRadians doggo.angle

        magnitude =
            case bearingDoggo controller of
                Forward ->
                    config.runningDoggoVelocity

                Halt ->
                    config.walkingDoggoVelocity

                Back ->
                    config.crawlingDoggoVelocity
    in
    V2.scale magnitude vec


integratePos : Float -> { r | pos : P2, vel : V2 } -> P2
integratePos frames entity =
    P2.add entity.pos (V2.scale frames entity.vel)
