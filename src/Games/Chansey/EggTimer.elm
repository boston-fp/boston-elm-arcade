module Games.Chansey.EggTimer
    exposing
        ( EggTimer
        , EggTimerStep(..)
        , new
        , step
        )

import Games.Chansey.Egg as Egg exposing (Egg)
import Games.Chansey.Types exposing (..)
import Random
import RecurringTimer exposing (RecurringTimer)


type EggTimer
    = EggTimer Random.Seed RecurringTimer


type EggTimerStep
    = EggTimerStep EggTimer
    | EggTimerFire EggTimer Egg


new : Random.Seed -> RecurringTimer -> EggTimer
new =
    EggTimer


step : Milliseconds -> EggTimer -> EggTimerStep
step delta (EggTimer seed0 timer0) =
    case RecurringTimer.step delta timer0 of
        RecurringTimer.Step timer1 ->
            EggTimerStep (EggTimer seed0 timer1)

        RecurringTimer.Fire timer1 ->
            let
                ( egg, seed1 ) =
                    Egg.random seed0
            in
            EggTimerFire (EggTimer seed1 timer1) egg
