module Games.Chansey.EggTimer exposing
    ( EggTimer
    , EggTimerStep(..)
    , done
    , new
    , step
    )

import Games.Chansey.Egg as Egg exposing (Egg)
import Games.Chansey.EggType exposing (EggType(..))
import Games.Chansey.Types exposing (..)
import Random
import RecurringTimer exposing (RecurringTimer)


type EggTimer
    = EggTimer
        { speed : { min : Y_Per_Millisecond, max : Y_Per_Millisecond }
        , seed : Random.Seed
        , timer : RecurringTimer
        , supply : Int -- *Egg* supply (bombs don't count)
        }


type EggTimerStep
    = EggTimerStep EggTimer
    | EggTimerFire EggTimer Egg
    | EggTimerDone


new :
    { min : Y_Per_Millisecond, max : Y_Per_Millisecond }
    -> Random.Seed
    -> RecurringTimer
    -> Int
    -> EggTimer
new speed seed timer supply =
    EggTimer
        { speed = speed
        , seed = seed
        , timer = timer
        , supply = supply
        }


done : EggTimer -> Bool
done (EggTimer timer) =
    timer.supply <= 0


step : Milliseconds -> EggTimer -> EggTimerStep
step delta (EggTimer timer) =
    if done (EggTimer timer) then
        EggTimerDone

    else
        case RecurringTimer.step delta timer.timer of
            RecurringTimer.Step timer1 ->
                EggTimerStep (EggTimer { timer | timer = timer1 })

            RecurringTimer.Fire timer1 ->
                let
                    ( egg, seed1 ) =
                        Egg.random timer.seed timer.speed
                in
                EggTimerFire
                    (EggTimer
                        { timer
                            | seed = seed1
                            , timer = timer1
                            , supply =
                                case Egg.typ egg of
                                    EggTypeBomb ->
                                        timer.supply

                                    EggTypeEgg ->
                                        timer.supply - 1
                        }
                    )
                    egg
