module RecurringTimer
    exposing
        ( RecurringTimer
        , Step(..)
        , new
        , newWithJitter
        , step
        )

import Random


type RecurringTimer
    = RecurringTimer Random.Seed (Random.Generator Milliseconds) Milliseconds


type alias Milliseconds =
    Float


new :
    Milliseconds
    -> RecurringTimer
new time =
    newWithJitter (Random.initialSeed 0) (Random.constant 0)


newWithJitter :
    Random.Seed
    -> Random.Generator Milliseconds
    -> RecurringTimer
newWithJitter seed0 gen =
    let
        ( time, seed1 ) =
            Random.step gen seed0
    in
    RecurringTimer seed1 gen time


type Step
    = Step RecurringTimer
    | Fire RecurringTimer


step : Milliseconds -> RecurringTimer -> Step
step delta (RecurringTimer seed0 gen time) =
    let
        time1 =
            time - delta
    in
    if time1 <= 0 then
        let
            ( time2, seed1 ) =
                Random.step gen seed0
        in
        Fire (RecurringTimer seed1 gen (time2 + time1))
    else
        Step (RecurringTimer seed0 gen time1)
