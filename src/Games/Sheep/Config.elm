module Games.Sheep.Config exposing (..)


type alias Config =
    { maxSheepVelocity : Float
    , minSheepVelocity : Float
    , sheepTurnRate : Float
    , crawlingDoggoVelocity : Float
    , walkingDoggoVelocity : Float
    , runningDoggoVelocity : Float
    , crawlingDoggoTurnRate : Float
    , walkingDoggoTurnRate : Float
    , runningDoggoTurnRate : Float
    }
