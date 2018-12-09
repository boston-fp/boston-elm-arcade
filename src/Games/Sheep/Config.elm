module Games.Sheep.Config exposing (..)


type alias Config =
    { maxSheepVelocity : Float
    , minSheepVelocity : Float
    , sheepTurnRate : Float

    , crawlingDoggoTurnRate : Float
    , walkingDoggoTurnRate : Float
    , runningDoggoTurnRate : Float
    }
