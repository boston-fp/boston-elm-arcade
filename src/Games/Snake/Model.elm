module Games.Snake.Model exposing (Model, init)

import Games.Snake.Board as Board
import Games.Snake.Snek as Snek
import Random


type alias Model =
    { snek : Snek.Snek
    , babbyPosition : Board.Point
    , paused : Bool
    , timeSinceLastDraw : Float
    , fail : Bool
    , seed : Random.Seed
    , score : Int
    , queuedDurr : Snek.Direction
    }


init : Model
init =
    { snek = Snek.init
    , babbyPosition = ( 10, 0 )
    , paused = False
    , timeSinceLastDraw = 0
    , fail = False
    , seed = Random.initialSeed 42
    , score = 0
    , queuedDurr = Snek.init.head.durrection
    }
