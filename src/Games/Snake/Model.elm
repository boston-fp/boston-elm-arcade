module Games.Snake.Model exposing (Model, init)

import Games.Snake.Board as Board
import Games.Snake.Snek as Snek


type alias Model =
    { snek : Snek.Snek
    , babbyPosition : Board.Point
    , paused : Bool
    , timeSinceLastDraw : Float
    , fail : Bool
    }


init : Model
init =
    { snek = Snek.init
    , babbyPosition = ( 10, 0 )
    , paused = False
    , timeSinceLastDraw = 0
    , fail = False
    }
