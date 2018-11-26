module Games.Platformer.Model exposing (Model, init)

import Games.Platformer.Controller as Controller exposing (Controller)
import Games.Platformer.Player as Player exposing (Player)


type alias Model =
    { player : Player
    , controller : Controller
    }


init : Model
init =
    { player = Player.init
    , controller = Controller.init
    }
