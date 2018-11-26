module Games.Platformer.Controller exposing (Controller, init, update)

import Key exposing (Key)


type alias Controller =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    }


init : Controller
init =
    { left = False
    , right = False
    , up = False
    , down = False
    }


update : Key -> Bool -> Controller -> Controller
update key keyState controller =
    case key of
        Key.Left ->
            { controller | left = keyState }

        Key.Right ->
            { controller | right = keyState }

        Key.Up ->
            { controller | up = keyState }

        Key.Down ->
            { controller | down = keyState }

        _ ->
            controller
