module Games.Sheep.Controller exposing (..)

import Key


{-| The sheep game controller.
-}
type alias Controller =
    { down : Bool
    , left : Bool
    , right : Bool
    , up : Bool
    , space : Bool
    }


{-| The buttons on this controller. Not the same type as Key, because we might
not support all key presses.
-}
type Button
    = Down
    | Left
    | Right
    | Up
    | Space


{-| The result of applying a key event to a controller. Either results in an
impulse (key down / key up), or a no-op (i.e. a button is being held down).
-}
type Update
    = Rise Button Controller
    | Fall Button Controller
    | Noop Controller


new : Controller
new =
    Controller False False False False False


update : Key.Event -> Controller -> Update
update event controller =
    case event of
        Key.KeyDown Key.Down ->
            if controller.down then
                Noop controller

            else
                Rise Down { controller | down = True }

        Key.KeyDown Key.Left ->
            if controller.left then
                Noop controller

            else
                Rise Left { controller | left = True }

        Key.KeyDown Key.Right ->
            if controller.right then
                Noop controller

            else
                Rise Right { controller | right = True }

        Key.KeyDown Key.Up ->
            if controller.up then
                Noop controller

            else
                Rise Up { controller | up = True }

        Key.KeyDown Key.Space ->
            if controller.space then
                Noop controller

            else
                Rise Space { controller | space = True }

        Key.KeyUp Key.Down ->
            if controller.down then
                Fall Down { controller | down = False }

            else
                Noop controller

        Key.KeyUp Key.Left ->
            if controller.left then
                Fall Left { controller | left = False }

            else
                Noop controller

        Key.KeyUp Key.Right ->
            if controller.right then
                Fall Right { controller | right = False }

            else
                Noop controller

        Key.KeyUp Key.Up ->
            if controller.up then
                Fall Up { controller | up = False }

            else
                Noop controller

        Key.KeyUp Key.Space ->
            if controller.space then
                Fall Space { controller | space = False }

            else
                Noop controller

        _ ->
            Noop controller
