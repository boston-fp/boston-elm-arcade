module Games.Platformer.Update exposing (Msg(..), subs, update)

import Browser.Events exposing (onAnimationFrame)
import Games.Platformer.Controller as Controller exposing (Controller)
import Games.Platformer.Model exposing (Model)
import Games.Platformer.Player as Player
import Json.Decode as Decode
import Key exposing (Key, KeyType(..))
import Time exposing (Posix)


type Msg
    = Tick Posix
    | KeyDown Key
    | KeyUp Key


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick time ->
            { model | player = Player.update model.controller model.player }

        KeyDown key ->
            { model | controller = Controller.update key True model.controller }

        KeyUp key ->
            { model | controller = Controller.update key False model.controller }


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ onAnimationFrame (\time -> Tick time)
        , Browser.Events.onKeyDown (Decode.map KeyDown Key.decoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp Key.decoder)
        ]
