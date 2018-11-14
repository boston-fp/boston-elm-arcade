module Games.Snake.Update exposing (Msg(..), subs, update)

import Browser.Events
import Games.Snake.Board as Board
import Games.Snake.Model as Model
    exposing
        ( Model
        , Point
        , Snek
        , snek2List
        , snekMap
        )
import Json.Decode as Decode
import Key
import Time


type Msg
    = NewFrame Float
    | KeyPressed Key.Key


deltaThreshold : Float
deltaThreshold =
    100


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewFrame delta ->
            if model.paused || Model.isDed model.snek then
                model

            else
                { model | snek = moveSnek Up model.snek }

        KeyPressed key ->
            case key of
                Key.Space ->
                    { model | paused = not model.paused }

                _ ->
                    model


type Direction
    = Left
    | Right
    | Up
    | Down


moveSnek : Direction -> Snek -> Snek
moveSnek direction =
    let
        move : Point -> Point
        move ( x, y ) =
            case direction of
                Left ->
                    ( x - 1, y )

                Right ->
                    ( x + 1, y )

                Up ->
                    ( x, y + 1 )

                Down ->
                    ( x, y - 1 )
    in
    snekMap move


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta NewFrame
        , Browser.Events.onKeyDown (Decode.map KeyPressed Key.decoder)
        ]
