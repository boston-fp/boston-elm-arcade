module Games.Snake.Update exposing (Msg(..), subs, update)

import Browser.Events
import Games.Snake.Model exposing (Model, Point)
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
            { model | body = moveSnake Up model.body }

        KeyPressed key ->
            model


type Direction
    = Left
    | Right
    | Up
    | Down


moveSnake : Direction -> List Point -> List Point
moveSnake direction =
    List.map
        (\( x, y ) ->
            case direction of
                Left ->
                    ( x - 1, y )

                Right ->
                    ( x + 1, y )

                Up ->
                    ( x, y + 1 )

                Down ->
                    ( x, y - 1 )
        )


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta NewFrame
        , Browser.Events.onKeyDown (Decode.map KeyPressed Key.decoder)
        ]
