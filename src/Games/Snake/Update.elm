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
    30


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewFrame delta ->
            let
                totalDelta =
                    model.timeSinceLastDraw + delta
            in
            if model.paused || model.fail then
                { model | timeSinceLastDraw = totalDelta }

            else if totalDelta > deltaThreshold then
                let
                    nextSnek =
                        moveSnek Up model.snek

                    ded =
                        Model.isDed nextSnek
                in
                { model
                    | snek =
                        if ded then
                            model.snek

                        else
                            nextSnek
                    , timeSinceLastDraw = 0
                    , fail = ded
                }

            else
                { model | timeSinceLastDraw = totalDelta }

        KeyPressed key ->
            case key of
                Key.Space ->
                    if model.fail then
                        Model.init

                    else
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
        [ if model.fail then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta NewFrame
        , Browser.Events.onKeyDown (Decode.map KeyPressed Key.decoder)
        ]
