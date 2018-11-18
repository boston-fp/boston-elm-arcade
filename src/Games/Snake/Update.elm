module Games.Snake.Update exposing (Msg(..), subs, update)

import Browser.Events
import Games.Snake.Board as Board
import Games.Snake.Model as Model exposing (Model)
import Games.Snake.Snek as Snek exposing (Direction(..))
import Json.Decode as Decode
import Key
import Time


type Msg
    = NewFrame Float
    | KeyPressed Key.Key


deltaThreshold : Float
deltaThreshold =
    180


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
                        Snek.move model.snek

                    ded =
                        Snek.isDed nextSnek
                in
                { model
                    | snek =
                        if ded then
                            model.snek

                        else if Snek.canHazBabby model.babbyPosition nextSnek then
                            Snek.enhance nextSnek

                        else
                            nextSnek
                    , timeSinceLastDraw = 0
                    , fail = ded
                }

            else
                { model | timeSinceLastDraw = totalDelta }

        KeyPressed key ->
            let
                headDurr =
                    model.snek.head.durrection

                changeDurr durr =
                    let
                        snek =
                            model.snek
                    in
                    { model | snek = Snek.changeDurr model.snek durr }
            in
            case key of
                Key.Space ->
                    if model.fail then
                        Model.init

                    else
                        { model | paused = not model.paused }

                Key.Up ->
                    if headDurr /= Down then
                        changeDurr Up

                    else
                        model

                Key.Down ->
                    if headDurr /= Up then
                        changeDurr Down

                    else
                        model

                Key.Left ->
                    if headDurr /= Right then
                        changeDurr Left

                    else
                        model

                Key.Right ->
                    if headDurr /= Left then
                        changeDurr Right

                    else
                        model

                _ ->
                    model


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ if model.fail then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta NewFrame
        , Browser.Events.onKeyDown (Decode.map KeyPressed Key.decoder)
        ]
