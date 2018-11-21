module Games.Snake.Update exposing (Msg(..), subs, update)

import Browser.Events
import Games.Snake.Board as Board
import Games.Snake.Model as Model exposing (Model)
import Games.Snake.Snek as Snek exposing (Direction(..), oppositeDurr)
import Json.Decode as Decode
import Key
import Random
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

                    snekWillEet =
                        Snek.canHazBabby model.babbyPosition nextSnek

                    ( babbyPosition, seed ) =
                        if snekWillEet then
                            Random.step foodPointGen model.seed

                        else
                            ( model.babbyPosition, model.seed )
                in
                { model
                    | snek =
                        if ded then
                            model.snek

                        else if snekWillEet then
                            Snek.enhance nextSnek

                        else
                            nextSnek
                    , timeSinceLastDraw = 0
                    , fail = ded
                    , babbyPosition = babbyPosition
                    , seed = seed
                }

            else
                { model | timeSinceLastDraw = totalDelta }

        KeyPressed key ->
            let
                headDurr =
                    model.snek.head.durrection

                maybeChangeDurr : Direction -> Model
                maybeChangeDurr durr =
                    let
                        snek =
                            model.snek
                    in
                    if durr /= oppositeDurr snek.head.durrection then
                        { model | snek = Snek.changeDurr model.snek durr }

                    else
                        model
            in
            case key of
                Key.Space ->
                    if model.fail then
                        Model.init

                    else
                        { model | paused = not model.paused }

                Key.Up ->
                    maybeChangeDurr Up

                Key.Down ->
                    maybeChangeDurr Down

                Key.Left ->
                    maybeChangeDurr Left

                Key.Right ->
                    maybeChangeDurr Right

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


foodPointGen : Random.Generator Board.Point
foodPointGen =
    Random.pair
        (Random.int ((-Board.width // 2) + 1) ((Board.width // 2) - 1))
        (Random.int ((-Board.height // 2) + 1) ((Board.height // 2) - 1))
