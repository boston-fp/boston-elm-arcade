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
    120


withNoCmd : a -> ( a, Cmd msg )
withNoCmd a =
    ( a, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewFrame delta ->
            let
                totalDelta =
                    model.timeSinceLastDraw + delta
            in
            if model.paused || model.fail then
                { model | timeSinceLastDraw = totalDelta } |> withNoCmd

            else if totalDelta > deltaThreshold then
                let
                    nextSnek =
                        if
                            model.queuedDurr
                                /= Snek.oppositeDurr
                                    (Snek.durrection model.snek)
                        then
                            Snek.changeDurr model.snek model.queuedDurr |> Snek.move

                        else
                            Snek.move model.snek

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
                        if Snek.isDed nextSnek then
                            model.snek

                        else if snekWillEet then
                            Snek.enhance nextSnek

                        else
                            nextSnek
                    , timeSinceLastDraw = 0
                    , fail = Snek.isDed nextSnek
                    , babbyPosition = babbyPosition
                    , seed = seed
                    , score =
                        if snekWillEet then
                            model.score + 10

                        else
                            model.score
                }
                    |> withNoCmd

            else
                { model | timeSinceLastDraw = totalDelta } |> withNoCmd

        KeyPressed key ->
            (case key of
                Key.Space ->
                    if model.fail then
                        Model.init

                    else
                        { model | paused = not model.paused }

                Key.Up ->
                    { model | queuedDurr = Up }

                Key.Down ->
                    { model | queuedDurr = Down }

                Key.Left ->
                    { model | queuedDurr = Left }

                Key.Right ->
                    { model | queuedDurr = Right }

                _ ->
                    model
            )
                |> withNoCmd


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
