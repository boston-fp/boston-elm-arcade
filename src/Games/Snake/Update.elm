module Games.Snake.Update exposing (Msg(..), moveSnek, subs, update)

import Browser.Events
import Games.Snake.Board as Board
import Games.Snake.Model as Model
    exposing
        ( Direction(..)
        , Model
        , Point
        , Segment
        , Snek
        , changeDurr
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
                        moveSnek model.snek

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
            let
                headDurr =
                    model.snek.head.durrection

                changeDurr_ =
                    changeDurr model
            in
            case key of
                Key.Space ->
                    if model.fail then
                        Model.init

                    else
                        { model | paused = not model.paused }

                Key.Up ->
                    if headDurr /= Down then
                        changeDurr_ Up

                    else
                        model

                Key.Down ->
                    if headDurr /= Up then
                        changeDurr_ Down

                    else
                        model

                Key.Left ->
                    if headDurr /= Right then
                        changeDurr_ Left

                    else
                        model

                Key.Right ->
                    if headDurr /= Left then
                        changeDurr_ Right

                    else
                        model

                _ ->
                    model


moveSnek : Snek -> Snek
moveSnek { head, rest } =
    let
        moveSegment : Segment -> Segment
        moveSegment segment =
            let
                ( x, y ) =
                    segment.location
            in
            case segment.durrection of
                Left ->
                    { segment | location = ( x - 1, y ) }

                Right ->
                    { segment | location = ( x + 1, y ) }

                Up ->
                    { segment | location = ( x, y + 1 ) }

                Down ->
                    { segment | location = ( x, y - 1 ) }

        setDurr : Direction -> Segment -> Segment
        setDurr dir seg =
            { seg | durrection = dir }
    in
    { head = moveSegment head
    , rest =
        List.map2
            (\parent seg -> moveSegment seg |> setDurr parent.durrection)
            (head :: rest)
            rest
    }


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ if model.fail then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta NewFrame
        , Browser.Events.onKeyDown (Decode.map KeyPressed Key.decoder)
        ]
