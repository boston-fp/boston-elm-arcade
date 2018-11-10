module Games.Snake.View exposing (view)

import Games.Snake.Model exposing (Model)
import Games.Snake.Update exposing (Msg(..))
import Html exposing (Html)
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (cx, cy, height, r, rx, ry, viewBox, width, x, y)


view : Model -> Html Msg
view model =
    svg
        [ width "120"
        , height "120"
        , viewBox "0 0 120 120"
        ]
        [ rect
            [ x "10"
            , y "10"
            , width "100"
            , height "100"
            , rx "15"
            , ry "15"
            ]
            []
        , circle
            [ cx "50"
            , cy "50"
            , r "50"
            ]
            []
        ]
