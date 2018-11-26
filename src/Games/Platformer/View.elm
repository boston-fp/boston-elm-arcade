module Games.Platformer.View exposing (view)

import Collage exposing (Collage, filled, group, ngon, outlined, rectangle, shift, shiftY, solid, uniform)
import Collage.Render exposing (svg)
import Color exposing (Color, rgb)
import Games.Platformer.Model exposing (Model)
import Games.Platformer.Player exposing (Player)
import Games.Platformer.Vector2D as Vector2D
import Html exposing (Html)
import Html.Attributes as Hattr


white : Color
white =
    rgb 220 220 220


background : Color
background =
    rgb 40 40 40


view : Model -> Html msg
view model =
    Html.div
        [ Hattr.style "background-color" "rgb(20,20,20)"
        , Hattr.style "width" "100%"
        , Hattr.style "height" "100vh"
        , Hattr.style "display" "flex"
        , Hattr.style "flex-direction" "column"
        , Hattr.style "align-items" "center"
        , Hattr.style "justify-content" "center"
        ]
        [ svg <| group <| [ playerView model.player, window ]
        ]


window : Collage msg
window =
    rectangle 640 640
        |> filled (uniform background)


playerView : Player -> Collage msg
playerView player =
    rectangle
        player.rigidbody.size.x
        player.rigidbody.size.y
        |> filled (uniform white)
        |> shift (Vector2D.asTuple player.rigidbody.position)
