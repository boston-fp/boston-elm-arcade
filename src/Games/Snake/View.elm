module Games.Snake.View exposing (view)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color
import Games.Snake.Model exposing (Model)
import Games.Snake.Update exposing (Msg(..))
import Html exposing (Html)


boardHeight : Float
boardHeight =
    700


boardWidth : Float
boardWidth =
    500


snakeSegmentSize : Float
snakeSegmentSize =
    1


view : Model -> Html msg
view model =
    let
        boardRect : Collage msg
        boardRect =
            rectangle boardWidth boardHeight
                |> outlined (solid 2 (uniform Color.black))

        snek : Collage msg
        snek =
            square snakeSegmentSize
                |> filled (uniform Color.green)

        food : Collage msg
        food =
            square snakeSegmentSize
                |> filled (uniform Color.darkCharcoal)
    in
    boardRect
        |> svg
