module Games.Snake.View exposing (view)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text exposing (..)
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
    10


view : Model -> Html msg
view model =
    let
        boardRect : Collage msg
        boardRect =
            rectangle boardWidth boardHeight
                |> outlined (solid 2 (uniform Color.black))

        snek : Collage msg
        snek =
            model.body |> List.map drawSnakeSegment |> group

        food : Collage msg
        food =
            square snakeSegmentSize
                |> filled (uniform Color.darkCharcoal)
                |> shift ( 100, -200 )

        pausedTxt : Collage msg
        pausedTxt =
            fromString "PAUSED"
                |> size huge
                |> color Color.black
                |> rendered

        maybePaused : List (Collage msg)
        maybePaused =
            if model.paused then
                [ pausedTxt ]

            else
                []
    in
    svg <| group <| (maybePaused ++ [ boardRect, snek, food ])


drawSnakeSegment : Point -> Collage msg
drawSnakeSegment ( x, y ) =
    square snakeSegmentSize
        |> styled ( uniform Color.green, solid 2 <| uniform Color.black )
        |> shift ( x * snakeSegmentSize, y * snakeSegmentSize )
