module Games.Snake.View exposing (view)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text exposing (..)
import Color
import Games.Snake.Board as Board
import Games.Snake.Model as Model exposing (Model, Point, Snek, snek2List)
import Html exposing (Html)


snakeSegmentSize : Float
snakeSegmentSize =
    10


gamePointToViewPoint : Point -> ( Float, Float )
gamePointToViewPoint ( x, y ) =
    ( gameCoordToViewCoord x, gameCoordToViewCoord y )


gameCoordToViewCoord : Int -> Float
gameCoordToViewCoord val =
    toFloat val * snakeSegmentSize


view : Model -> Html msg
view model =
    let
        boardRect : Collage msg
        boardRect =
            rectangle (gameCoordToViewCoord Board.width) (gameCoordToViewCoord Board.height)
                |> outlined (solid 2 (uniform Color.black))

        snek : Collage msg
        snek =
            model.snek |> snek2List |> List.map drawSnakeSegment |> group

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

        dedText : Collage msg
        dedText =
            fromString "DED"
                |> size (huge * 4)
                |> color Color.black
                |> rendered

        maybeFullscreenText : List (Collage msg)
        maybeFullscreenText =
            if Model.isDed (Debug.log "snekBody" model.snek) then
                [ dedText ]

            else if model.paused then
                [ pausedTxt ]

            else
                []
    in
    svg <| group <| (maybeFullscreenText ++ [ boardRect, snek, food ])


drawSnakeSegment : Point -> Collage msg
drawSnakeSegment point =
    square snakeSegmentSize
        |> styled ( uniform Color.green, solid 2 <| uniform Color.black )
        |> shift (gamePointToViewPoint point)
