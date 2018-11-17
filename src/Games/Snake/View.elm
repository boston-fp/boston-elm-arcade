module Games.Snake.View exposing (view)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text exposing (..)
import Color
import Games.Snake.Board as Board
import Games.Snake.Model as Model exposing (Model, Point, Snek, snek2List)
import Html exposing (Html)
import Html.Attributes as Hattr


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
                |> outlined (solid 2 (uniform Color.green))

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
                |> color Color.yellow
                |> rendered

        dedText : Collage msg
        dedText =
            fromString "DED"
                |> size (huge * 4)
                |> color Color.red
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
    Html.div
        [ Hattr.style "background-color" "black"
        , Hattr.style "width" "100%"
        , Hattr.style "height" "100vh"
        , Hattr.style "display" "flex"
        , Hattr.style "flex-direction" "column"
        , Hattr.style "align-items" "center"
        ]
        [ svg <| group <| (maybeFullscreenText ++ [ boardRect, snek, food ]) ]


drawSnakeSegment : Point -> Collage msg
drawSnakeSegment point =
    square snakeSegmentSize
        |> styled ( uniform Color.green, solid 2 <| uniform Color.black )
        |> shift (gamePointToViewPoint point)
