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


grid : Collage msg
grid =
    let
        lines : Int -> Int -> Collage msg
        lines size length =
            List.range (-size // 2) (size // 2)
                |> List.map
                    (\index ->
                        let
                            t =
                                toFloat index * snakeSegmentSize
                        in
                        shiftY t <|
                            traced (solid thin (uniform <| Color.rgb 40 40 40)) <|
                                Collage.line <|
                                    toFloat length
                                        * snakeSegmentSize
                    )
                |> group
    in
    group
        [ lines Board.height Board.width
        , lines Board.width Board.height |> rotate (Basics.pi / 2)
        ]


view : Model -> Html msg
view model =
    let
        boardRect : Collage msg
        boardRect =
            rectangle (gameCoordToViewCoord Board.width) (gameCoordToViewCoord Board.height)
                |> outlined (solid 2 (uniform Color.green))

        snek : Collage msg
        snek =
            group
                (drawSnakeSegment Color.blue model.snek.head.location
                    :: List.map (drawSnakeSegment Color.green << .location)
                        model.snek.rest
                )

        food : Collage msg
        food =
            square snakeSegmentSize
                |> filled (uniform Color.yellow)
                |> shift ( 100, -200 )
                |> shift ( snakeSegmentSize / 2, snakeSegmentSize / 2 )

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
            -- if Model.isDed (Debug.log "snekBody" model.snek) then
            if model.fail then
                [ dedText ]

            else if model.paused then
                [ pausedTxt ]

            else
                []
    in
    Html.div
        [ Hattr.style "background-color" "rgb(20,20,20)"
        , Hattr.style "width" "100%"
        , Hattr.style "height" "100vh"
        , Hattr.style "display" "flex"
        , Hattr.style "flex-direction" "column"
        , Hattr.style "align-items" "center"
        , Hattr.style "justify-content" "center"
        ]
        [ svg <|
            group <|
                (maybeFullscreenText ++ [ food, snek, boardRect, grid ])
        ]


drawSnakeSegment : Color.Color -> Point -> Collage msg
drawSnakeSegment color point =
    square snakeSegmentSize
        |> styled ( uniform color, solid 2 <| uniform Color.black )
        |> shift (gamePointToViewPoint point)
        |> shift ( snakeSegmentSize / 2, snakeSegmentSize / 2 )
