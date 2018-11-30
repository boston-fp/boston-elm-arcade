module Games.Chansey.Basket exposing
    ( Basket
    , Control(..)
    , column
    , new
    , step
    , view
    , y
    )

import Collage
import Color
import Games.Chansey.Column as Column exposing (Column)
import Games.Chansey.Types exposing (..)


type Basket
    = Basket { column_ : Column, paddle : Paddle }


new : Column -> Basket
new col =
    Basket { column_ = col, paddle = PaddleNone }


column : Basket -> Column
column (Basket { column_ }) =
    column_


y : Y
y =
    -300


type Paddle
    = PaddleNone
    | PaddleL
    | PaddleR
    | PaddleLR


type Control
    = LeftDown
    | LeftUp
    | RightDown
    | RightUp


step : Control -> Basket -> Basket
step control (Basket basket) =
    case control of
        LeftDown ->
            case basket.paddle of
                PaddleNone ->
                    Basket { column_ = Column.Left, paddle = PaddleL }

                PaddleL ->
                    Basket basket

                PaddleR ->
                    Basket { basket | paddle = PaddleLR }

                PaddleLR ->
                    Basket basket

        LeftUp ->
            case basket.paddle of
                PaddleNone ->
                    Basket basket

                PaddleL ->
                    Basket { column_ = Column.Center, paddle = PaddleNone }

                PaddleR ->
                    Basket basket

                PaddleLR ->
                    Basket { column_ = Column.Right, paddle = PaddleR }

        RightDown ->
            case basket.paddle of
                PaddleNone ->
                    Basket { column_ = Column.Right, paddle = PaddleR }

                PaddleL ->
                    Basket { basket | paddle = PaddleLR }

                PaddleR ->
                    Basket basket

                PaddleLR ->
                    Basket basket

        RightUp ->
            case basket.paddle of
                PaddleNone ->
                    Basket basket

                PaddleL ->
                    Basket basket

                PaddleR ->
                    Basket { column_ = Column.Center, paddle = PaddleNone }

                PaddleLR ->
                    Basket { column_ = Column.Left, paddle = PaddleL }



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Basket -> Collage.Collage msg
view (Basket basket) =
    Collage.circle 15
        |> Collage.filled (Collage.uniform Color.blue)
        |> Collage.shift ( Column.x basket.column_, y )
