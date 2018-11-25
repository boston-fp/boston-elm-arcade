module Games.Chansey.View exposing (..)

import Collage
import Collage.Render
import Color
import Games.Chansey exposing (..)
import Games.Chansey.Basket as Basket
import Games.Chansey.Column as Column exposing (Column(..))
import Games.Chansey.Egg as Egg exposing (Egg)
import Games.Chansey.EggType exposing (EggType(..))
import Games.Chansey.Types exposing (..)
import Html exposing (Html)


view : Model -> Html Msg
view model =
    Html.div
        []
        [ (Collage.Render.svg << Collage.group)
            (Basket.view model.basket :: List.map Egg.view model.eggs ++ [ viewBackground ])
        , Html.text (String.fromInt model.score)
        ]


viewBackground : Collage.Collage msg
viewBackground =
    Collage.rectangle 400 800
        |> Collage.filled (Collage.uniform Color.black)
