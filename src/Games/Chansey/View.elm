module Games.Chansey.View exposing (..)

import Collage
import Collage.Render
import Collage.Text
import Color
import Games.Chansey exposing (..)
import Games.Chansey.Basket as Basket
import Games.Chansey.Column as Column exposing (Column(..))
import Games.Chansey.Egg as Egg exposing (Egg)
import Games.Chansey.EggType exposing (EggType(..))
import Games.Chansey.Level as Level
import Games.Chansey.State exposing (..)
import Games.Chansey.Types exposing (..)
import Html exposing (Html)


view : Model -> Html Msg
view model =
    Html.div
        []
        [ (Collage.Render.svg << Collage.group)
            [ case model.state of
                ShowingLevelTitle ->
                    ("Level " ++ String.fromInt model.level.config.levelnum)
                        |> Collage.Text.fromString
                        |> Collage.Text.color Color.white
                        |> Collage.Text.size Collage.Text.huge
                        |> Collage.rendered

                PlayingLevel ->
                    Level.view model.level

                ShowingLevelScore ->
                    (String.fromInt model.level.score ++ "/" ++ String.fromInt model.level.config.numeggs)
                        |> Collage.Text.fromString
                        |> Collage.Text.color Color.white
                        |> Collage.Text.size Collage.Text.huge
                        |> Collage.rendered

                GameWinScreen ->
                    "Winner!"
                        |> Collage.Text.fromString
                        |> Collage.Text.color Color.white
                        |> Collage.Text.size Collage.Text.huge
                        |> Collage.rendered
            , viewBackground
            ]

        -- , Html.text (Debug.toString model)
        ]


viewBackground : Collage.Collage msg
viewBackground =
    Collage.rectangle 400 800
        |> Collage.filled (Collage.uniform Color.black)
