module Games.Chansey.View exposing (..)

import Collage
import Collage.Render
import Color
import Games.Chansey exposing (..)
import Html exposing (Html)


view : Model -> Html Msg
view model =
    Html.div
        []
        [ (Collage.Render.svg << Collage.group)
            (viewBasket model.basket :: List.map viewEgg model.eggs ++ [ viewBackground ])
        , Html.text (String.fromInt model.score)
        ]


viewBasket : Column -> Collage.Collage msg
viewBasket basket =
    Collage.circle 15
        |> Collage.filled (Collage.uniform Color.blue)
        |> Collage.shift ( colX basket, -300 )


viewEgg : Egg -> Collage.Collage msg
viewEgg egg =
    Collage.circle 10
        |> Collage.filled
            (Collage.uniform
                (case egg.typ of
                    EggTypeEgg ->
                        Color.yellow

                    EggTypeBomb ->
                        Color.red
                )
            )
        |> Collage.shift ( colX egg.column, egg.y )


viewBackground : Collage.Collage msg
viewBackground =
    Collage.rectangle 400 800
        |> Collage.filled (Collage.uniform Color.black)
