module Games.Snake.Update exposing (Msg(..), update)

import Games.Snake.Model exposing (Model)


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model
