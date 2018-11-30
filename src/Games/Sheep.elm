module Games.Sheep exposing (..)

import Html exposing (Html)


type Model
    = Model


type Msg
    = Msg


init : Model
init =
    Model


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Html Msg
view _ =
    Html.text ""


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
