module Games.Sheep exposing (..)

import Html exposing (Html)


type alias Model =
    { lastmsg : Maybe Msg }


type Msg
    = Msg


init : Model
init =
    { lastmsg = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( { model | lastmsg = Just msg }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
      []
      [ Html.text (Debug.toString model) ]

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
