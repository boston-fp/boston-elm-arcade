module Games.Sheep exposing (..)

import Browser.Events
import Html exposing (Html)
import Json.Decode


type alias Model =
    { lastmsg : Maybe Msg }


type Msg
    = Tick Float
    | Keydown String
    | Keyup String


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
    let
        keyDecoder : Json.Decode.Decoder String
        keyDecoder =
            Json.Decode.field "key" Json.Decode.string
    in
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (Json.Decode.map Keydown keyDecoder)
        , Browser.Events.onKeyUp (Json.Decode.map Keyup keyDecoder)
        ]
