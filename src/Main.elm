module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element as E exposing (el, px, text)
import Element.Background as Bg
import Element.Input as Input exposing (button)
import Games.Snake.Model
import Html exposing (Html)


type Model
    = NoGame
    | Snake Games.Snake.Model.Model


{-| TODO:
-}
type alias Game =
    String


games : List Game
games =
    [ "Snake"
    , "18th Century MMORPG in Colonial Times (Working Title)"
    , "Pong"
    , "Tower Defense"
    ]


init : ( Model, Cmd Msg )
init =
    ( NoGame, Cmd.none )


type Msg
    = NoOp
    | StartPlaying Game


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    E.layout [ E.centerX, E.centerY ] <|
        E.wrappedRow
            [ E.centerX, E.centerY, E.padding 10, E.spacing 10 ]
            (games
                |> List.map
                    (\game ->
                        el []
                            (Input.button
                                [ Bg.color (E.rgb255 85 131 200)
                                , E.padding 10
                                ]
                                { onPress = Just (StartPlaying game)
                                , label = text game
                                }
                            )
                    )
            )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
