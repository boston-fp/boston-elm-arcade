module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element as E exposing (el, px, text)
import Element.Background as Bg
import Element.Input as Input exposing (button)
import Games.Snake.Model
import Games.Snake.Update
import Games.Snake.View
import Html exposing (Html)


type Model
    = NoGame
    | PlayingSnake Games.Snake.Model.Model


gameName : Game -> String
gameName game =
    case game of
        Snake ->
            "Snake"


gameInit : Game -> Model
gameInit game =
    case game of
        Snake ->
            PlayingSnake Games.Snake.Model.init


type Game
    = Snake


games : List Game
games =
    [ Snake ]


init : ( Model, Cmd Msg )
init =
    ( NoGame, Cmd.none )


type Msg
    = StartPlaying Game
    | SnakeMsg Games.Snake.Update.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartPlaying game ->
            ( gameInit game, Cmd.none )

        SnakeMsg snakeMsg ->
            let
                snakeModel =
                    case model of
                        NoGame ->
                            Debug.todo "crash"

                        PlayingSnake m ->
                            m

                newModel =
                    Games.Snake.Update.update snakeMsg snakeModel
            in
            ( PlayingSnake newModel, Cmd.none )


noGame : Model -> Html Msg
noGame model =
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
                                , label = text (gameName game)
                                }
                            )
                    )
            )


view : Model -> Html Msg
view model =
    case model of
        NoGame ->
            noGame model

        PlayingSnake snakeModel ->
            Html.map SnakeMsg (Games.Snake.View.view snakeModel)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
