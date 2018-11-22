module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Element as E exposing (el, px, text)
import Element.Background as Bg
import Element.Input as Input exposing (button)
import Games.Snake.Model as SnakeModel
import Games.Snake.Update as SnakeUpdate
import Games.Snake.View as SnakeView
import Html exposing (Html)
import Key
import Ports
import Time


type Model
    = NoGame
    | PlayingSnake SnakeModel.Model


gameName : Game -> String
gameName game =
    case game of
        Snake ->
            "Snake"


gameInit : Game -> Model
gameInit game =
    case game of
        Snake ->
            PlayingSnake SnakeModel.init


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
    | SnakeMsg SnakeUpdate.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartPlaying game ->
            ( gameInit game, Cmd.none )

        SnakeMsg snakeMsg ->
            let
                snakeModel : SnakeModel.Model
                snakeModel =
                    case model of
                        NoGame ->
                            SnakeModel.init

                        PlayingSnake m ->
                            m

                newModel : SnakeModel.Model
                newModel =
                    SnakeUpdate.update snakeMsg snakeModel
            in
            ( PlayingSnake newModel, Ports.setTitle <| gameName Snake )


noGame : Model -> Html Msg
noGame model =
    E.layout
        [ E.centerX
        , E.centerY
        , Bg.color (E.rgb255 20 20 20)
        , E.width E.fill
        , E.height E.fill
        ]
    <|
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
            Html.map SnakeMsg (SnakeView.view snakeModel)



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NoGame ->
            Sub.none

        PlayingSnake snakeModel ->
            SnakeUpdate.subs snakeModel |> Sub.map SnakeMsg


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
