module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Element as E exposing (el, px, text)
import Element.Background as Bg
import Element.Input as Input exposing (button)
import Games.Snake.Model as SnakeModel
import Games.Snake.Update as SnakeUpdate
import Games.Snake.View as SnakeView
import Html exposing (Html)
import Key
import Time
import Url
import Url.Parser exposing ((</>), Parser, oneOf, s)


type Game
    = Snake


type GameState
    = NoGame
    | PlayingSnake SnakeModel.Model


gameStateParser : Parser (GameState -> a) a
gameStateParser =
    oneOf
        [ Url.Parser.map NoGame Url.Parser.top
        , Url.Parser.map (PlayingSnake SnakeModel.init) (s (gameName Snake))
        ]


gameName : Game -> String
gameName game =
    case game of
        Snake ->
            "Snake"


gameInit : Game -> GameState
gameInit game =
    case game of
        Snake ->
            PlayingSnake SnakeModel.init


games : List Game
games =
    [ Snake ]


type alias Model =
    { navKey : Nav.Key, gameState : GameState }


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    ( { navKey = key, gameState = urlToGameState url }, Cmd.none )


urlToGameState : Url.Url -> GameState
urlToGameState url =
    Url.Parser.parse gameStateParser url |> Maybe.withDefault NoGame


type Msg
    = SnakeMsg SnakeUpdate.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            ( { model | gameState = urlToGameState url }, Cmd.none )

        LinkClicked req ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        SnakeMsg snakeMsg ->
            let
                snakeModel : SnakeModel.Model
                snakeModel =
                    case model.gameState of
                        NoGame ->
                            SnakeModel.init

                        PlayingSnake m ->
                            m

                newModel : SnakeModel.Model
                newModel =
                    SnakeUpdate.update snakeMsg snakeModel
            in
            ( { model | gameState = PlayingSnake newModel }, Cmd.none )


gameUrl : Game -> String
gameUrl game =
    "/" ++ gameName game


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
                            (E.link
                                [ Bg.color (E.rgb255 85 131 200)
                                , E.padding 10
                                ]
                                { url = gameUrl game
                                , label = text (gameName game)
                                }
                            )
                    )
            )


view : Model -> Browser.Document Msg
view model =
    let
        formatTitle : String -> String
        formatTitle title =
            "Boston Elm Arcade - " ++ title
    in
    case model.gameState of
        NoGame ->
            Browser.Document
                (formatTitle "Choose a Game!")
                [ noGame model ]

        PlayingSnake snakeModel ->
            Browser.Document
                (formatTitle (gameName Snake))
                [ Html.map SnakeMsg (SnakeView.view snakeModel) ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        NoGame ->
            Sub.none

        PlayingSnake snakeModel ->
            SnakeUpdate.subs snakeModel |> Sub.map SnakeMsg


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
