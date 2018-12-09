module Games.Sheep exposing (..)

import Browser
import Browser.Events
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render as Render exposing (svg)
import Collage.Text as Text exposing (..)
import Color exposing (Color, rgb)
import Dict.Any exposing (AnyDict)
import Games.Sheep.Config exposing (Config)
import Games.Sheep.Controller as Controller exposing (Controller)
import Games.Sheep.Doggo as Doggo exposing (Doggo)
import Games.Sheep.Eff as Eff exposing (Eff)
import Games.Sheep.Fence as Fence exposing (Fence)
import Games.Sheep.Sheep as Sheep exposing (Sheep)
import Html exposing (Html)
import Html.Attributes as Hattr
import Json.Decode
import Key exposing (Key(..), KeyType(..))
import P2 exposing (P2(..))
import Radians exposing (Radians)
import Random
import SelectList exposing (SelectList)
import V2 exposing (V2(..))


type alias Model =
    { controller : Controller
    , doggo : Doggo
    , sheep : List Sheep
    , borks : Borks
    , fences : List Fence
    , windowSize : WindowSize
    , totalFrames : Float
    , seed : Random.Seed
    , config : Config
    }


type alias Borks =
    AnyDict ( Float, Float ) P2 Bork


type alias WindowSize =
    { widthPx : Int
    , heightPx : Int
    }


type Msg
    = Frame Float
    | KeyEvent Key.Event
    | WindowResized WindowSize


type alias Bork =
    { dir : V2
    , framesLeft : Float
    }


newBork : Config -> Controller -> Doggo -> Borks -> Borks
newBork config controller doggo =
    Dict.Any.update doggo.pos
        (\existingBork ->
            case existingBork of
                Just bork ->
                    Just bork

                Nothing ->
                    Just
                        { dir = V2.signorm (Doggo.doggoVel config controller doggo)
                        , framesLeft = 10
                        }
        )


stepBorks : Float -> Borks -> Borks
stepBorks frames borks =
    borks
        |> Dict.Any.map (\_ bork -> { bork | framesLeft = bork.framesLeft - 1 })
        |> Dict.Any.filter (\_ bork -> bork.framesLeft > 0)


updateFlock :
    Config
    -> Random.Seed
    -> Float
    -> List Fence
    -> Doggo
    -> (List Sheep -> List Sheep)
updateFlock config seed frames fences doggo =
    SelectList.selectedMapForList
        (\flock ->
            let
                ( herd1, sheep, herd2 ) =
                    SelectList.toTuple flock
            in
            Sheep.update config seed frames fences doggo (herd1 ++ herd2) sheep
        )


init : Model
init =
    let
        randomSheep : Random.Seed -> ( Sheep, Random.Seed )
        randomSheep seed0 =
            let
                ( px, seed1 ) =
                    Random.step (Random.float -400 400) seed0

                ( py, seed2 ) =
                    Random.step (Random.float -400 400) seed1

                ( vx, seed3 ) =
                    Random.step (Random.float -4 4) seed2

                ( vy, seed4 ) =
                    Random.step (Random.float -4 4) seed3

                ( m, seed5 ) =
                    Random.step (Random.float 1 1.1) seed4

                ( c, seed6 ) =
                    Random.step (Random.uniform Sheep.Black [ Sheep.Brown, Sheep.White ]) seed5
            in
            ( { pos = P2 px py
              , vel = V2 vx vy
              , mass = m
              , food = 1
              , state = Sheep.Flocking
              , color = c
              }
            , seed6
            )

        randomFlock : Int -> Random.Seed -> List Sheep
        randomFlock n seed0 =
            if n == 0 then
                []

            else
                let
                    ( sheep, seed1 ) =
                        randomSheep seed0
                in
                sheep :: randomFlock (n - 1) seed1
    in
    { controller = Controller.new
    , doggo =
        { pos = P2 0 0
        , angle = 0
        }
    , borks = Dict.Any.empty P2.asTuple
    , fences =
        [ ( P2 -600 600, P2 600 600 )
        , ( P2 600 600, P2 600 -600 )
        , ( P2 600 -600, P2 -600 -600 )
        , ( P2 -600 -600, P2 -600 600 )
        ]
    , sheep = randomFlock 100 (Random.initialSeed 0)
    , windowSize = WindowSize 0 0
    , totalFrames = 0
    , seed = Random.initialSeed 0
    , config =
        { maxSheepVelocity = 3
        , minSheepVelocity = 0.1
        , sheepTurnRate = 0.05
        , crawlingDoggoTurnRate = pi / 60
        , walkingDoggoTurnRate = pi / 30
        , runningDoggoTurnRate = pi / 40
        }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noCmd x =
            ( x, Cmd.none )
    in
    case msg of
        Frame frames ->
            let
                ( freshSeed, newSeed ) =
                    Random.step Random.independentSeed model.seed

                newFlock =
                    updateFlock model.config freshSeed frames model.fences model.doggo model.sheep

                newDoggo =
                    Tuple.first <|
                        Eff.run
                            (Doggo.update
                                model.config
                                model.controller
                                model.doggo
                            )
                            { frames = frames }
                            ()
            in
            noCmd
                { model
                    | doggo = newDoggo
                    , sheep = newFlock
                    , totalFrames = model.totalFrames + frames
                    , borks = stepBorks frames model.borks
                    , seed = newSeed
                }

        WindowResized size ->
            noCmd { model | windowSize = size }

        KeyEvent event ->
            case Controller.update event model.controller of
                Controller.Rise button newController ->
                    case button of
                        Controller.Space ->
                            noCmd
                                { model
                                    | borks = newBork model.config newController model.doggo model.borks
                                    , controller = newController
                                }

                        _ ->
                            noCmd { model | controller = newController }

                Controller.Fall button_ newController ->
                    noCmd { model | controller = newController }

                Controller.Noop newController ->
                    noCmd { model | controller = newController }


view : Model -> Html Msg
view model =
    let
        title : Collage msg
        title =
            fromString "The Sheep Whisperer"
                |> size (huge * 4)
                |> color Color.red
                |> rendered
    in
    Html.div
        [ Hattr.style "background-color" "rgb(80,136,80)"
        , Hattr.style "width" "100wh"
        , Hattr.style "height" "100vh"
        , Hattr.style "display" "flex"
        , Hattr.style "flex-direction" "column"
        , Hattr.style "align-items" "center"
        , Hattr.style "justify-content" "center"
        ]
        [ svg <|
            group <|
                List.concat
                    [ [ viewDoggo model.doggo model.totalFrames ]
                    , [ viewBorks model.borks ]
                    , List.map Fence.view model.fences
                    , List.map Sheep.view model.sheep
                    ]

        -- , Html.text (Debug.toString model)
        ]


viewBorks : Borks -> Collage msg
viewBorks borks =
    Dict.Any.toList borks
        |> List.map
            (\( pos, bork ) ->
                Text.fromString "bork!"
                    |> rendered
                    |> shift (P2.asTuple pos)
                    |> scale bork.framesLeft
            )
        |> group


viewDoggo : Doggo -> Float -> Collage Msg
viewDoggo doggo frames =
    let
        body =
            rectangle
                36
                20
                |> filled (uniform (rgb 148 80 0))

        head =
            group
                [ rectangle
                    14
                    12
                    |> filled (uniform (rgb 148 80 0))
                , rectangle
                    8
                    16
                    |> filled (uniform (rgb 128 60 0))
                ]
                |> shift ( 24, 0 )

        tail =
            rectangle
                40
                4
                |> filled (uniform (rgb 148 80 0))
                |> shift ( -16, 0 )
                |> rotate (sin (frames / pi) / 2)
    in
    group [ body, tail, head ]
        |> rotate (radians doggo.angle)
        |> shift ( P2.x doggo.pos, P2.y doggo.pos )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (\s -> Frame (s * 3 / 50))
        , Browser.Events.onKeyDown (Json.Decode.map (KeyEvent << KeyDown) Key.decoder)
        , Browser.Events.onKeyUp (Json.Decode.map (KeyEvent << KeyUp) Key.decoder)
        , Browser.Events.onResize (\w h -> WindowResized (WindowSize w h))
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> ( init, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }
