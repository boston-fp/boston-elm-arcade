module Games.Sheep exposing (..)

import Browser
import Browser.Events
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render as Render exposing (svg)
import Collage.Text exposing (..)
import Color exposing (..)
import Dict.Any exposing (AnyDict)
import Games.Sheep.Bork as Bork exposing (Borks)
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


type alias WindowSize =
    { widthPx : Int
    , heightPx : Int
    }


type Msg
    = Frame Float
    | KeyEvent Key.Event
    | WindowResized WindowSize


updateFlock :
    Config
    -> List Fence
    -> Doggo
    -> List Sheep
    -> Eff { ro | frames : Float } { rw | seed : Random.Seed } (List Sheep)
updateFlock config fences doggo =
    Eff.sequenceList
        << SelectList.selectedMapForList
            (\flock ->
                let
                    ( herd1, sheep, herd2 ) =
                        SelectList.toTuple flock
                in
                Sheep.update config fences doggo (herd1 ++ herd2) sheep
            )


init : Model
init =
    let
        randomSheep : Eff ro { rw | seed : Random.Seed } Sheep
        randomSheep =
            Eff.pure
                (\px py vx vy mass color ->
                    { pos = P2 px py
                    , vel = V2 vx vy
                    , mass = mass
                    , food = 1
                    , state = Sheep.Flocking
                    , color = color
                    }
                )
                |> Eff.ap (Eff.random (Random.float -400 400))
                |> Eff.ap (Eff.random (Random.float -400 400))
                |> Eff.ap (Eff.random (Random.float -4 4))
                |> Eff.ap (Eff.random (Random.float -4 4))
                |> Eff.ap (Eff.random (Random.float 1 1.2))
                |> Eff.ap (Eff.random (Random.uniform Sheep.Black [ Sheep.Brown, Sheep.White ]))
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
    , sheep =
        Tuple.first
            (Eff.run
                (Eff.replicate 100 randomSheep)
                ()
                { seed = Random.initialSeed 0 }
            )
    , windowSize = WindowSize 0 0
    , totalFrames = 0
    , seed = Random.initialSeed 0
    , config =
        { maxSheepVelocity = 3
        , minSheepVelocity = 0.1
        , sheepTurnRate = 0.05
        , crawlingDoggoVelocity = 3
        , walkingDoggoVelocity = 5
        , runningDoggoVelocity = 10
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
                newFlockEff : Eff { ro | frames : Float } { rw | seed : Random.Seed } (List Sheep)
                newFlockEff =
                    updateFlock model.config model.fences model.doggo model.sheep

                newDoggoEff : Eff { ro | frames : Float } any Doggo
                newDoggoEff =
                    Doggo.update model.config model.controller model.doggo

                ( ( newFlock, newDoggo ), newSeed ) =
                    Eff.run
                        (Eff.map2
                            (\flock doggo -> ( flock, doggo ))
                            newFlockEff
                            newDoggoEff
                        )
                        { frames = frames }
                        { seed = model.seed }
            in
            noCmd
                { model
                    | doggo = newDoggo
                    , sheep = newFlock
                    , totalFrames = model.totalFrames + frames
                    , borks = Bork.stepBorks frames model.borks
                    , seed = newSeed.seed
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
                                    | borks = Bork.newBork model.config newController model.doggo model.borks
                                    , controller = newController
                                }

                        _ ->
                            noCmd { model | controller = newController }

                Controller.Fall button_ newController ->
                    noCmd { model | controller = newController }

                Controller.Noop newController ->
                    noCmd { model | controller = newController }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (\s -> Frame (s * 3 / 50))
        , Browser.Events.onKeyDown (Json.Decode.map (KeyEvent << KeyDown) Key.decoder)
        , Browser.Events.onKeyUp (Json.Decode.map (KeyEvent << KeyUp) Key.decoder)
        , Browser.Events.onResize (\w h -> WindowResized (WindowSize w h))
        ]


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
                    , List.map viewFence model.fences
                    , List.map viewSheep model.sheep
                    ]

        -- , Html.text (Debug.toString model)
        ]


viewBorks : Borks -> Collage msg
viewBorks borks =
    Dict.Any.toList borks
        |> List.map
            (\( pos, bork ) ->
                Collage.Text.fromString "bork!"
                    |> rendered
                    |> shift (P2.asTuple pos)
                    |> scale bork.framesLeft
            )
        |> group


viewDoggo : Doggo -> Float -> Collage msg
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


viewFence : Fence -> Collage msg
viewFence ( P2 x1 y1, P2 x2 y2 ) =
    traced
        (solid 10 (uniform black))
        (segment ( x1, y1 ) ( x2, y2 ))


viewSheep : Sheep -> Collage msg
viewSheep sheep =
    let
        color =
            case sheep.state of
                Sheep.Flocking ->
                    rgb 255 0 0

                Sheep.Grazing ->
                    rgb 0 255 0

                Sheep.Sleeping ->
                    rgb 0 0 255

        radii c =
            group
                [ circle Sheep.gAwarenessRadius
                    |> outlined (dot thin (uniform black))
                , circle Sheep.gComfortZoneRadius
                    |> outlined (dot thin (uniform green))
                , circle Sheep.gPersonalSpaceRadius
                    |> outlined (dot thin (uniform red))
                , c
                ]
    in
    group
        [ rectangle
            4
            4
            |> filled (uniform color)
        , rectangle
            36
            24
            |> filled
                (uniform
                    (case sheep.color of
                        Sheep.White ->
                            rgb 220 220 220

                        Sheep.Black ->
                            rgb 40 40 40

                        Sheep.Brown ->
                            rgb 139 69 19
                    )
                )
        , rectangle
            8
            8
            |> filled (uniform (rgb 20 20 20))
            |> shift ( 20, 0 )
        ]
        |> scale sheep.mass
        |> (if False then
                radii

            else
                identity
           )
        |> rotate (V2.toRadians sheep.vel)
        |> shift ( P2.x sheep.pos, P2.y sheep.pos )


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> ( init, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }
