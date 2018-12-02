module Games.Sheep exposing (Doggo, Model, Msg(..), init, integratePos, main, subscriptions, update, view)

import Browser
import Browser.Events
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render as Render exposing (svg)
import Collage.Text as Text exposing (..)
import Color exposing (Color, rgb)
import Dict.Any exposing (AnyDict)
import Games.Sheep.Sheep as Sheep exposing (Sheep)
import Html exposing (Html)
import Html.Attributes as Hattr
import Json.Decode
import Key exposing (Key(..), KeyType(..))
import P2 exposing (P2(..))
import Radians exposing (Radians)
import SelectList exposing (SelectList)
import V2 exposing (V2(..))


type alias Model =
    { doggo : Doggo
    , sheep : List Sheep
    , borks : Borks
    , windowSize : WindowSize
    , totalFrames : Float
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


type alias Doggo =
    { pos : P2
    , up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    , angle : Radians
    }


type alias Bork =
    { dir : V2
    , framesLeft : Float
    }


newBork : Doggo -> Borks -> Borks
newBork doggo =
    Dict.Any.update doggo.pos
        (\existingBork ->
            case existingBork of
                Just bork ->
                    Just bork

                Nothing ->
                    Just
                        { dir = V2.signorm (doggoVel doggo)
                        , framesLeft = 10
                        }
        )


stepBorks : Float -> Borks -> Borks
stepBorks frames borks =
    borks
        |> Dict.Any.map (\_ bork -> { bork | framesLeft = bork.framesLeft - 1 })
        |> Dict.Any.filter (\_ bork -> bork.framesLeft > 0)


integratePos : Float -> { r | pos : P2, vel : V2 } -> P2
integratePos frames entity =
    P2.add entity.pos (V2.scale frames entity.vel)


updateFlock : Float -> Doggo -> (List Sheep -> List Sheep)
updateFlock frames doggo =
    SelectList.selectedMapForList
        (\flock ->
            let
                ( herd1, sheep, herd2 ) =
                    SelectList.toTuple flock
            in
            Sheep.update frames (herd1 ++ herd2) sheep
        )



-- let
--     sheep1 : List Sheep
--     sheep1 =
--         List.map
--             (\sheep ->
--                 { sheep
--                     | pos = integratePos frames sheep
--                     , vel = calculateSheepVelocity model.doggo sheep
--                 }
--             )
--             model.sheep
-- in
-- noCmd
--     { model
--         | doggo = moveDoggo frames model.doggo
--         , sheep = sheep1
--     }


{-| 'repel p q' calculates a vector pointing from 'p' to 'q', with norm
proportional to the inverse square of the distance bewteen 'p' and 'q'.

        ↗    <-- returned vector
      q

p

-}
repel : { r | pos : P2 } -> { s | pos : P2 } -> V2
repel pariah senpai =
    let
        vec =
            P2.diff senpai.pos pariah.pos
    in
    V2.scale (1 / V2.quadrance vec) vec


type Bearing
    = Forward
    | Halt
    | Back


doggoVel : Doggo -> V2
doggoVel doggo =
    let
        vec =
            V2.fromRadians doggo.angle

        magnitude =
            10

        sign =
            case bearingDoggo doggo of
                Forward ->
                    1

                Halt ->
                    0

                Back ->
                    -1
    in
    V2.scale (magnitude * sign) vec


bearingDoggo : Doggo -> Bearing
bearingDoggo d =
    case ( d.up, d.down ) of
        ( True, True ) ->
            Halt

        ( True, False ) ->
            Forward

        ( False, True ) ->
            Back

        ( False, False ) ->
            Halt


init : Model
init =
    { doggo =
        { pos = P2 0 0
        , up = False
        , down = False
        , left = False
        , right = False
        , angle = 0
        }
    , borks = Dict.Any.empty P2.asTuple
    , sheep =
        [ Sheep (P2 50 -150) (V2 4 8) 0.5 1 Sheep.Flocking
        , Sheep (P2 -100 50) (V2 0 0) 1 1 Sheep.Flocking
        , Sheep (P2 200 -50) (V2 0 0) 0.7 1 Sheep.Flocking
        , Sheep (P2 100 -50) (V2 0 0) 2 1 Sheep.Flocking
        , Sheep (P2 -50 100) (V2 0 0) 0.4 1 Sheep.Flocking
        , Sheep (P2 -100 -50) (V2 0 0) 0.8 1 Sheep.Flocking
        , Sheep (P2 0 -100) (V2 0 0) 1.2 1 Sheep.Flocking
        ]
    , windowSize = WindowSize 0 0
    , totalFrames = 0
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
                newFlock =
                    updateFlock frames model.doggo model.sheep

                newDoggo =
                    moveDoggo frames model.doggo
            in
            noCmd
                { model
                    | doggo = newDoggo
                    , sheep = newFlock
                    , totalFrames = model.totalFrames + frames
                    , borks = stepBorks frames model.borks
                }

        WindowResized size ->
            noCmd { model | windowSize = size }

        KeyEvent e ->
            noCmd <|
                case e of
                    KeyDown Key.Space ->
                        { model | borks = newBork model.doggo model.borks }

                    _ ->
                        { model | doggo = setKey e model.doggo }


setKey : Key.Event -> Doggo -> Doggo
setKey e doggo =
    case e of
        KeyUp Key.Left ->
            { doggo | left = False }

        KeyUp Key.Right ->
            { doggo | right = False }

        KeyDown Key.Left ->
            { doggo | left = True }

        KeyDown Key.Right ->
            { doggo | right = True }

        KeyUp Key.Up ->
            { doggo | up = False }

        KeyUp Key.Down ->
            { doggo | down = False }

        KeyDown Key.Up ->
            { doggo | up = True }

        KeyDown Key.Down ->
            { doggo | down = True }

        _ ->
            doggo


moveDoggo : Float -> Doggo -> Doggo
moveDoggo frames doggo =
    let
        turntRate =
            Basics.pi / 50

        angleDt : Radians
        angleDt =
            turntRate
                * frames
                * (case ( doggo.left, doggo.right ) of
                    ( True, False ) ->
                        1

                    ( False, True ) ->
                        -1

                    _ ->
                        0
                  )

        newPos : P2
        newPos =
            integratePos frames
                { pos = doggo.pos
                , vel = doggoVel doggo
                }
    in
    { doggo | pos = newPos, angle = doggo.angle + angleDt }


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
                viewBorks model.borks
                    :: viewDoggo model.doggo model.totalFrames
                    :: List.map Sheep.view model.sheep

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
                |> rotate (sin (frames / (pi * 4)) / 4)
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
