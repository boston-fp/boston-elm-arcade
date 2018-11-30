module Games.Sheep exposing (Doggo, Model, Msg(..), Sheep, init, integratePos, main, subscriptions, update, view)

import Browser
import Browser.Events
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render as Render exposing (svg)
import Collage.Text exposing (..)
import Color exposing (Color, rgb)
import Html exposing (Html)
import Html.Attributes as Hattr
import Json.Decode
import Key exposing (Key(..), KeyType(..))
import P2 exposing (P2(..))
import Radians
import V2 exposing (V2(..))


type alias Model =
    { doggo : Doggo
    , sheep : List Sheep
    , windowSize : WindowSize
    }


type alias WindowSize =
    { widthPx : Int
    , heightPx : Int
    }


type Msg
    = Tick Float
    | KeyEvent Key.Event
    | WindowResized WindowSize


type alias Doggo =
    { pos : P2
    , up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    , angle : Float
    }


type alias Sheep =
    { pos : P2
    , vel : V2
    , mass : Float
    }


integratePos : Float -> { r | pos : P2, vel : V2 } -> P2
integratePos dt entity =
    P2.add entity.pos (V2.scale dt entity.vel)


{-| Calculate a sheep's velocity, as a pure function of inputs. Currently,
that's just the doggo (but in the future will include the other shep).
-}
calculateSheepVelocity : Doggo -> Sheep -> V2
calculateSheepVelocity doggo shep =
    V2.maxNorm 0.05 (repel doggo shep)


{-| 'repel p q' calculates a vector pointing away from 'p', with norm
proportional to the inverse square of the distance bewteen 'p' and 'q'.
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
            V2.fromDegrees doggo.angle

        multiplier =
            case bearingDoggo doggo of
                Forward ->
                    1

                Halt ->
                    0

                Back ->
                    -1
    in
    V2.scale multiplier vec


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
    , sheep =
        [ Sheep (P2 50 -150) (V2 4 8) 0.5
        , Sheep (P2 -100 50) (V2 0 0) 1
        , Sheep (P2 200 -50) (V2 0 0) 0.7
        , Sheep (P2 100 -50) (V2 0 0) 4
        , Sheep (P2 -50 100) (V2 0 0) 0.2
        ]
    , windowSize = WindowSize 0 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        pure x =
            ( x, Cmd.none )
    in
    case msg of
        Tick dt ->
            let
                sheep1 : List Sheep
                sheep1 =
                    List.map
                        (\sheep ->
                            { sheep
                                | pos = integratePos dt sheep
                                , vel = calculateSheepVelocity model.doggo sheep
                            }
                        )
                        model.sheep
            in
            ( { model
                | doggo = moveDoggo dt model
                , sheep = sheep1
              }
            , Cmd.none
            )

        WindowResized size ->
            ( { model | windowSize = size }, Cmd.none )

        KeyEvent e ->
            pure { model | doggo = pointDoggo e model.doggo }


pointDoggo : Key.Event -> Doggo -> Doggo
pointDoggo e doggo =
    doggo



-- case ( e, doggo.bearing ) of
--     ( KeyDown Key.Up, _ ) ->
--         { doggo | bearing = Forward }


moveDoggo : Float -> { x | doggo : Doggo } -> Doggo
moveDoggo dt x =
    let
        doggo =
            x.doggo

        newPos =
            integratePos dt
                { pos = x.doggo.pos, vel = doggoVel doggo }
    in
    { doggo | pos = newPos }


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
        [ svg <| group <| viewDoggo model.doggo :: List.map viewSheep model.sheep

        -- , Html.text (Debug.toString model)
        ]


viewSheep : Sheep -> Collage Msg
viewSheep sheep =
    group
        [ rectangle
            -- body
            36
            24
            |> filled (uniform (rgb 220 220 220))

        -- |> shift ( sheep.pos.x, sheep.pos.y )
        , rectangle
            -- head
            8
            8
            |> filled (uniform (rgb 20 20 20))
            |> shift ( 20, 0 )
        ]
        |> scale sheep.mass
        |> rotate (Radians.unwrap (V2.toRadians sheep.vel))
        |> shift ( P2.x sheep.pos, P2.y sheep.pos )


viewDoggo : Doggo -> Collage Msg
viewDoggo doggo =
    group
        [ rectangle
            -- body
            36
            20
            |> filled (uniform (rgb 148 80 0))
        , group
            -- head
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
        , rectangle
            24
            4
            |> filled (uniform (rgb 148 80 0))
            |> shift ( -20, 0 )
        ]
        |> rotate (degrees doggo.angle)
        |> shift ( P2.x doggo.pos, P2.y doggo.pos )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (Json.Decode.map (KeyEvent << KeyDown) Key.decoder)
        , Browser.Events.onKeyUp (Json.Decode.map (KeyEvent << KeyDown) Key.decoder)
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
