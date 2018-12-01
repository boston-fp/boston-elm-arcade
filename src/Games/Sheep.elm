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
import Radians exposing (Radians(..))
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


type alias Sheep =
    { pos : P2
    , vel : V2
    , mass : Float
    }


integratePos : Float -> { r | pos : P2, vel : V2 } -> P2
integratePos frames entity =
    P2.add entity.pos (V2.scale frames entity.vel)


{-| Calculate a sheep's velocity, as a pure of inputs. Currently,
that's just the doggo (but in the future will include the other shep).
-}
calculateSheepVelocity : Doggo -> Sheep -> V2
calculateSheepVelocity doggo shep =
    repel doggo shep
        |> V2.scale (100 / shep.mass)
        |> V2.maxNorm maxSheepVelocity


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

        multiplier =
            16
                * (case bearingDoggo doggo of
                    Forward ->
                        1

                    Halt ->
                        0

                    Back ->
                        -1
                  )
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
        , angle = Radians 0
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
        noCmd x =
            ( x, Cmd.none )
    in
    case msg of
        Frame frames ->
            let
                sheep1 : List Sheep
                sheep1 =
                    List.map
                        (\sheep ->
                            { sheep
                                | pos = integratePos frames sheep
                                , vel = calculateSheepVelocity model.doggo sheep
                            }
                        )
                        model.sheep
            in
            noCmd
                { model
                    | doggo = moveDoggo frames model.doggo
                    , sheep = sheep1
                }

        WindowResized size ->
            noCmd { model | windowSize = size }

        KeyEvent e ->
            noCmd { model | doggo = setKey e model.doggo }


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
            Radians <|
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
    { doggo | pos = newPos, angle = Radians.add doggo.angle angleDt }


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
            36
            24
            |> filled (uniform (rgb 220 220 220))
        , rectangle
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
                24
                4
                |> filled (uniform (rgb 148 80 0))
                |> shift ( -10, 0 )
    in
    group [ body |> at left tail, head ]
        |> rotate (radians <| Radians.unwrap doggo.angle)
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



-- Constants


maxSheepVelocity : Float
maxSheepVelocity =
    1
