module Games.Sheep exposing (Doggo, Model, Msg(..), Pos, Sheep, Vel, init, integratePos, subscriptions, update, view)

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


type alias Model =
    { doggo : Doggo
    , sheep : List Sheep
    }


type Msg
    = Tick Float
    | KeyEvent Key.Event


type alias Doggo =
    { pos : Pos
    , bearing : Bearing
    , angle : Float
    }


type alias Sheep =
    { pos : Pos
    , vel : Vel
    }


type alias Pos =
    { x : Float
    , y : Float
    }


type alias Vel =
    { x : Float
    , y : Float
    }


vminus : Vel -> Vel -> Vel
vminus v1 v2 =
    { x = v1.x - v2.x
    , y = v1.y - v2.y
    }


vscale : Vel -> Float -> Vel
vscale { x, y } s =
    { x = s * x
    , y = s * y
    }


vmagnitude : Vel -> Float
vmagnitude vel =
    sqrt (vel.x * vel.x + vel.y * vel.y)


integratePos : Float -> { pos : Pos, vel : Vel } -> Pos
integratePos dt entity =
    let
        pos1 =
            { x = entity.pos.x + dt * entity.vel.x
            , y = entity.pos.y + dt * entity.vel.y
            }
    in
    pos1



-- Calculate a sheep's velocity, as a pure function of inputs. Currently, that's
-- just the doggo (but in the future will include the other shep).


calculateSheepVelocity : Doggo -> Sheep -> Vel
calculateSheepVelocity doggo shep =
    let
        -- Vector pointing from dog to sheep
        v1 =
            vminus shep.pos doggo.pos
    in
    vscale v1 (min 2 (1 / vmagnitude v1))


type Bearing
    = Forward
    | Halt
    | Back


doggoVel : Doggo -> Vel
doggoVel { bearing, angle } =
    let
        vec =
            { x = cos angle
            , y = sin angle
            }

        multiplier =
            case bearing of
                Forward ->
                    1

                Halt ->
                    0

                Back ->
                    -1
    in
    vscale vec (multiplier * vmagnitude vec)


init : Model
init =
    { doggo =
        { pos = { x = 0, y = 0 }
        , bearing = Halt
        , angle = 0
        }
    , sheep =
        [ Sheep (Pos 50 -150) (Vel 0 0)
        , Sheep (Pos -100 50) (Vel 0 0)
        , Sheep (Pos 200 -50) (Vel 0 0)
        , Sheep (Pos 100 -50) (Vel 0 0)
        , Sheep (Pos -50 100) (Vel 0 0)
        ]
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

        KeyEvent e ->
            pure <|
                case e of
                    KeyUp k ->
                        model

                    KeyDown k ->
                        model


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
        , Html.text (Debug.toString model)
        ]


viewSheep : Sheep -> Collage Msg
viewSheep sheep =
    group
        [ rectangle
            -- body
            24
            36
            |> filled (uniform (rgb 220 220 220))

        -- |> shift ( sheep.pos.x, sheep.pos.y )
        , rectangle
            -- head
            8
            8
            |> filled (uniform (rgb 20 20 20))
            |> shift ( 0, 20 )
        ]
        |> shift ( sheep.pos.x, sheep.pos.y )


viewDoggo : Doggo -> Collage Msg
viewDoggo doggo =
    group
        [ rectangle
            -- body
            20
            36
            |> filled (uniform (rgb 148 80 0))
        , group
            -- head
            [ rectangle
                12
                14
                |> filled (uniform (rgb 148 80 0))
            , rectangle
                16
                8
                |> filled (uniform (rgb 148 80 0))
            ]
            |> shift ( 0, 24 )
        ]
        |> shift ( doggo.pos.x, doggo.pos.y )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (Json.Decode.map (KeyEvent << KeyDown) Key.decoder)
        , Browser.Events.onKeyUp (Json.Decode.map (KeyEvent << KeyDown) Key.decoder)
        ]
