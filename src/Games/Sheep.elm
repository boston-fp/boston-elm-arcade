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
    , lastmsg : Maybe Msg
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
            vminus shep.vel (doggoVel doggo)
    in
    vscale v1 (min 2 (1 / vmagnitude v1))


doggoVel : Doggo -> Vel
doggoVel _ =
    { x = 0, y = 0 }


init : Model
init =
    { lastmsg = Nothing
    , doggo =
        { pos = { x = 0, y = 0 }
        , bearing = Halt
        , angle = 0
        }
    , sheep =
        [ Sheep (Pos 50 -150) (Vel 0 0)
        , Sheep (Pos 0 0) (Vel 0 0)
        , Sheep (Pos 200 -50) (Vel 0 0)
        , Sheep (Pos 100 -50) (Vel 0 0)
        , Sheep (Pos -50 100) (Vel 0 0)
        ]
    }


type Bearing
    = Forward
    | Halt
    | Back


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        logged x =
            ( { x
                | lastmsg = Just msg
              }
            , Cmd.none
            )
    in
    case msg of
        Tick dt ->
            logged
                (moveDoggo dt model)

        KeyEvent e ->
            logged <|
                case e of
                    KeyUp k ->
                        Debug.todo ""

                    KeyDown k ->
                        Debug.todo ""


moveDoggo : Float -> { x | doggo : Doggo } -> { x | doggo : Doggo }
moveDoggo dt x =
    let
        doggo =
            x.doggo

        newPosVel =
            integratePos dt { pos = x.doggo.pos, vel = Debug.todo "" }
    in
    { x
        | doggo = doggo
    }


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
        [ svg <| group <| List.map viewSheep model.sheep ]


viewSheep : Sheep -> Collage Msg
viewSheep sheep =
    rectangle
        24
        36
        |> filled (uniform (rgb 220 220 220))
        |> shift ( sheep.pos.x, sheep.pos.y )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (Json.Decode.map (KeyEvent << KeyDown) Key.decoder)
        , Browser.Events.onKeyUp (Json.Decode.map (KeyEvent << KeyDown) Key.decoder)
        ]
