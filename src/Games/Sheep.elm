module Games.Sheep exposing (Doggo, Model, Msg(..), Pos, Sheep, Vel, init, integratePos, subscriptions, update, view)

import Browser.Events
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes as Hattr
import Json.Decode
import Key exposing (Key(..))


type alias Model =
    { doggo : Doggo
    , sheep : List Sheep
    , lastmsg : Maybe Msg
    }


type Msg
    = Tick Float
    | Key Key


type alias Doggo =
    { pos : Pos
    , vel : Vel
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



-- | Update an entity's position with its velocity.


integratePos : Float -> { r | pos : Pos, vel : Vel } -> { r | pos : Pos, vel : Vel }
integratePos dt entity =
    let
        pos1 =
            { x = entity.pos.x + dt * entity.vel.x
            , y = entity.pos.y + dt * entity.vel.y
            }
    in
    { entity | pos = pos1 }


init : Model
init =
    { lastmsg = Nothing
    , doggo =
        { pos = { x = 0, y = 0 }
        , vel = { x = 0, y = 0 }
        }
    , sheep = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( { model | lastmsg = Just msg }, Cmd.none )


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
        [ Hattr.style "background-color" "rgb(20,20,20)"
        , Hattr.style "width" "100wh"
        , Hattr.style "height" "100vh"
        , Hattr.style "display" "flex"
        , Hattr.style "flex-direction" "column"
        , Hattr.style "align-items" "center"
        , Hattr.style "justify-content" "center"
        ]
        [ svg <| group <| [ title ] ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (Json.Decode.map Key Key.decoder)
        ]
