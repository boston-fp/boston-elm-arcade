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


{-| Update an entity's position with its velocity.
-}
integratePos : Float -> { r | pos : Pos, vel : Vel } -> { r | pos : Pos, vel : Vel }
integratePos dt entity =
    let
        pos1 =
            { x = entity.pos.x + dt * entity.vel.x
            , y = entity.pos.y + dt * entity.vel.y
            }
    in
    { entity | pos = pos1 }

-- Calculate a sheep's velocity, as a pure function of inputs. Currently, that's
-- just the doggo (but in the future will include the other shep).
calculateSheepVelocity : Doggo -> Sheep -> Vel
calculateSheepVelocity doggo shep =
  let
      -- Vector pointing from dog to sheep
      v1 = vminus shep.vel doggo.vel
      one_over_mag = 1 / vmagnitude v1
  in
      vscale (vscale v1 one_over_mag) (min 2 one_over_mag)


init : Model
init =
    { lastmsg = Nothing
    , doggo =
        { pos = { x = 0, y = 0 }
        , vel = { x = 0, y = 0 }
        }
    , sheep =
        [ Sheep (Pos 50 -150) (Vel 0 0)
        , Sheep (Pos 0 0) (Vel 0 0)
        , Sheep (Pos 200 -50) (Vel 0 0)
        , Sheep (Pos 100 -50) (Vel 0 0)
        , Sheep (Pos -50 100) (Vel 0 0)
        ]
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
        , Browser.Events.onKeyDown (Json.Decode.map Key Key.decoder)
        ]
