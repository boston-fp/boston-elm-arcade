module Games.Chansey exposing (..)

import Browser
import Browser.Events
import Games.Chansey.Basket as Basket exposing (Basket)
import Games.Chansey.Column exposing (Column(..))
import Games.Chansey.Egg as Egg exposing (Egg)
import Games.Chansey.EggTimer as EggTimer exposing (EggTimer, EggTimerStep(..))
import Games.Chansey.EggType as EggType exposing (EggType(..))
import Games.Chansey.Level as Level
import Games.Chansey.Types exposing (..)
import Json.Decode
import Random
import RecurringTimer exposing (RecurringTimer)


type alias Model =
    { level : Level.Model
    , paused : Bool
    , lastkey : String
    }


type Msg
    = Tick Milliseconds
    | Keydown String
    | Keyup String


init : Model
init =
    { level = Level.init { numeggs = 100 }
    , paused = False
    , lastkey = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( { model | level = Level.update (Level.Tick delta) model.level }
            , Cmd.none
            )

        Keydown "p" ->
            ( { model | level = Level.update Level.RightPaddleDown model.level }
            , Cmd.none
            )

        Keydown "q" ->
            ( { model
                | level = Level.update Level.LeftPaddleDown model.level
              }
            , Cmd.none
            )

        Keydown " " ->
            ( { model
                | paused = not model.paused
              }
            , Cmd.none
            )

        Keydown key ->
            ( { model
                | lastkey = key
              }
            , Cmd.none
            )

        Keyup "p" ->
            ( { model
                | level = Level.update Level.RightPaddleUp model.level
              }
            , Cmd.none
            )

        Keyup "q" ->
            ( { model | level = Level.update Level.LeftPaddleUp model.level }
            , Cmd.none
            )

        Keyup _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keyDecoder : Json.Decode.Decoder String
        keyDecoder =
            Json.Decode.field "key" Json.Decode.string
    in
    Sub.batch
        [ if model.paused then
            Sub.none
          else
            Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (Json.Decode.map Keydown keyDecoder)
        , Browser.Events.onKeyUp (Json.Decode.map Keyup keyDecoder)
        ]
