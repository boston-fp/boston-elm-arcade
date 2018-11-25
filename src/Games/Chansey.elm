module Games.Chansey exposing (..)

import Browser
import Browser.Events
import Games.Chansey.Basket as Basket exposing (Basket)
import Games.Chansey.Column exposing (Column(..))
import Games.Chansey.Egg as Egg exposing (Egg)
import Games.Chansey.EggTimer as EggTimer exposing (EggTimer, EggTimerStep(..))
import Games.Chansey.EggType as EggType exposing (EggType(..))
import Games.Chansey.Types exposing (..)
import Json.Decode
import Random
import RecurringTimer exposing (RecurringTimer)


type alias Model =
    { basket : Basket
    , eggs : List Egg
    , eggtimer : EggTimer
    , numeggs : Int -- Number of non-bomb eggs that have fallen
    , score : Int
    , paused : Bool
    , lastkey : String
    , done : Bool
    }


type Msg
    = Tick Milliseconds
    | Keydown String
    | Keyup String


init : Model
init =
    { basket = Basket.new Center
    , eggs = []
    , eggtimer =
        EggTimer.new
            (Random.initialSeed 1)
            (RecurringTimer.newWithJitter (Random.initialSeed 2) (Random.float 100 300))
    , numeggs = 0
    , score = 0
    , paused = False
    , lastkey = ""
    , done = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( updateTick delta model, Cmd.none )

        Keydown "p" ->
            ( { model | basket = Basket.update Basket.RightDown model.basket }, Cmd.none )

        Keydown "q" ->
            ( { model | basket = Basket.update Basket.LeftDown model.basket }, Cmd.none )

        Keydown " " ->
            ( { model | paused = not model.paused }, Cmd.none )

        Keydown key ->
            ( { model | lastkey = key }, Cmd.none )

        Keyup "p" ->
            ( { model | basket = Basket.update Basket.RightUp model.basket }, Cmd.none )

        Keyup "q" ->
            ( { model | basket = Basket.update Basket.LeftUp model.basket }, Cmd.none )

        Keyup _ ->
            ( model, Cmd.none )


{-| Step eggs forward by a frame. Return the eggs remaining and the eggs caught
(which, if the speed is not set insanely high, should have at most one element).
-}
stepEggs :
    { delta : Milliseconds, basket : Column }
    -> List Egg
    -> { remaining : List Egg, caught : List EggType }
stepEggs { delta, basket } =
    let
        step :
            Egg
            -> { remaining : List Egg, caught : List EggType }
            -> { remaining : List Egg, caught : List EggType }
        step egg { remaining, caught } =
            let
                egg1 =
                    Egg.fall delta egg
            in
            if egg1.y <= -400 then
                { remaining = remaining, caught = caught }
            else if egg.y > -300 && egg1.y <= -300 && egg.column == basket then
                { remaining = remaining, caught = egg.typ :: caught }
            else
                { remaining = egg1 :: remaining, caught = caught }
    in
    List.foldr
        step
        { remaining = [], caught = [] }


updateTick : Milliseconds -> Model -> Model
updateTick delta model =
    let
        { remaining, caught } =
            stepEggs
                { delta = delta, basket = Basket.column model.basket }
                model.eggs

        ( eggtimer1, newEgg ) =
            case EggTimer.step delta model.eggtimer of
                EggTimerStep eggtimer_ ->
                    ( eggtimer_, Nothing )

                EggTimerFire eggtimer_ newEgg_ ->
                    if model.numeggs <= 99 then
                        ( eggtimer_, Just newEgg_ )
                    else
                        ( eggtimer_, Nothing )
    in
    { model
        | eggs =
            case newEgg of
                Nothing ->
                    remaining

                Just egg ->
                    egg :: remaining
        , eggtimer = eggtimer1
        , numeggs =
            case newEgg of
                Nothing ->
                    model.numeggs

                Just egg ->
                    case egg.typ of
                        EggTypeEgg ->
                            model.numeggs + 1

                        EggTypeBomb ->
                            model.numeggs
        , score = model.score + List.sum (List.map EggType.score caught)
    }


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
