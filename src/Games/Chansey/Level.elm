module Games.Chansey.Level exposing (..)

import Collage exposing (Collage)
import Collage.Text
import Color
import Games.Chansey.Basket as Basket exposing (Basket)
import Games.Chansey.Column as Column
import Games.Chansey.Egg as Egg exposing (Egg)
import Games.Chansey.EggTimer as EggTimer exposing (EggTimer, EggTimerStep(..))
import Games.Chansey.EggType as EggType exposing (EggType(..))
import Games.Chansey.Types exposing (..)
import Html
import Random
import RecurringTimer


type alias Model =
    { config : Config
    , basket : Basket
    , eggs : List Egg
    , eggtimer : EggTimer
    , score : Int
    }


type alias Config =
    { levelnum : Int
    , numeggs : Int
    , spawntimer : { min : Milliseconds, max : Milliseconds }
    , eggspeed : { min : Y_Per_Millisecond, max : Y_Per_Millisecond }
    }


type Control
    = Tick Milliseconds
    | LeftPaddleDown
    | LeftPaddleUp
    | RightPaddleDown
    | RightPaddleUp


type Status
    = Won
    | Lost
    | Ongoing


status : Model -> Status
status model =
    if EggTimer.done model.eggtimer && List.isEmpty model.eggs then
        if model.score == model.config.numeggs then
            Won
        else
            Lost
    else
        Ongoing


init : Config -> Model
init config =
    { config = config
    , basket = Basket.new Column.Center
    , eggs = []
    , eggtimer =
        EggTimer.new
            config.eggspeed
            (Random.initialSeed config.levelnum)
            (RecurringTimer.newWithJitter
                (Random.initialSeed (config.levelnum * 10000))
                (Random.float config.spawntimer.min config.spawntimer.max)
            )
            config.numeggs
    , score = 0
    }

step : Control -> Model -> Model
step control model =
    case control of
        Tick delta ->
            onTick delta model

        LeftPaddleDown ->
            { model | basket = Basket.step Basket.LeftDown model.basket }

        LeftPaddleUp ->
            { model | basket = Basket.step Basket.LeftUp model.basket }

        RightPaddleDown ->
            { model | basket = Basket.step Basket.RightDown model.basket }

        RightPaddleUp ->
            { model | basket = Basket.step Basket.RightUp model.basket }


onTick : Milliseconds -> Model -> Model
onTick delta model =
    let
        ( eggs1, caught ) =
            stepEggs
                delta
                model.basket
                model.eggs

        score1 : Int
        score1 =
            model.score + List.sum (List.map EggType.score caught)

        ( eggtimer1, newEgg ) =
            case EggTimer.step delta model.eggtimer of
                EggTimerStep eggtimer_ ->
                    ( eggtimer_, Nothing )

                EggTimerFire eggtimer_ newEgg_ ->
                    ( eggtimer_, Just newEgg_ )

                EggTimerDone ->
                    ( model.eggtimer, Nothing )
    in
    { model
        | eggs =
            case newEgg of
                Nothing ->
                    eggs1

                Just egg ->
                    egg :: eggs1
        , eggtimer = eggtimer1
        , score = score1
    }


{-| Step eggs forward by a frame. Return the eggs remaining and the eggs caught
(which, if the speed is not set insanely high, should have at most one element).
-}
stepEggs :
    Milliseconds
    -> Basket
    -> List Egg
    -> ( List Egg, List EggType )
stepEggs delta basket =
    let
        f :
            Egg
            -> ( List Egg, List EggType )
            -> ( List Egg, List EggType )
        f egg0 ( remaining, caught ) =
            case Egg.fall delta basket egg0 of
                Egg.Falling egg1 ->
                    ( egg1 :: remaining, caught )

                Egg.Caught typ ->
                    ( remaining, typ :: caught )

                Egg.Disappeared ->
                    ( remaining, caught )
    in
    List.foldr
        f
        ( [], [] )



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Collage msg
view model =
    Collage.group <|
        Basket.view model.basket
            :: List.map Egg.view model.eggs
