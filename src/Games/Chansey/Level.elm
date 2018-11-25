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


type Model
    = Active ActiveModel
    | Inactive InactiveModel


type alias ActiveModel =
    { basket : Basket
    , eggs : List Egg
    , eggtimer : EggTimer
    , score : Int
    }


type alias InactiveModel =
    { score : Int }


type alias Config =
    { numeggs : Int
    }


type Control
    = Tick Milliseconds
    | LeftPaddleDown
    | LeftPaddleUp
    | RightPaddleDown
    | RightPaddleUp


init : Config -> Model
init config =
    Active
        { basket = Basket.new Column.Center
        , eggs = []
        , eggtimer =
            EggTimer.new
                (Random.initialSeed 1)
                (RecurringTimer.newWithJitter (Random.initialSeed 2) (Random.float 100 300))
                config.numeggs
        , score = 0
        }


update : Control -> Model -> Model
update control model =
    case model of
        Active model_ ->
            updateActive control model_

        Inactive model_ ->
            Inactive model_


updateActive : Control -> ActiveModel -> Model
updateActive control model =
    case control of
        Tick delta ->
            updateTick delta model

        LeftPaddleDown ->
            Active { model | basket = Basket.update Basket.LeftDown model.basket }

        LeftPaddleUp ->
            Active { model | basket = Basket.update Basket.LeftUp model.basket }

        RightPaddleDown ->
            Active { model | basket = Basket.update Basket.RightDown model.basket }

        RightPaddleUp ->
            Active { model | basket = Basket.update Basket.RightUp model.basket }


updateTick : Milliseconds -> ActiveModel -> Model
updateTick delta model =
    let
        ( eggs1, caught ) =
            stepEggs
                delta
                model.basket
                model.eggs

        score1 : Int
        score1 =
            model.score + List.sum (List.map EggType.score caught)
    in
    if EggTimer.done model.eggtimer then
        if List.isEmpty model.eggs then
            Inactive { score = score1 }
        else
            Active
                { model
                    | eggs = eggs1
                    , score = score1
                }
    else
        let
            ( eggtimer1, newEgg ) =
                case EggTimer.step delta model.eggtimer of
                    EggTimerStep eggtimer_ ->
                        ( eggtimer_, Nothing )

                    EggTimerFire eggtimer_ newEgg_ ->
                        ( eggtimer_, Just newEgg_ )

                    EggTimerDone ->
                        ( model.eggtimer, Nothing )
        in
        Active
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
        step :
            Egg
            -> ( List Egg, List EggType )
            -> ( List Egg, List EggType )
        step egg0 ( remaining, caught ) =
            case Egg.fall delta basket egg0 of
                Egg.Falling egg1 ->
                    ( egg1 :: remaining, caught )

                Egg.Caught typ ->
                    ( remaining, typ :: caught )

                Egg.Disappeared ->
                    ( remaining, caught )
    in
    List.foldr
        step
        ( [], [] )



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Collage msg
view model =
    case model of
        Active model_ ->
            Collage.group <|
                Basket.view model_.basket
                    :: List.map Egg.view model_.eggs

        Inactive { score } ->
            score
                |> String.fromInt
                |> Collage.Text.fromString
                |> Collage.Text.color Color.white
                |> Collage.Text.size Collage.Text.huge
                |> Collage.rendered
