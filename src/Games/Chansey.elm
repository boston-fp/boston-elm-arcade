module Games.Chansey exposing (..)

import Browser
import Browser.Events
import Json.Decode
import Random
import RecurringTimer exposing (RecurringTimer)


type alias Model =
    { basket : Column
    , paddle : Maybe Paddle
    , eggs : List Egg
    , eggtimer : EggTimer
    , numeggs : Int -- Number of non-bomb eggs that have fallen
    , score : Int
    , paused : Bool
    , lastkey : String
    , done : Bool
    }


type alias Milliseconds =
    Float


type alias Y_Per_Millisecond =
    Float


type alias X =
    Float


type alias Y =
    Float


type alias Egg =
    { typ : EggType
    , column : Column
    , y : Y
    }


type EggTimer
    = EggTimer Random.Seed RecurringTimer


type EggTimerStep
    = EggTimerStep EggTimer
    | EggTimerFire EggTimer Egg


newEggTimer : Random.Seed -> RecurringTimer -> EggTimer
newEggTimer =
    EggTimer


stepEggTimer : Milliseconds -> EggTimer -> EggTimerStep
stepEggTimer delta (EggTimer seed0 timer0) =
    case RecurringTimer.step delta timer0 of
        RecurringTimer.Step timer1 ->
            EggTimerStep (EggTimer seed0 timer1)

        RecurringTimer.Fire timer1 ->
            let
                ( egg, seed1 ) =
                    randomEgg seed0
            in
            EggTimerFire (EggTimer seed1 timer1) egg


{-| How many Y units an egg falls per millisecond.
-}
eggSpeed : Y_Per_Millisecond
eggSpeed =
    0.8


eggFall : Milliseconds -> Egg -> Egg
eggFall delta egg =
    { egg | y = egg.y - eggSpeed * delta }


type EggType
    = EggTypeEgg
    | EggTypeBomb


eggTypeScore : EggType -> Int
eggTypeScore typ =
    case typ of
        EggTypeEgg ->
            1

        EggTypeBomb ->
            -5


type Column
    = Left
    | Center
    | Right


colX : Column -> X
colX col =
    case col of
        Left ->
            -100

        Center ->
            0

        Right ->
            100


type Paddle
    = PaddleL
    | PaddleR
    | PaddleLR


randomEgg : Random.Seed -> ( Egg, Random.Seed )
randomEgg seed0 =
    let
        ( col, seed1 ) =
            randomCol seed0

        ( n, seed2 ) =
            Random.step (Random.int 0 9) seed1

        type_ =
            if n == 0 then
                EggTypeBomb
            else
                EggTypeEgg
    in
    ( Egg type_ col 390, seed2 )


randomCol : Random.Seed -> ( Column, Random.Seed )
randomCol seed =
    case Random.step (Random.int 0 2) seed of
        ( 0, seed1 ) ->
            ( Left, seed1 )

        ( 1, seed1 ) ->
            ( Center, seed1 )

        ( _, seed1 ) ->
            ( Right, seed1 )


type Msg
    = Tick Milliseconds
    | Keydown String
    | Keyup String


init : Model
init =
    { basket = Center
    , paddle = Nothing
    , eggs = []
    , eggtimer =
        newEggTimer
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

        Keydown "q" ->
            case model.paddle of
                Nothing ->
                    ( { model
                        | basket = Left
                        , paddle = Just PaddleL
                      }
                    , Cmd.none
                    )

                Just PaddleL ->
                    ( model, Cmd.none )

                Just PaddleR ->
                    ( { model | paddle = Just PaddleLR }, Cmd.none )

                Just PaddleLR ->
                    ( model, Cmd.none )

        Keydown "p" ->
            case model.paddle of
                Nothing ->
                    ( { model
                        | basket = Right
                        , paddle = Just PaddleR
                      }
                    , Cmd.none
                    )

                Just PaddleL ->
                    ( { model | paddle = Just PaddleLR }, Cmd.none )

                Just PaddleR ->
                    ( model, Cmd.none )

                Just PaddleLR ->
                    ( model, Cmd.none )

        Keyup "q" ->
            case model.paddle of
                Nothing ->
                    ( model, Cmd.none )

                Just PaddleL ->
                    ( { model
                        | paddle = Nothing
                        , basket = Center
                      }
                    , Cmd.none
                    )

                Just PaddleR ->
                    ( model, Cmd.none )

                Just PaddleLR ->
                    ( { model
                        | paddle = Just PaddleR
                        , basket = Right
                      }
                    , Cmd.none
                    )

        Keyup "p" ->
            case model.paddle of
                Nothing ->
                    ( model, Cmd.none )

                Just PaddleL ->
                    ( model, Cmd.none )

                Just PaddleR ->
                    ( { model
                        | paddle = Nothing
                        , basket = Center
                      }
                    , Cmd.none
                    )

                Just PaddleLR ->
                    ( { model
                        | paddle = Just PaddleL
                        , basket = Left
                      }
                    , Cmd.none
                    )

        Keydown " " ->
            ( { model | paused = not model.paused }, Cmd.none )

        Keydown key ->
            ( { model | lastkey = key }, Cmd.none )

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
                    eggFall delta egg
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
                { delta = delta, basket = model.basket }
                model.eggs

        ( eggtimer1, newEgg ) =
            case stepEggTimer delta model.eggtimer of
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
        , score = model.score + List.sum (List.map eggTypeScore caught)
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
