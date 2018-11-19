module Games.Chansey exposing (..)

import Browser
import Browser.Events
import Collage
import Collage.Render
import Color
import Html exposing (Html)
import Json.Decode
import Random


type alias Model =
    { basket : Column
    , paddle : Maybe Paddle
    , eggs : List Egg
    , eggsupply : List Egg
    , eggtimer : Milliseconds
    , seed : Random.Seed
    , score : Int
    , paused : Bool
    , lastkey : String
    }


type alias Milliseconds =
    Float


type alias Y_Per_Millisecond =
    Float


type alias X =
    Float


type alias Y =
    Float


type Egg
    = Egg Column Y
    | Bomb Column Y


eggSpeed : Y_Per_Millisecond
eggSpeed =
    0.5


eggCol : Egg -> Column
eggCol egg =
    case egg of
        Egg col _ ->
            col

        Bomb col _ ->
            col


eggY : Egg -> Y
eggY egg =
    case egg of
        Egg _ y ->
            y

        Bomb _ y ->
            y


eggFall : Milliseconds -> Egg -> Egg
eggFall delta egg =
    case egg of
        Egg col y ->
            Egg col (y - eggSpeed * delta)

        Bomb col y ->
            Bomb col (y - eggSpeed * delta)


type Column
    = Left
    | Center
    | Right


type Paddle
    = PaddleQ
    | PaddleP
    | PaddlePQ


colX : Column -> X
colX col =
    case col of
        Left ->
            -100

        Center ->
            0

        Right ->
            100


replicateRandom :
    Int
    -> (Random.Seed -> ( a, Random.Seed ))
    -> Random.Seed
    -> ( List a, Random.Seed )
replicateRandom n f seed0 =
    case n of
        0 ->
            ( [], seed0 )

        _ ->
            let
                ( x, seed1 ) =
                    f seed0

                ( xs, seed2 ) =
                    replicateRandom (n - 1) f seed1
            in
            ( x :: xs, seed2 )


randomEgg : Random.Seed -> ( Egg, Random.Seed )
randomEgg seed0 =
    let
        ( col, seed1 ) =
            randomCol seed0

        ( n, seed2 ) =
            Random.step (Random.int 0 9) seed1
    in
    if n == 0 then
        ( Bomb col 400, seed2 )
    else
        ( Egg col 400, seed2 )


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
    let
        ( eggs, seed ) =
            replicateRandom 100 randomEgg (Random.initialSeed 0)
    in
    { basket = Center
    , paddle = Nothing
    , eggs = []
    , eggsupply = eggs
    , eggtimer = 0
    , seed = seed
    , score = 0
    , paused = False
    , lastkey = ""
    }


view : Model -> Html Msg
view model =
    Html.div
        []
        [ (Collage.Render.svg << Collage.group)
            (viewBasket model.basket :: List.map viewEgg model.eggs ++ [ viewBackground ])
        , Html.text (String.fromInt model.score)
        , Html.text model.lastkey
        ]


viewBasket : Column -> Collage.Collage msg
viewBasket basket =
    Collage.circle 15
        |> Collage.filled (Collage.uniform Color.blue)
        |> Collage.shift ( colX basket, -300 )


viewEgg : Egg -> Collage.Collage msg
viewEgg egg =
    Collage.circle 10
        |> Collage.filled
            (Collage.uniform
                (case egg of
                    Egg _ _ ->
                        Color.yellow

                    Bomb _ _ ->
                        Color.red
                )
            )
        |> Collage.shift ( colX (eggCol egg), eggY egg )


viewBackground : Collage.Collage msg
viewBackground =
    Collage.rectangle 400 800
        |> Collage.filled (Collage.uniform Color.black)


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
                        , paddle = Just PaddleQ
                      }
                    , Cmd.none
                    )

                Just PaddleQ ->
                    ( model, Cmd.none )

                Just PaddleP ->
                    ( { model | paddle = Just PaddlePQ }, Cmd.none )

                Just PaddlePQ ->
                    ( model, Cmd.none )

        Keydown "p" ->
            case model.paddle of
                Nothing ->
                    ( { model
                        | basket = Right
                        , paddle = Just PaddleP
                      }
                    , Cmd.none
                    )

                Just PaddleQ ->
                    ( { model | paddle = Just PaddlePQ }, Cmd.none )

                Just PaddleP ->
                    ( model, Cmd.none )

                Just PaddlePQ ->
                    ( model, Cmd.none )

        Keyup "q" ->
            case model.paddle of
                Nothing ->
                    ( model, Cmd.none )

                Just PaddleQ ->
                    ( { model
                        | paddle = Nothing
                        , basket = Center
                      }
                    , Cmd.none
                    )

                Just PaddleP ->
                    ( model, Cmd.none )

                Just PaddlePQ ->
                    ( { model
                        | paddle = Just PaddleP
                        , basket = Right
                      }
                    , Cmd.none
                    )

        Keyup "p" ->
            case model.paddle of
                Nothing ->
                    ( model, Cmd.none )

                Just PaddleQ ->
                    ( model, Cmd.none )

                Just PaddleP ->
                    ( { model
                        | paddle = Nothing
                        , basket = Center
                      }
                    , Cmd.none
                    )

                Just PaddlePQ ->
                    ( { model
                        | paddle = Just PaddleQ
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


updateTick : Milliseconds -> Model -> Model
updateTick delta model =
    let
        ( eggs1, score ) =
            List.foldr
                (\egg ( eggs, n ) ->
                    let
                        y =
                            eggY egg

                        egg1 =
                            eggFall delta egg

                        y1 =
                            eggY egg1

                        col =
                            eggCol egg
                    in
                    if y1 <= -400 then
                        ( eggs, n )
                    else if y > -300 && y1 <= -300 && col == model.basket then
                        case egg1 of
                            Egg _ _ ->
                                ( eggs, n + 1 )

                            Bomb _ _ ->
                                ( eggs, -model.score )
                    else
                        ( egg1 :: eggs, n )
                )
                ( [], 0 )
                model.eggs

        timer1 =
            model.eggtimer - delta

        ( eggs2, seed1 ) =
            if timer1 <= 0 then
                let
                    ( col, seed_ ) =
                        randomCol model.seed

                    ( n, seed3 ) =
                        Random.step (Random.int 0 9) seed_
                in
                if n == 0 then
                    ( Bomb col 390 :: eggs1, seed3 )
                else
                    ( Egg col 390 :: eggs1, seed3 )
            else
                ( eggs1, model.seed )

        ( timer2, seed2 ) =
            if timer1 <= 0 then
                Random.step (Random.float 300 500) seed1
            else
                ( timer1, seed1 )
    in
    { model
        | eggs = eggs2
        , eggtimer = timer2
        , seed = seed2
        , score = model.score + score
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
