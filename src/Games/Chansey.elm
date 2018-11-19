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
    , eggtimer : Milliseconds
    , numeggs : Int -- Number of non-bomb eggs that have fallen
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


type alias Egg =
    { typ : EggType
    , column : Column
    , y : Y
    }


type EggType
    = EggTypeEgg
    | EggTypeBomb


{-| How many Y units an egg falls per millisecond.
-}
eggSpeed : Y_Per_Millisecond
eggSpeed =
    0.5


eggFall : Milliseconds -> Egg -> Egg
eggFall delta egg =
    { egg | y = egg.y - eggSpeed * delta }


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
    , eggtimer = 0
    , numeggs = 0
    , seed = Random.initialSeed 0
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
                (case egg.typ of
                    EggTypeEgg ->
                        Color.yellow

                    EggTypeBomb ->
                        Color.red
                )
            )
        |> Collage.shift ( colX egg.column, egg.y )


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
                        egg1 =
                            eggFall delta egg
                    in
                    if egg1.y <= -400 then
                        ( eggs, n )
                    else if egg.y > -300 && egg1.y <= -300 && egg.column == model.basket then
                        case egg.typ of
                            EggTypeEgg ->
                                ( eggs, n + 1 )

                            EggTypeBomb ->
                                ( eggs, -model.score )
                    else
                        ( egg1 :: eggs, n )
                )
                ( [], 0 )
                model.eggs

        timer1 =
            model.eggtimer - delta

        ( newEgg, seed1 ) =
            if timer1 <= 0 && model.numeggs <= 99 then
                let
                    ( newEgg_, seed_ ) =
                        randomEgg model.seed
                in
                ( Just newEgg_, seed_ )
            else
                ( Nothing, model.seed )

        ( timer2, seed2 ) =
            if timer1 <= 0 then
                Random.step (Random.float 300 500) seed1
            else
                ( timer1, seed1 )
    in
    { model
        | eggs =
            case newEgg of
                Nothing ->
                    eggs1

                Just egg ->
                    egg :: eggs1
        , eggtimer = timer2
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
