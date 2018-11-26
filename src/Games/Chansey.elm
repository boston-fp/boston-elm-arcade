module Games.Chansey exposing (..)

import Browser
import Browser.Events
import Games.Chansey.Basket as Basket exposing (Basket)
import Games.Chansey.Column exposing (Column(..))
import Games.Chansey.Egg as Egg exposing (Egg)
import Games.Chansey.EggTimer as EggTimer exposing (EggTimer, EggTimerStep(..))
import Games.Chansey.EggType as EggType exposing (EggType(..))
import Games.Chansey.Level as Level
import Games.Chansey.State exposing (State(..))
import Games.Chansey.Types exposing (..)
import Json.Decode
import Random
import RecurringTimer exposing (RecurringTimer)


type alias Model =
    { state : State
    , level : Level.Model
    , levels : List Level.Model
    , lastkey : String
    }


type Msg
    = Tick Milliseconds
    | Keydown String
    | Keyup String


init : Model
init =
    { state = ShowingLevelTitle
    , level =
        Level.init
            { levelnum = 1
            , numeggs = 10
            , spawntimer = { min = 500, max = 500 }
            , eggspeed = { min = 0.4, max = 0.4 }
            }
    , levels =
        [ Level.init
            { levelnum = 2
            , numeggs = 20
            , spawntimer = { min = 450, max = 450 }
            , eggspeed = { min = 0.45, max = 0.45 }
            }
        , Level.init
            { levelnum = 3
            , numeggs = 30
            , spawntimer = { min = 400, max = 400 }
            , eggspeed = { min = 0.5, max = 0.5 }
            }
        , Level.init
            { levelnum = 4
            , numeggs = 40
            , spawntimer = { min = 350, max = 350 }
            , eggspeed = { min = 0.55, max = 0.55 }
            }
        , Level.init
            { levelnum = 5
            , numeggs = 50
            , spawntimer = { min = 300, max = 300 }
            , eggspeed = { min = 0.6, max = 0.6 }
            }
        , Level.init
            { levelnum = 6
            , numeggs = 60
            , spawntimer = { min = 250, max = 250 }
            , eggspeed = { min = 0.65, max = 0.65 }
            }
        ]
    , lastkey = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.state of
        ShowingLevelTitle ->
            ( ifSpace msg model { model | state = PlayingLevel }
            , Cmd.none
            )

        PlayingLevel ->
            case msg of
                Tick delta ->
                    let
                        level1 =
                            Level.step (Level.Tick delta) model.level

                        model1 =
                            case Level.status level1 of
                                Level.Ongoing ->
                                    { model | level = level1 }

                                Level.Lost ->
                                    { model
                                        | state = ShowingLevelScore
                                    }

                                Level.Won ->
                                    case model.levels of
                                        [] ->
                                            { model | state = GameWinScreen }

                                        level :: levels ->
                                            { model
                                                | state = ShowingLevelTitle
                                                , level = level
                                                , levels = levels
                                            }
                    in
                    ( model1, Cmd.none )

                Keydown "p" ->
                    ( { model
                        | level = Level.step Level.RightPaddleDown model.level
                      }
                    , Cmd.none
                    )

                Keydown "q" ->
                    ( { model
                        | level = Level.step Level.LeftPaddleDown model.level
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
                        | level = Level.step Level.RightPaddleUp model.level
                      }
                    , Cmd.none
                    )

                Keyup "q" ->
                    ( { model | level = Level.step Level.LeftPaddleUp model.level }
                    , Cmd.none
                    )

                Keyup _ ->
                    ( model, Cmd.none )

        ShowingLevelScore ->
            ( ifSpace msg
                model
                { model
                    | state = ShowingLevelTitle
                    , level = Level.init model.level.config
                }
            , Cmd.none
            )

        GameWinScreen ->
            ( model, Cmd.none )


ifSpace : Msg -> Model -> Model -> Model
ifSpace msg model0 model1 =
    case msg of
        Tick _ ->
            model0

        Keydown " " ->
            model1

        Keydown _ ->
            model0

        Keyup _ ->
            model0


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keyDecoder : Json.Decode.Decoder String
        keyDecoder =
            Json.Decode.field "key" Json.Decode.string
    in
    Sub.batch
        [ case model.state of
            PlayingLevel ->
                Browser.Events.onAnimationFrameDelta Tick

            ShowingLevelTitle ->
                Sub.none

            ShowingLevelScore ->
                Sub.none

            GameWinScreen ->
                Sub.none
        , Browser.Events.onKeyDown (Json.Decode.map Keydown keyDecoder)
        , Browser.Events.onKeyUp (Json.Decode.map Keyup keyDecoder)
        ]
