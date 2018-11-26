module Games.Chansey.Egg
    exposing
        ( Egg
        , FallResult(..)
        , fall
        , random
        , typ
        , view
        )

import Collage exposing (Collage)
import Color
import Games.Chansey.Basket as Basket exposing (Basket)
import Games.Chansey.Column as Column exposing (Column(..))
import Games.Chansey.Constants as Constants
import Games.Chansey.EggType exposing (EggType(..))
import Games.Chansey.Types exposing (..)
import Random


type Egg
    = Egg
        { typ_ : EggType
        , column : Column
        , y : Y

        -- How many Y units an egg falls per millisecond.
        , speed : Y_Per_Millisecond
        }


typ : Egg -> EggType
typ (Egg { typ_ }) =
    typ_


type FallResult
    = Falling Egg
    | Caught EggType
    | Disappeared


fall : Milliseconds -> Basket -> Egg -> FallResult
fall delta basket (Egg egg) =
    let
        y1 =
            egg.y - egg.speed * delta
    in
    if
        (Basket.column basket == egg.column)
            && (egg.y > Basket.y)
            && (y1 <= Basket.y)
    then
        Caught egg.typ_
    else if y1 < Constants.ymin then
        Disappeared
    else
        Falling (Egg { egg | y = y1 })


random :
    Random.Seed
    -> { min : Y_Per_Millisecond, max : Y_Per_Millisecond }
    -> ( Egg, Random.Seed )
random seed0 { min, max } =
    let
        ( col, seed1 ) =
            randomCol seed0

        ( speed, seed2 ) =
            Random.step (Random.float min max) seed1

        ( n, seed3 ) =
            Random.step (Random.int 0 9) seed2

        type_ =
            if n == 0 then
                EggTypeBomb
            else
                EggTypeEgg
    in
    ( Egg { typ_ = type_, column = col, y = 390, speed = speed }, seed3 )


randomCol : Random.Seed -> ( Column, Random.Seed )
randomCol seed =
    case Random.step (Random.int 0 2) seed of
        ( 0, seed1 ) ->
            ( Left, seed1 )

        ( 1, seed1 ) ->
            ( Center, seed1 )

        ( _, seed1 ) ->
            ( Right, seed1 )



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Egg -> Collage msg
view (Egg egg) =
    Collage.circle 10
        |> Collage.filled
            (Collage.uniform
                (case egg.typ_ of
                    EggTypeEgg ->
                        Color.yellow

                    EggTypeBomb ->
                        Color.red
                )
            )
        |> Collage.shift ( Column.x egg.column, egg.y )
