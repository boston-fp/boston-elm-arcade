module Games.Chansey.Egg exposing (..)

import Games.Chansey.Column exposing (Column(..))
import Games.Chansey.EggType exposing (EggType(..))
import Games.Chansey.Types exposing (..)
import Random


type alias Egg =
    { typ : EggType
    , column : Column
    , y : Y
    }


{-| How many Y units an egg falls per millisecond.
-}
speed : Y_Per_Millisecond
speed =
    0.8


fall : Milliseconds -> Egg -> Egg
fall delta egg =
    { egg | y = egg.y - speed * delta }


random : Random.Seed -> ( Egg, Random.Seed )
random seed0 =
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
