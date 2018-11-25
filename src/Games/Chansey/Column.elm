module Games.Chansey.Column exposing (..)

import Games.Chansey.Types exposing (..)


type Column
    = Left
    | Center
    | Right



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


x : Column -> X
x col =
    case col of
        Left ->
            -100

        Center ->
            0

        Right ->
            100
