port module Ports exposing (logToConsole)

import Json.Encode as E


port logToConsole : String -> Cmd msg


{-| save arbitary json to js
-}
port save : SaveData -> Cmd msg


type alias SaveData =
    { gameName : String, data : E.Value }
