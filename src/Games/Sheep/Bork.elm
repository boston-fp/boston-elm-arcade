module Games.Sheep.Bork exposing (..)

import Dict.Any exposing (AnyDict)
import Games.Sheep.Controller exposing (Controller)
import Games.Sheep.Doggo as Doggo exposing (Doggo)
import P2 exposing (P2)
import V2 exposing (V2)


type alias Borks =
    AnyDict ( Float, Float ) P2 Bork


type alias Bork =
    { dir : V2
    , framesLeft : Float
    }


newBork : Doggo.Config r -> Controller -> Doggo -> Borks -> Borks
newBork config controller doggo =
    Dict.Any.update doggo.pos
        (\existingBork ->
            case existingBork of
                Just bork ->
                    Just bork

                Nothing ->
                    Just
                        { dir = V2.signorm (Doggo.doggoVel config controller doggo)
                        , framesLeft = 10
                        }
        )


stepBorks : Float -> Borks -> Borks
stepBorks frames borks =
    borks
        |> Dict.Any.map (\_ bork -> { bork | framesLeft = bork.framesLeft - 1 })
        |> Dict.Any.filter (\_ bork -> bork.framesLeft > 0)
