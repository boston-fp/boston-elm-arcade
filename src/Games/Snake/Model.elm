module Games.Snake.Model exposing (Model, Point, init, isDed)

import Games.Snake.Board as Board


{-| x is right (->) y is up (^)
-}
type alias Point =
    ( Float, Float )


type alias Snake =
    List Point


type alias Model =
    { body : Snake
    , foodPosition : Point
    , paused : Bool
    }


init : Model
init =
    { body = [ ( 0, -20 ), ( 0, -10 ), ( 0, 0 ) ]
    , foodPosition = ( 10, 1 )
    , paused = False
    }


isDed : Snake -> Bool
isDed snek =
    let
        ( x, y ) =
            snek |> List.reverse |> List.head |> Maybe.withDefault ( 0, 0 )
    in
    x > (Board.width / 2) || x < (-Board.width / 2) || y > (Board.height / 2) || y < (-Board.height / 2)
