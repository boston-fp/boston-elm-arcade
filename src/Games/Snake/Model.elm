module Games.Snake.Model exposing
    ( Direction(..)
    , Model
    , Point
    , Snek
    , init
    , isDed
    , snek2List
    , snekMap
    )

import Games.Snake.Board as Board


{-| x is right (->) y is up (^)
-}
type alias Point =
    ( Int, Int )


type alias Snek =
    { head : Point
    , rest : List Point
    }


type alias Model =
    { snek : Snek
    , foodPosition : Point
    , paused : Bool
    , timeSinceLastDraw : Float
    , fail : Bool
    , durr : Direction
    }


init : Model
init =
    { snek = Snek ( 0, 0 ) []
    , foodPosition = ( 10, 0 )
    , paused = False
    , timeSinceLastDraw = 0
    , fail = False
    , durr = Up
    }


isDed : Snek -> Bool
isDed snek =
    let
        ( x, y ) =
            snek.head
    in
    List.any identity
        [ x >= (Board.width // 2)
        , x <= (-Board.width // 2)
        , y >= (Board.height // 2)
        , y <= (-Board.height // 2)
        ]


snek2List : Snek -> List Point
snek2List { head, rest } =
    head :: rest


snekMap : (Point -> Point) -> Snek -> Snek
snekMap f snek =
    Snek (f snek.head) (List.map f snek.rest)


type Direction
    = Left
    | Right
    | Up
    | Down
