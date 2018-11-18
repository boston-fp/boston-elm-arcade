module Games.Snake.Model exposing
    ( Direction(..)
    , Model
    , Point
    , Segment
    , Snek
    , changeDurr
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
    { head : Segment
    , rest : List Segment
    }


type alias Segment =
    { location : Point
    , durrection : Direction
    }


type alias Model =
    { snek : Snek
    , foodPosition : Point
    , paused : Bool
    , timeSinceLastDraw : Float
    , fail : Bool
    }


init : Model
init =
    { snek =
        { head = Segment ( 0, 0 ) Left
        , rest =
            [ Segment ( 1, 0 ) Left
            , Segment ( 2, 0 ) Left
            , Segment ( 3, 0 ) Left
            , Segment ( 4, 0 ) Left
            ]
        }
    , foodPosition = ( 10, 0 )
    , paused = False
    , timeSinceLastDraw = 0
    , fail = False
    }


isDed : Snek -> Bool
isDed snek =
    let
        ( x, y ) =
            snek.head.location
    in
    List.any identity
        [ x >= (Board.width // 2)
        , x <= (-Board.width // 2)
        , y >= (Board.height // 2)
        , y <= (-Board.height // 2)
        ]


snek2List : Snek -> List Segment
snek2List { head, rest } =
    head :: rest


snekMap : (Segment -> Segment) -> Snek -> Snek
snekMap f snek =
    Snek (f snek.head) (List.map f snek.rest)


type Direction
    = Left
    | Right
    | Up
    | Down


changeDurr : Model -> Direction -> Model
changeDurr model durrection =
    let
        snek =
            model.snek

        head =
            snek.head
    in
    { model | snek = { snek | head = { head | durrection = durrection } } }
