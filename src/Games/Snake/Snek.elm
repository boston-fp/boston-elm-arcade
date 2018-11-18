module Games.Snake.Snek exposing
    ( Direction(..)
    , Segment
    , Snek
    , changeDurr
    , init
    , isDed
    , map
    , move
    , toList
    )

import Games.Snake.Board as Board exposing (Point)
import Set


type alias Snek =
    { head : Segment
    , rest : List Segment
    }


type alias Segment =
    { location : Point
    , durrection : Direction
    }


type Direction
    = Left
    | Right
    | Up
    | Down


changeDurr : Snek -> Direction -> Snek
changeDurr snek durrection =
    let
        head =
            snek.head
    in
    { snek | head = { head | durrection = durrection } }


move : Snek -> Snek
move { head, rest } =
    let
        moveSegment : Segment -> Segment
        moveSegment segment =
            let
                ( x, y ) =
                    segment.location
            in
            case segment.durrection of
                Left ->
                    { segment | location = ( x - 1, y ) }

                Right ->
                    { segment | location = ( x + 1, y ) }

                Up ->
                    { segment | location = ( x, y + 1 ) }

                Down ->
                    { segment | location = ( x, y - 1 ) }

        setDurr : Direction -> Segment -> Segment
        setDurr dir seg =
            { seg | durrection = dir }
    in
    { head = moveSegment head
    , rest =
        List.map2
            (\parent seg -> moveSegment seg |> setDurr parent.durrection)
            (head :: rest)
            rest
    }


isDed : Snek -> Bool
isDed snek =
    let
        ( x, y ) =
            snek.head.location

        snekList =
            toList snek
    in
    List.any identity
        [ x >= (Board.width // 2)
        , x <= (-Board.width // 2)
        , y >= (Board.height // 2)
        , y <= (-Board.height // 2)
        , Set.size
            (Set.fromList << List.map .location <| snekList)
            /= List.length snekList
        ]


toList : Snek -> List Segment
toList { head, rest } =
    head :: rest


map : (Segment -> Segment) -> Snek -> Snek
map f snek =
    Snek (f snek.head) (List.map f snek.rest)


init : Snek
init =
    { head = Segment ( 0, 0 ) Left
    , rest =
        [ Segment ( 1, 0 ) Left
        , Segment ( 2, 0 ) Left
        , Segment ( 3, 0 ) Left
        , Segment ( 4, 0 ) Left
        ]
    }
