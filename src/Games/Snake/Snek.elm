module Games.Snake.Snek exposing
    ( Direction(..)
    , Segment
    , Snek
    , canHazBabby
    , changeDurr
    , durrection
    , enhance
    , init
    , isDed
    , map
    , move
    , oppositeDurr
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


durrection : Snek -> Direction
durrection snek =
    snek.head.durrection


oppositeDurr : Direction -> Direction
oppositeDurr durr =
    case durr of
        Left ->
            Right

        Right ->
            Left

        Up ->
            Down

        Down ->
            Up


changeDurr : Snek -> Direction -> Snek
changeDurr snek durr =
    let
        head =
            snek.head
    in
    { snek | head = { head | durrection = durr } }


canHazBabby : Point -> Snek -> Bool
canHazBabby babbyLoc { head } =
    head.location == babbyLoc


enhance : Snek -> Snek
enhance snek =
    let
        lastSeg : Segment
        lastSeg =
            snek
                |> toList
                |> List.reverse
                |> List.head
                -- This should never happen, TODO: add error handling
                |> Maybe.withDefault snek.head
    in
    { snek
        | rest =
            snek.rest
                ++ [ Segment
                        (movePoint
                            (oppositeDurr lastSeg.durrection)
                            lastSeg.location
                        )
                        lastSeg.durrection
                   ]
    }


movePoint : Direction -> Point -> Point
movePoint durr ( x, y ) =
    case durr of
        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )

        Up ->
            ( x, y + 1 )

        Down ->
            ( x, y - 1 )


move : Snek -> Snek
move { head, rest } =
    let
        moveSegment : Segment -> Segment
        moveSegment segment =
            { segment
                | location = movePoint segment.durrection segment.location
            }

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
