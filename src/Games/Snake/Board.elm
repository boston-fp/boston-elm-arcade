module Games.Snake.Board exposing (Point, height, width)


height : Int
height =
    30


width : Int
width =
    30


{-| x is right (->) y is up (^)
-}
type alias Point =
    ( Int, Int )
