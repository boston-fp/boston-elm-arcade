module Games.Snake.Board exposing (Point, height, width)


height : Int
height =
    50


width : Int
width =
    70


{-| x is right (->) y is up (^)
-}
type alias Point =
    ( Int, Int )
