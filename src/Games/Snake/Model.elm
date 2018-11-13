module Games.Snake.Model exposing (Model, Point, init)

{-| x is right (->) y is up (^)
-}


type alias Point =
    ( Float, Float )


type alias Model =
    { body : List Point
    , foodPosition : Point
    }


init : Model
init =
    { body = [ ( 0, 0 ), ( 0, -1 ), ( 0, -2 ) ]
    , foodPosition = ( 10, 1 )
    }
