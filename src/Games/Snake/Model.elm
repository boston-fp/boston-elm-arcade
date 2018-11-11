module Games.Snake.Model exposing (Model, init)


type alias Point =
    ( Float, Float )


type alias Model =
    { body : List Point
    , foodPosition : Point
    }


init : Model
init =
    { body = [ ( 0, 0 ) ]
    , foodPosition = ( 10, 1 )
    }
