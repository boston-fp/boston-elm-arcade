module Games.Platformer.Vector2D exposing (Vector2D, add, asTuple, clamp, stepToZero, zero)


type alias Vector2D =
    { x : Float
    , y : Float
    }


map : (Float -> Float) -> Vector2D -> Vector2D
map fn vec =
    Vector2D (fn vec.x) (fn vec.y)


asTuple : Vector2D -> ( Float, Float )
asTuple vec =
    ( vec.x, vec.y )


zero : Vector2D
zero =
    Vector2D 0 0


add : Vector2D -> Vector2D -> Vector2D
add v1 v2 =
    Vector2D (v1.x + v2.x) (v1.y + v2.y)


clamp : Vector2D -> Vector2D -> Vector2D
clamp clampVec vec =
    let
        clampedX =
            Basics.clamp -clampVec.x clampVec.x vec.x

        clampedY =
            Basics.clamp -clampVec.y clampVec.y vec.y
    in
    Vector2D clampedX clampedY



-- interpolate?


stepToZero : Vector2D -> Vector2D -> Vector2D
stepToZero step vec =
    let
        absStep =
            map abs step

        isNearbyX =
            abs vec.x < absStep.x

        isNearbyY =
            abs vec.y < absStep.y

        xComponent =
            if isNearbyX then
                0

            else
                case compare vec.x 0 of
                    GT ->
                        vec.x - step.x

                    LT ->
                        vec.x + step.x

                    EQ ->
                        0

        yComponent =
            if isNearbyY then
                0

            else
                case compare vec.y 0 of
                    GT ->
                        vec.y - step.y

                    LT ->
                        vec.y + step.y

                    EQ ->
                        0
    in
    Vector2D xComponent yComponent
