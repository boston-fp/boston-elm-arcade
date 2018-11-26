module Games.Platformer.BoundingBox exposing (BoundingBox, isOverlapping)

import Games.Platformer.Vector2D exposing (Vector2D)


type alias BoundingBox =
    { top : Float
    , right : Float
    , left : Float
    , bottom : Float
    }


isTouching : BoundingBox -> BoundingBox -> Bool
isTouching bb1 bb2 =
    let
        xTouch =
            bb1.right >= bb2.left && bb2.right >= bb1.left

        yTouch =
            bb1.bottom >= bb2.top && bb2.bottom >= bb1.top
    in
    xTouch && yTouch


isOverlapping : BoundingBox -> BoundingBox -> Bool
isOverlapping bb1 bb2 =
    let
        xOverlap =
            bb1.right > bb2.left && bb2.right > bb1.left

        yOverlap =
            bb1.bottom > bb2.top && bb2.bottom > bb1.top
    in
    xOverlap && yOverlap
