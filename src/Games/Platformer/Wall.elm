module Games.Platformer.Wall exposing (Wall)

import Games.Platformer.BoundingBox exposing (BoundingBox)
import Games.Platformer.Vector2D exposing (Vector2D)


type alias Wall =
    { position : Vector2D
    , size : Vector2D
    }


bounds : Wall -> BoundingBox
bounds wall =
    BoundingBox
        (wall.position.y + (wall.size.y / 2))
        (wall.position.x + (wall.size.x / 2))
        (wall.position.x - (wall.size.x / 2))
        (wall.position.y - (wall.size.y / 2))
