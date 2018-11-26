module Games.Platformer.Entity exposing (Entity(..))

import Games.Platformer.Rigidbody exposing (Rigidbody)
import Games.Platformer.Wall exposing (Wall)


type Entity
    = Rigidbody Rigidbody
    | Wall Wall


checkCollision : Entity a -> Entity b -> Bool
checkCollision e1 e2 =
    BoundingBox.isOverlapping (bounds e1) (bounds e2)
