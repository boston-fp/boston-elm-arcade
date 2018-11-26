module Games.Platformer.Rigidbody exposing (Rigidbody, addForce, init, isGrounded, update)

import Games.Platformer.BoundingBox as BoundingBox exposing (BoundingBox)
import Games.Platformer.Vector2D as Vector2D exposing (Vector2D)


gravity : Vector2D
gravity =
    Vector2D 0 -2


type alias Rigidbody =
    { size : Vector2D
    , position : Vector2D
    , velocity : Vector2D
    , acceleration : Vector2D
    , maxVelocity : Vector2D
    }


init : Rigidbody
init =
    { size = Vector2D.zero
    , position = Vector2D.zero
    , velocity = Vector2D.zero
    , acceleration = Vector2D.zero
    , maxVelocity = Vector2D 8 40
    }


update : Rigidbody -> Rigidbody
update rb =
    let
        movedRigidbody =
            { rb
                | position = Vector2D.add rb.position rb.velocity
                , velocity =
                    rb.velocity
                        |> Vector2D.stepToZero (Vector2D 0.5 0)
                        |> Vector2D.add rb.acceleration
                        |> Vector2D.clamp rb.maxVelocity
                        |> (if isGrounded rb then
                                \vel ->
                                    if vel.y < 0 then
                                        Vector2D vel.x 0

                                    else
                                        vel

                            else
                                identity
                           )
                , acceleration = Vector2D.zero -- reset acceleration every update
            }
    in
    movedRigidbody
        |> addForce gravity


addForce : Vector2D -> Rigidbody -> Rigidbody
addForce force rb =
    { rb | acceleration = Vector2D.add rb.acceleration force }



-- should take other ground levels into account


isGrounded : Rigidbody -> Bool
isGrounded rb =
    rb.position.y <= 0


bounds : Rigidbody -> BoundingBox
bounds rb =
    BoundingBox
        (rb.position.y + (rb.size.y / 2))
        (rb.position.x + (rb.size.x / 2))
        (rb.position.x - (rb.size.x / 2))
        (rb.position.y - (rb.size.y / 2))


checkCollision : Rigidbody -> Rigidbody -> Bool
checkCollision rb1 rb2 =
    BoundingBox.isOverlapping (bounds rb1) (bounds rb2)
