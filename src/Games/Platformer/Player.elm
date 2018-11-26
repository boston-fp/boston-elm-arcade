module Games.Platformer.Player exposing (Player, init, jump, moveLeft, moveRight, stop, update)

import Games.Platformer.Controller as Controller exposing (Controller)
import Games.Platformer.Rigidbody as Rigidbody exposing (Rigidbody)
import Games.Platformer.Vector2D as Vector2D exposing (Vector2D)


type alias Player =
    { rigidbody : Rigidbody
    }


init : Player
init =
    let
        initRb =
            Rigidbody.init
    in
    { rigidbody = { initRb | size = Vector2D 24 24, position = Vector2D 0 20 } }


update : Controller -> Player -> Player
update controller player =
    { player | rigidbody = Rigidbody.update player.rigidbody }
        |> (if controller.left then
                moveLeft

            else
                identity
           )
        |> (if controller.right then
                moveRight

            else
                identity
           )
        |> (if controller.up then
                jump

            else
                identity
           )


jump : Player -> Player
jump player =
    if Rigidbody.isGrounded player.rigidbody then
        { player | rigidbody = Rigidbody.addForce (Vector2D 0 4) player.rigidbody }

    else
        player


moveLeft : Player -> Player
moveLeft player =
    { player | rigidbody = Rigidbody.addForce (Vector2D -1 0) player.rigidbody }


moveRight : Player -> Player
moveRight player =
    { player | rigidbody = Rigidbody.addForce (Vector2D 1 0) player.rigidbody }


stop : Player -> Player
stop player =
    let
        rigidbody =
            player.rigidbody
    in
    { player | rigidbody = { rigidbody | velocity = Vector2D.zero, acceleration = Vector2D.zero } }
