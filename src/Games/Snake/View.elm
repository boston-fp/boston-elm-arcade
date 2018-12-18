module Games.Snake.View exposing (view)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Games.Snake.Board as Board exposing (Point)
import Games.Snake.Model as Model exposing (Model)
import Games.Snake.Snek as Snek
import Html exposing (Html)
import Html.Attributes as Hattr
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL


snakeSegmentSize : Float
snakeSegmentSize =
    20


gamePointToViewPoint : Point -> ( Float, Float )
gamePointToViewPoint ( x, y ) =
    ( gameCoordToViewCoord x, gameCoordToViewCoord y )


gameCoordToViewCoord : Int -> Float
gameCoordToViewCoord val =
    toFloat val * snakeSegmentSize


view : Model -> Html msg
view model =
    WebGL.toHtml
        [ Hattr.style "background-color" "rgb(20,20,20)"
        , Hattr.style "width" "100%"
        , Hattr.style "height" "100vh"
        , Hattr.style "display" "flex"
        , Hattr.style "flex-direction" "column"
        , Hattr.style "align-items" "center"
        , Hattr.style "justify-content" "center"
        ]
        [ WebGL.entity vertexShader fragmentShader triangle {}
        ]


type alias Vertex =
    { position : Vec3, color : Vec3 }


triangle : WebGL.Mesh Vertex
triangle =
    let
        black =
            vec3 0 0 1
    in
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) black
          , Vertex (vec3 1 1 0) black
          , Vertex (vec3 1 -1 0) black
          )
        ]



-- Shaders


type alias Uniforms =
    {}


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        varying vec3 vcolor;
        
        void main() {
            gl_Position = vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;

        void main() {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
