module Tests exposing (all)

import Expect
import Games.Snake.Board as Board
import Games.Snake.Model as SnekModel
import Test exposing (..)


all : Test
all =
    describe "isDed"
        [ test "when in bounds" <|
            \_ ->
                let
                    testSnek =
                        [ ( 0, 0 ) ]
                in
                Expect.false "Snek should not be ded, but is ded" <| SnekModel.isDed testSnek
        , test "when out of bounds x" <|
            \_ ->
                let
                    testSnek =
                        [ ( Board.width / 2 + 100, 0 ) ]
                in
                Expect.true "Snek to be ded, but is alive" <| SnekModel.isDed (Debug.log "testSnek" testSnek)
        , test "when out of bounds y" <|
            \_ ->
                let
                    testSnek =
                        [ ( 0, Board.height / 2 + 100 ) ]
                in
                Expect.true "Snek to be ded, but is alive" <| SnekModel.isDed (Debug.log "testSnek" testSnek)
        , test "when out of bounds below" <|
            \_ ->
                let
                    testSnek =
                        [ ( 0, -Board.height / 2 - 100 ) ]
                in
                Expect.true "Snek to be ded, but is alive" <| SnekModel.isDed (Debug.log "testSnek" testSnek)
        ]
