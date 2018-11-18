module Tests exposing (all)

import Expect
import Games.Snake.Board as Board
import Games.Snake.Model as SnekModel
import Games.Snake.Snek as Snek exposing (Direction(..), Segment)
import Games.Snake.Update as SnekUpdate
import Test exposing (..)


all : Test
all =
    describe "moveSnek"
        [ test "simple" <|
            \_ ->
                let
                    testSnek =
                        Snek.init
                in
                Expect.equal (Snek.move testSnek)
                    { head = Segment ( -1, 0 ) Left
                    , rest =
                        [ Segment ( 0, 0 ) Left
                        , Segment ( 1, 0 ) Left
                        , Segment ( 2, 0 ) Left
                        , Segment ( 3, 0 ) Left
                        ]
                    }
        , test "move up" <|
            \_ ->
                let
                    testSnek : Snek.Snek
                    testSnek =
                        { head = Segment ( 0, 0 ) Up
                        , rest =
                            [ Segment ( 1, 0 ) Left
                            , Segment ( 2, 0 ) Left
                            , Segment ( 3, 0 ) Left
                            , Segment ( 4, 0 ) Left
                            ]
                        }
                in
                Expect.equal (Snek.move testSnek)
                    { head = Segment ( 0, 1 ) Up
                    , rest =
                        [ Segment ( 0, 0 ) Up
                        , Segment ( 1, 0 ) Left
                        , Segment ( 2, 0 ) Left
                        , Segment ( 3, 0 ) Left
                        ]
                    }
        ]



{- }
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
-}
