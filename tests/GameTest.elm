module GameTest exposing (..)

import Test exposing (..)
import Expect

import TicTacToe exposing (init)

gInitialModel = TicTacToe.init


all : Test
all =
    describe "Game" [
            test "should assert" <|
                \() ->
                    Expect.equal 1 1
    ]
