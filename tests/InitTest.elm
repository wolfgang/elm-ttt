module InitTest exposing (..)

import Test exposing (..)
import Expect

import Init
import Model exposing (..)

(gInitialModel, gInitialCmd) = Init.init


all : Test
all =
    describe "Init.init should" [
            test "crete initial command Cmd.none" <|
                \() -> Expect.equal Cmd.none gInitialCmd
            ,
            test "create initial model.gameState of IN_PROGRESS" <|
                \() ->
                    Expect.equal IN_PROGRESS gInitialModel.gameState
    ]
