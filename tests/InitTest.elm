module InitTest exposing (..)

import Test exposing (..)
import Expect
import Init
import Model exposing (..)
import Board
import WinningAnimation


( gInitialModel, gInitialCmd ) =
    Init.init


all : Test
all =
    describe "Init.init should"
        [ test "create initial command Cmd.none" <|
            \() -> Expect.equal Cmd.none gInitialCmd
        , test "set initial model.gameState to IN_PROGRESS" <|
            \() -> Expect.equal IN_PROGRESS gInitialModel.gameState
        , test "set initial model.highlightedCell to Nothing " <|
            \() -> Expect.equal Nothing gInitialModel.highlightedCell
        , test "set initial model.board to Board.getInitialCells" <|
            \() -> Expect.equal Board.getInitialCells gInitialModel.board
        , test "set initial model.winningAnimation to winningAnimation.init" <|
            \() -> Expect.equal WinningAnimation.init gInitialModel.winningAnimation
        ]
