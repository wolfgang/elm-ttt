module BoardTest exposing (..)

import Test exposing (..)
import Expect
import Board
import Model exposing (Cell, CellState(..))
import Init

all : Test
all =
    describe "Board" [
        test "getInitialCells should return all empty cells" <|
            \() -> 
                Expect.equal (board 
                        Empty Empty Empty
                        Empty Empty Empty
                        Empty Empty Empty)
                Board.getInitialCells
        ,
        test "setCellState sets cell state at given coords" <|
            \() ->
                let 
                    (model, _) = Init.init
                    modelWithXAt11 = Board.setCellState X_ (1, 1) model
                in
                    Expect.equal (board 
                        Empty Empty Empty
                        Empty X_    Empty
                        Empty Empty Empty
                    )
                    modelWithXAt11.board
    ]

board : CellState -> CellState -> CellState -> 
        CellState -> CellState -> CellState -> 
        CellState -> CellState -> CellState -> List Cell
board c00 c10 c20 c01 c11 c21 c02 c12 c22 = 
    [
        {coords=(0, 0), state=c00}, {coords=(1, 0), state=c10}, {coords=(2,0), state=c20},
        {coords=(0, 1), state=c01}, {coords=(1, 1), state=c11}, {coords=(2,1), state=c21},
        {coords=(0, 2), state=c02}, {coords=(1, 2), state=c12}, {coords=(2,2), state=c22}
    ]
