module BoardTest exposing (..)

import Test exposing (..)
import Expect
import Board
import Model exposing (Model, Cell, CellState(..))
import Init

all : Test
all =
    let
        (initialModel, _) = Init.init
    in
        describe "Board" [
            test "getInitialCells should return all empty cells" <|
                \() -> 
                    board 
                        Empty Empty Empty
                        Empty Empty Empty
                        Empty Empty Empty
                    |> Expect.equal Board.getInitialCells
            ,
            test "setCellState sets cell state at given coords" <|
                \() ->
                    modelWithBoard
                        Empty Empty Empty
                        Empty X_    Empty
                        Empty Empty Empty
                    |> Expect.equal (Board.setCellState X_ (1, 1) initialModel)
            ,
            test "getEmptyCells returns cell coords of empty cells" <|
                \() ->
                    let 
                        modelWithSomeEmptyCells = modelWithBoard 
                                                    Empty Empty Empty 
                                                    X_ X_ X_ 
                                                    O_ O_ Empty
                    in 
                        Expect.equal 
                            [(0, 0), (1, 0), (2, 0), (2, 2)] 
                            (Board.getEmptyCells modelWithSomeEmptyCells)
            ,
            test "getCellStates returns reversed list of cell states for a list of cell coords" <|
                \() ->
                    let model = modelWithBoard 
                                    Empty Empty Empty 
                                    X_    X_    X_ 
                                    O_    O_    Empty
                    in
                        Expect.equal 
                            [O_, X_, Empty]
                            (Board.getCellStates [(2, 0), (1, 1), (0, 2)] model)
        ]


modelWithBoard : 
        CellState -> CellState -> CellState -> 
        CellState -> CellState -> CellState -> 
        CellState -> CellState -> CellState 
        -> Model

modelWithBoard c00 c10 c20 c01 c11 c21 c02 c12 c22 = 
    let (model, _) = Init.init
    in { model | board = board c00 c10 c20 c01 c11 c21 c02 c12 c22 }

board : CellState -> CellState -> CellState -> 
        CellState -> CellState -> CellState -> 
        CellState -> CellState -> CellState -> List Cell
board c00 c10 c20 c01 c11 c21 c02 c12 c22 = 
    [
        {coords=(0, 0), state=c00}, {coords=(1, 0), state=c10}, {coords=(2,0), state=c20},
        {coords=(0, 1), state=c01}, {coords=(1, 1), state=c11}, {coords=(2,1), state=c21},
        {coords=(0, 2), state=c02}, {coords=(1, 2), state=c12}, {coords=(2,2), state=c22}
    ]
