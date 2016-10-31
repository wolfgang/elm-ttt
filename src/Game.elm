module Game exposing (makeMove)
import Model exposing (Model, Cell, CellState)
import Board
import Msg exposing (Msg)

makeMove :  CellState -> (Int, Int) -> Model -> (Model -> Cmd Msg) -> (Model, Cmd Msg)
makeMove cellState coords model nextCmdFn =
    let newModel = modelWithNewCellState cellState coords model
    in 
        if isGameOver newModel then
            (modelWithEmptyBoard newModel, Cmd.none)
        else
            (newModel, nextCmdFn newModel)


modelWithNewCellState : CellState -> (Int, Int) -> Model -> Model
modelWithNewCellState state coords model = 
    let newModel = Board.setCellState state coords model
    in 
        { newModel | highlightedCell = Nothing }

isGameOver : Model -> Bool
isGameOver model = List.length (Board.getEmptyCells model) == 0

modelWithEmptyBoard : Model -> Model
modelWithEmptyBoard model = { model | board = Board.getInitialCells }

