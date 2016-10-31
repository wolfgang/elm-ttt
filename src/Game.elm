module Game exposing (makeMove)
import Model exposing (Model, Cell, CellState(..))
import Board
import Msg exposing (Msg)

type GameState =
    WIN_O
    |WIN_X
    |DRAW
    |IN_PROGRESS

makeMove :  CellState -> (Int, Int) -> Model -> (Model -> Cmd Msg) -> (Model, Cmd Msg)
makeMove cellState coords model nextCmdFn =
    let newModel = modelWithNewCellState cellState coords model
    in 
        if (getGameState newModel) /= IN_PROGRESS then
            (modelWithEmptyBoard newModel, Cmd.none)
        else
            (newModel, nextCmdFn newModel)

modelWithNewCellState : CellState -> (Int, Int) -> Model -> Model
modelWithNewCellState state coords model = 
    let newModel = Board.setCellState state coords model
    in 
        { newModel | highlightedCell = Nothing }

getGameState : Model -> GameState
getGameState model =
    if hasLineWith O_ model then
        WIN_O
    else if hasLineWith X_ model then
        WIN_X
    else if List.length (Board.getEmptyCells model) == 0 then
        DRAW
    else 
        IN_PROGRESS

hasLineWith : CellState -> Model -> Bool
hasLineWith cellState model = 
    let wantedStates = List.repeat 3 cellState
    in
        Board.getCellStates (0, 0) (1, 0) (2, 0) model == wantedStates ||
        Board.getCellStates (0, 1) (1, 1) (2, 1) model == wantedStates ||
        Board.getCellStates (0, 2) (1, 2) (2, 2) model == wantedStates ||
        Board.getCellStates (0, 0) (0, 1) (0, 2) model == wantedStates ||
        Board.getCellStates (1, 0) (1, 1) (1, 2) model == wantedStates ||
        Board.getCellStates (2, 0) (2, 1) (2, 2) model == wantedStates ||
        Board.getCellStates (0, 0) (1, 1) (2, 2) model == wantedStates

modelWithEmptyBoard : Model -> Model
modelWithEmptyBoard model = { model | board = Board.getInitialCells }
