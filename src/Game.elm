module Game exposing (makeMove)
import Model exposing (Model, Cell, GameState(..), CellState(..))
import Board
import Msg exposing (Msg)


makeMove :  CellState -> (Int, Int) -> Model -> (Model -> Cmd Msg) -> (Model, Cmd Msg)
makeMove cellState coords model nextCmdFn =
    let 
        modelWithMove = modelWithNewCellState cellState coords model
        newModel = { modelWithMove | gameState = getGameState modelWithMove }
    in 
        if newModel.gameState /= IN_PROGRESS then
            (newModel, Cmd.none)
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
    else if hasNoEmptyCellsLeft model then
        DRAW
    else 
        IN_PROGRESS

hasLineWith : CellState -> Model -> Bool
hasLineWith cellState model = 
    let wantedStates = List.repeat 3 cellState
    in
        hasRowWith wantedStates 0 model ||
        hasRowWith wantedStates 1 model ||
        hasRowWith wantedStates 2 model ||
        hasColumnWith wantedStates 0 model ||
        hasColumnWith wantedStates 1 model ||
        hasColumnWith wantedStates 2 model ||
        hasLeftDiagonalWith wantedStates model ||
        hasRightDiagonalWith wantedStates model

hasRowWith : List CellState -> Int -> Model -> Bool
hasRowWith cellStates rowIndex model = 
    Board.getCellStates (0, rowIndex) (1, rowIndex) (2, rowIndex) model ==  cellStates

hasColumnWith : List CellState -> Int -> Model -> Bool
hasColumnWith cellStates columIndex model = 
    Board.getCellStates (columIndex, 0) (columIndex, 1) (columIndex, 2) model ==  cellStates

hasLeftDiagonalWith : List CellState -> Model -> Bool
hasLeftDiagonalWith cellStates model =
    Board.getCellStates (0, 0) (1, 1) (2, 2) model == cellStates

hasRightDiagonalWith : List CellState -> Model -> Bool
hasRightDiagonalWith cellStates model =
    Board.getCellStates (2, 0) (1, 1) (0, 2) model == cellStates

hasNoEmptyCellsLeft : Model -> Bool
hasNoEmptyCellsLeft model =
    List.length (Board.getEmptyCells model) == 0

modelWithEmptyBoard : Model -> Model
modelWithEmptyBoard model = { model | board = Board.getInitialCells }
