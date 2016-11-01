module Game exposing (makeMove)
import Model exposing (Model, Cell, GameState(..), CellState(..))
import Board
import Msg exposing (Msg)


makeMove :  CellState -> (Int, Int) -> Model -> (Model -> Cmd Msg) -> (Model, Cmd Msg)
makeMove cellState coords model nextCmdFn =
    let 
        modelWithMove = modelWithNewCellState cellState coords model
        newModel = { modelWithMove | gameState = getGameState cellState modelWithMove }
    in 
        if newModel.gameState /= (IN_PROGRESS, []) then
            (newModel, Cmd.none)
        else
            (newModel, nextCmdFn newModel)

modelWithNewCellState : CellState -> (Int, Int) -> Model -> Model
modelWithNewCellState state coords model = 
    let newModel = Board.setCellState state coords model
    in 
        { newModel | highlightedCell = Nothing }

getGameState : CellState -> Model -> (GameState, List (Int, Int))
getGameState cellState model =
    case getWinningLine cellState model of
        Just (cellState, line) -> (if cellState==X_ then WIN_X else WIN_O, line)
        _ ->
            if hasNoEmptyCellsLeft model then
                (DRAW, [])
            else
                (IN_PROGRESS, [])

getWinningLine : CellState -> Model -> Maybe (CellState, List (Int, Int))
getWinningLine cellState model =
    let wantedStates = List.repeat 3 cellState
    in
        if getCellStatesForRow 0 model == wantedStates then
            Just (cellState,  getRowCoords 0)
        else if getCellStatesForRow 1 model == wantedStates then
            Just (cellState, getRowCoords 1)
        else if getCellStatesForRow 2 model == wantedStates then
            Just (cellState, getRowCoords 2)
        else if getCellStatesForColumn 0 model == wantedStates then
            Just (cellState, getColumCoords 0)
        else if getCellStatesForColumn 1 model == wantedStates then
            Just (cellState, getColumCoords 1)
        else if getCellStatesForColumn 2 model == wantedStates then
            Just (cellState, getColumCoords 2)
        else if getLeftToRightDiagonal model == wantedStates then
            Just (cellState, getLeftToRightDiagonalCoords)
        else if getRightToLeftDiagonal model == wantedStates then
            Just (cellState, getRightToLeftDiagonalCoords)
        else Nothing
            

hasRowWith : List CellState -> Int -> Model -> Bool
hasRowWith cellStates rowIndex model = 
    (getCellStatesForRow rowIndex model) ==  cellStates

getCellStatesForRow : Int -> Model -> List CellState
getCellStatesForRow rowIndex model = 
    Board.getCellStates (getRowCoords rowIndex) model

hasColumnWith : List CellState -> Int -> Model -> Bool
hasColumnWith cellStates columIndex model = 
    (getCellStatesForColumn columIndex model) ==  cellStates

getCellStatesForColumn : Int -> Model -> List CellState
getCellStatesForColumn columIndex model =
    Board.getCellStates (getColumCoords columIndex) model    

hasLeftDiagonalWith : List CellState -> Model -> Bool
hasLeftDiagonalWith cellStates model =
    getLeftToRightDiagonal model == cellStates

hasRightDiagonalWith : List CellState -> Model -> Bool
hasRightDiagonalWith cellStates model =
    getRightToLeftDiagonal model == cellStates

getLeftToRightDiagonal : Model -> List CellState
getLeftToRightDiagonal model = 
    Board.getCellStates getLeftToRightDiagonalCoords model

getRightToLeftDiagonal : Model -> List CellState
getRightToLeftDiagonal model = 
    Board.getCellStates getRightToLeftDiagonalCoords model

getRowCoords : Int -> List (Int, Int)
getRowCoords rowIndex = [(0, rowIndex), (1, rowIndex), (2, rowIndex)]

getColumCoords : Int -> List (Int, Int)
getColumCoords columIndex = [(columIndex, 0), (columIndex, 1), (columIndex, 2)]

getLeftToRightDiagonalCoords : List (Int, Int)
getLeftToRightDiagonalCoords = [(0, 0), (1, 1), (2, 2)]

getRightToLeftDiagonalCoords : List (Int, Int)
getRightToLeftDiagonalCoords = [(2, 0), (1, 1), (0, 2)]


hasNoEmptyCellsLeft : Model -> Bool
hasNoEmptyCellsLeft model =
    List.length (Board.getEmptyCells model) == 0

modelWithEmptyBoard : Model -> Model
modelWithEmptyBoard model = { model | board = Board.getInitialCells }
