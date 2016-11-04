module Game exposing (makeMove)
import Model exposing (..)
import Board
import Msg exposing (Msg)
import ListExt
import BoardUI


makeMove :  CellState -> (Int, Int) -> Model -> (Model -> Cmd Msg) -> (Model, Cmd Msg)
makeMove cellState coords model nextCmdFn =
    let 
        gameState = getGameState cellState modelWithMove
        modelWithMove = modelWithNewCellState cellState coords model
        newModel = { modelWithMove | 
                        gameState = gameState, 
                        winningAnimation = createWinningAnimation gameState model 
                    }
    in 
        if newModel.gameState /= (IN_PROGRESS, []) then
            (newModel, Cmd.none)
        else
            (newModel, nextCmdFn newModel)

createWinningAnimation : (GameState, List (Int, Int)) -> Model -> WinningAnimation
createWinningAnimation gameState model =
    let
        (_, cellCoords) = gameState
        startCoords = ListExt.nth 0 cellCoords (-1, -1)
        endCoords = ListExt.nth 2 cellCoords (-1, -1)
        startRect = BoardUI.getCellRectAt startCoords model
        endRect = BoardUI.getCellRectAt endCoords model
        (xOffset, yOffset) = getWinningLineOffset (endRect.size/2) startCoords endCoords
        (startX0, startY0) = startRect.position
        (endX0, endY0) = endRect.position
        (startX, startY) = (startX0 + xOffset, startY0 + yOffset)
        (endX, endY) = (endX0 + (endRect.size - xOffset), endY0 + (endRect.size - yOffset))
    in
        { 
            startPoint = (startX, startY), 
            currentPoint = (startX, startY),
            endPoint = (endX, endY),
            speed = 300
        }


getWinningLineOffset : Float -> (Int, Int) -> (Int, Int) -> (Float, Float)
getWinningLineOffset mult (row0, col0) (row2, col2) =
    if row0 == row2 && col0 /= col2 then (mult, 0)
    else if row0 /=row2 && col0 == col2 then (0, mult)
    else if (row0, col0)==(0, 2) then (0, mult*2)
    else (0, 0)

modelWithNewCellState : CellState -> (Int, Int) -> Model -> Model
modelWithNewCellState state coords model = 
    let newModel = Board.setCellState state coords model
    in 
        { newModel | highlightedCell = Nothing }

getGameState : CellState -> Model -> (GameState, List (Int, Int))
getGameState cellState model =
    case getWinningLine cellState model of
        Just (cellState, line) -> (WIN, line)
        _ ->
            if hasNoEmptyCellsLeft model then
                (DRAW, [])
            else
                (IN_PROGRESS, [])

getWinningLine : CellState -> Model -> Maybe (CellState, List (Int, Int))
getWinningLine cellState model =
    let wantedStates = List.repeat 3 cellState
    in
        case getWinningRow cellState wantedStates model of
            Just rowIndex -> Just (cellState, getRowCoords rowIndex)
            Nothing ->
                case getWinningColumn cellState wantedStates model of
                    Just columIndex -> Just (cellState, getColumCoords columIndex)
                    Nothing ->
                        if getLeftToRightDiagonal model == wantedStates then
                            Just (cellState, getLeftToRightDiagonalCoords)
                        else if getRightToLeftDiagonal model == wantedStates then
                            Just (cellState, getRightToLeftDiagonalCoords)
                        else Nothing

getWinningRow : CellState -> List CellState -> Model -> Maybe Int
getWinningRow cellState wantedStates model =
        if getCellStatesForRow 0 model == wantedStates then Just 0
        else if getCellStatesForRow 1 model == wantedStates then Just 1
        else if getCellStatesForRow 2 model == wantedStates then Just 2
        else Nothing

getWinningColumn : CellState -> List CellState -> Model -> Maybe Int
getWinningColumn cellState wantedStates model =
        if getCellStatesForColumn 0 model == wantedStates then Just 0
        else if getCellStatesForColumn 1 model == wantedStates then Just 1
        else if getCellStatesForColumn 2 model == wantedStates then Just 2
        else Nothing


getCellStatesForRow : Int -> Model -> List CellState
getCellStatesForRow rowIndex model = 
    Board.getCellStates (getRowCoords rowIndex) model

getCellStatesForColumn : Int -> Model -> List CellState
getCellStatesForColumn columIndex model =
    Board.getCellStates (getColumCoords columIndex) model    

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
getRightToLeftDiagonalCoords = [(0, 2), (1, 1), (2, 0)]

hasNoEmptyCellsLeft : Model -> Bool
hasNoEmptyCellsLeft model =
    List.length (Board.getEmptyCells model) == 0

modelWithEmptyBoard : Model -> Model
modelWithEmptyBoard model = { model | board = Board.getInitialCells }
