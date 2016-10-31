module Board exposing (CellRect, cellCoords, getCellRectAt, setCellState, getEmptyCells, getInitialCells, getCellStates)
import Model exposing (Model, Cell, CellState(Empty))

type alias CellRect = {
    position : (Float, Float),
    size: Float
}

cellCoords : List (Int, Int)
cellCoords = 
    [
        (0, 0), (1, 0), (2, 0), 
        (0, 1), (1, 1), (2, 1), 
        (0, 2), (1, 2), (2, 2)
    ]

getCellRectAt : (Int, Int) -> Model -> CellRect
getCellRectAt (row, col) model =
    let 
        gs = model.gridSettings
        cellSize = (gs.size - gs.gridLineThickness)/3
        cellX = (cellSize + gs.gridLineThickness/2)*(toFloat row)
        cellY = (cellSize + gs.gridLineThickness/2)*(toFloat col)
    in 
        { 
            position = (cellX, cellY), 
            size = cellSize 
        }

setCellState : CellState -> (Int,Int) -> Model -> Model
setCellState state coords model =
    { model | board = List.map (\cell -> setStateOfCell cell coords state) model.board}


getEmptyCells : Model -> List (Int, Int)
getEmptyCells model =
    List.filter (\cell -> cell.state == Empty) model.board
    |> List.map (\cell -> cell.coords) 

getInitialCells : List Cell
getInitialCells = 
    List.map (\coords -> { coords = coords, state = Empty }) cellCoords


getCellStates : (Int, Int) -> (Int, Int) -> (Int, Int) -> Model -> List CellState
getCellStates coords1 coords2 coords3 model =
    let wantedCoords = [coords1, coords2, coords3]
    in
        List.foldr 
            (\cell result -> 
                if List.member cell.coords wantedCoords then 
                    result ++ [cell.state] 
                else result)
            []
            model.board

setStateOfCell : Cell -> (Int, Int) -> CellState -> Cell
setStateOfCell cell wantedCoords wantedState =
    if cell.coords == wantedCoords then
        { cell | state = wantedState }
    else
        cell


