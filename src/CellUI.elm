module CellUI exposing (setHighlightedCell, getCellRectAt)
import Model exposing (Model, Cell, CellState(Empty))
import Board exposing (CellRect)

setHighlightedCell : Model -> Model
setHighlightedCell model =
    List.foldr setHighlightedCellAt model model.board


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

setHighlightedCellAt : Cell -> Model -> Model
setHighlightedCellAt cell model =
    let 
        cellCoords = cell.coords
        cellRect = getCellRectAt cellCoords model
    in
        if cellContainsPoint cellRect model.mousePosition && cell.state == Empty then
            { model | highlightedCell = Just cellCoords }
        else
            model

cellContainsPoint : CellRect -> (Float, Float) -> Bool
cellContainsPoint cell (x, y) = 
    let 
        (cellX, cellY) = cell.position
        cellSize = cell.size
    in
        x >= cellX && 
        x <= cellX + cellSize  &&
        y >= cellY &&
        y <= cellY + cellSize

-- Alternative implementation using recursion for early exit

setHighlightedCell2 : List (Int, Int) -> Model -> Model
setHighlightedCell2 cellCoords model =
    case List.head cellCoords of
        Nothing -> model
        Just coord -> 
            let cellRect = getCellRectAt coord model
            in
                if cellContainsPoint cellRect model.mousePosition then
                    { model | highlightedCell = Just coord }
                else
                    setHighlightedCell2 (tailOrEmptyList cellCoords) model

tailOrEmptyList list = 
    case List.tail list of
        Nothing -> []
        Just tail -> tail
