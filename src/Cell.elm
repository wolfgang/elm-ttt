module Cell exposing (setHighlightedCell)
import Model exposing (Model, CellRect)
import Board

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

setHighlightedCellAt : (Int, Int) -> Model -> Model
setHighlightedCellAt cellCoords model =
    let 
        cellRect = Board.getCellRectAt cellCoords model
    in
        if cellContainsPoint cellRect model.mousePosition then
            { model | highlightedCell = Just cellCoords }
        else
            model

setHighlightedCell : Model -> Model
setHighlightedCell model =
    List.foldr 
        setHighlightedCellAt
        model 
        Board.cellCoords
