module Cell exposing (setHighlightedCell)
import Model exposing (Model)
import Board exposing (CellRect)

setHighlightedCell : Model -> Model
setHighlightedCell model =
    List.foldr 
        setHighlightedCellAt
        model 
        Board.cellCoords
--  setHighlightedCell2 Board.cellCoords model

setHighlightedCellAt : (Int, Int) -> Model -> Model
setHighlightedCellAt cellCoords model =
    let 
        cellRect = Board.getCellRectAt cellCoords model
    in
        if cellContainsPoint cellRect model.mousePosition then
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
            let cellRect = Board.getCellRectAt coord model
            in
                if cellContainsPoint cellRect model.mousePosition then
                    { model | highlightedCell = Just coord }
                else
                    setHighlightedCell2 (tailOrEmptyList cellCoords) model

tailOrEmptyList list = 
    case List.tail list of
        Nothing -> []
        Just tail -> tail
