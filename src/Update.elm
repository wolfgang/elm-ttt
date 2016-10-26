module Update exposing (update)
import Msg exposing (Msg(..))
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

checkHighlightCell : (Int, Int) -> Model -> Model
checkHighlightCell cellCoords model =
    let 
        cellRect = Board.getCellRectAt cellCoords model
    in
        if cellContainsPoint cellRect model.mousePosition then
            { model | highlightedCell = Just cellCoords }
        else
            model

highlightCell : Model -> Model
highlightCell model =
    List.foldr 
        checkHighlightCell
        model 
        Board.cellCoords



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    NoOp -> (model, Cmd.none)
    MouseMoved position -> 
        let 
            newModel = { 
                model | 
                    mousePosition = (toFloat position.x, toFloat position.y),
                    highlightedCell = Nothing
                }
        in
            (highlightCell newModel, Cmd.none)

