module Update exposing (update)
import Msg exposing (Msg(..))
import Model exposing (Model, CellRect)


--rectContainsPoint : CellRect -> (Float, Float) -> Bool
--rectContainsPoint rect (x, y) = False

--checkHighlightCell : (Int, Int) -> Model -> Model
--checkHighlightCell cellCoords model =
--    let 
--        cellRect = Board.getCellRectAt cellCoords model
--    in
--        if rectContainsPoint cellRect model.mousePosition then
--            { model | highlightedCell = Just cellRect }
--        else
--            model

--highlightCell : Model -> Model
--highlightCell model =
--    List.foldr 
--        checkHighlightCell
--        model 
--        Board.cellCoords

highlightCell : Model -> Model
highlightCell model = model



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    NoOp -> (model, Cmd.none)
    MouseMoved position -> 
        let 
            newModel = { model | mousePosition = (toFloat position.x, toFloat position.y) }
        in
            (highlightCell newModel, Cmd.none)

