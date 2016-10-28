module Update exposing (update)
import Msg exposing (Msg(..))
import Model exposing (Model, Cell, CellState(..))
import Board
import CellUI


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
            (CellUI.setHighlightedCell newModel, Cmd.none)

    MouseClicked position -> 
        case model.highlightedCell of
            Nothing -> (model, Cmd.none)
            Just coords -> 
                let newModel = CellUI.setCellState X_ coords model
                in 
                    ({ newModel | highlightedCell = Nothing }, Cmd.none)
