module Update exposing (update)
import Msg exposing (Msg(..))
import Model exposing (Model)
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

