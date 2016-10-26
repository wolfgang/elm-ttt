module Update exposing (update)
import Msg exposing (Msg(..))
import Model exposing (Model)
import Board
import Cell


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
            (Cell.setHighlightedCell newModel, Cmd.none)

