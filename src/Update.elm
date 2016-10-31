module Update exposing (update)
import Msg exposing (Msg(..))
import Model exposing (Model, Cell, CellState(..))
import Board
import BoardUI
import AI
import Game

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
            (BoardUI.setHighlightedCell newModel, Cmd.none)

    MouseClicked position -> 
        case model.highlightedCell of
            Nothing -> (model, Cmd.none)
            Just coords -> 
                Game.makeMove X_ coords model AI.makeRandomMoveCmd

    RandomMove coords -> 
        Game.makeMove O_ coords model (\model -> Cmd.none)