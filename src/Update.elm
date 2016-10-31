module Update exposing (update)
import Msg exposing (Msg(..))
import Model exposing (Model, Cell, CellState(..))
import Board
import CellUI
import AI
import Task

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
                let newModel = modelWithNewCellState X_ coords model
                in (newModel, nextCmd newModel (AI.makeRandomMoveCmd newModel))

    RandomMove coords -> (modelWithNewCellState O_ coords model, Cmd.none)

    ClearBoard -> ( { model | board = Board.getInitialCells }, Cmd.none)

modelWithNewCellState : CellState -> (Int, Int) -> Model -> Model
modelWithNewCellState state coords model = 
    let newModel = Board.setCellState state coords model
    in 
        { newModel | highlightedCell = Nothing }

nextCmd : Model -> Cmd Msg -> Cmd Msg
nextCmd model wantedCmd = 
    if List.length (Board.getEmptyCells model) == 0 then
        Task.perform (always ClearBoard) (always ClearBoard) (Task.succeed ())
    else
        wantedCmd


