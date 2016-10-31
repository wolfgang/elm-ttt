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
                makeMove X_ coords model AI.makeRandomMoveCmd

    RandomMove coords -> 
        makeMove O_ coords model (\model -> Cmd.none)

makeMove :  CellState -> (Int, Int) -> Model -> (Model -> Cmd Msg) -> (Model, Cmd Msg)
makeMove cellState coords model nextCmdFn =
    let newModel = modelWithNewCellState cellState coords model
    in 
        if isGameOver newModel then
            (modelWithEmptyBoard newModel, Cmd.none)
        else
            (newModel, nextCmdFn newModel)


modelWithNewCellState : CellState -> (Int, Int) -> Model -> Model
modelWithNewCellState state coords model = 
    let newModel = Board.setCellState state coords model
    in 
        { newModel | highlightedCell = Nothing }

isGameOver : Model -> Bool
isGameOver model = List.length (Board.getEmptyCells model) == 0

modelWithEmptyBoard : Model -> Model
modelWithEmptyBoard model = { model | board = Board.getInitialCells }

