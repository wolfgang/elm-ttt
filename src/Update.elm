module Update exposing (update)
import Msg exposing (Msg(..))
import Model exposing (..)
import Board
import BoardUI
import AI
import Game

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    NoOp -> (model, Cmd.none)
    MouseMoved position -> 
        case model.gameState of
        (IN_PROGRESS, _) ->
            let 
                newModel = { 
                    model | 
                        mousePosition = (toFloat position.x, toFloat position.y),
                        highlightedCell = Nothing
                    }
            in
                (BoardUI.setHighlightedCell newModel, Cmd.none)
        _ -> (model, Cmd.none)

    MouseClicked position -> 
        case model.gameState of
            (IN_PROGRESS, _) ->
                case model.highlightedCell of
                    Nothing -> (model, Cmd.none)
                    Just coords -> 
                        Game.makeMove X_ coords model AI.makeRandomMoveCmd
            _ -> ({ model | board = Board.getInitialCells, gameState = (IN_PROGRESS, []) }, Cmd.none)

    RandomMove coords -> 
        Game.makeMove O_ coords model (\model -> Cmd.none)

    Tick deltaSeconds -> ({ model | elapsedTime = model.elapsedTime + deltaSeconds }, Cmd.none)