module Update exposing (update)
import Msg exposing (Msg(..))
import Model exposing (Model, CellState(..))
import Board
import CellUI
import Random


getFreeCells model =
    List.filter (\cell -> cell.state == Empty) model.board
    |> List.map (\cell -> cell.coords) 
    
getNth n list = 
    List.drop n list |> safeHead

safeHead list =
    case List.head list of
        Nothing -> (-1, -1)
        Just x -> x


makeRandomMoveCmd model = 
    let
        freeCells = getFreeCells model
    in 
        Random.generate RandomMove 
            (Random.map (\index -> getNth index freeCells) (Random.int 0 (List.length freeCells - 1)))

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
                let newModel = Board.setCellState X_ coords model
                in 
                    ({ newModel | highlightedCell = Nothing }, (makeRandomMoveCmd newModel))

    RandomMove coords -> 
        let newModel = Board.setCellState O_ coords model
        in ({ newModel | highlightedCell = Nothing }, Cmd.none)
