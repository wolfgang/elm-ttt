import Html.App as App
import Color exposing (rgb)
import Mouse
import Model exposing (Model, Cell, CellState(..))
import Update
import View 
import Board

import Msg exposing (Msg(..))

main =
  App.program
    { 
        init = init, 
        update = Update.update, 
        view = View.draw, 
        subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init =
  (
    {
        gridSettings = {
            size = 600,
            gridLineColor = rgb 0 0 0,
            gridLineThickness = 20,
            cellBaseColor = rgb 255 255 255,
            cellHighlightColor = rgb 200 200 200
        },
        mousePosition = (0, 0),
        highlightedCell = Nothing,
        board = initializeBoard

    },
    Cmd.none
  )

initializeBoard : List Cell
initializeBoard = 
    List.map 
        (\coords -> 
            if coords == (1, 0) then { coords = coords, state = O_ }
            else if coords == (2, 2) then { coords = coords, state = X_ }
            else { coords = coords, state = Empty })
        Board.cellCoords

subscriptions : Model -> Sub Msg
subscriptions model = Mouse.moves (\position -> MouseMoved position)

