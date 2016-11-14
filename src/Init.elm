module Init exposing (init)

import Color
import Board
import Model exposing (Model, GameState (IN_PROGRESS))
import Msg exposing (Msg)
import WinningAnimation


init : (Model, Cmd Msg)
init =
  (
    {
        gridSettings = {
            size = 600,
            gridLineColor = Color.rgb 0 0 0,
            gridLineThickness = 20,
            cellBaseColor = Color.rgb 255 255 255,
            cellHighlightColor = Color.rgb 200 200 200
        },
        mousePosition = (0, 0),
        highlightedCell = Nothing,
        board = Board.getInitialCells,
        gameState = IN_PROGRESS,
        winningAnimation = WinningAnimation.init
    },
    Cmd.none
  )
