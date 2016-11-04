import Html.App as App
import Color exposing (rgb)
import Mouse
import Model exposing (..)
import Update
import View 
import Board
import AnimationFrame
import Time
import WinningAnimation

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
        board = Board.getInitialCells,
        gameState = IN_PROGRESS,
        winningAnimation = WinningAnimation.init,
        elapsedTime = 0


    },
    Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions =
    (\_ -> Sub.batch 
        [ 
            Mouse.moves MouseMoved,
            Mouse.clicks MouseClicked,
            AnimationFrame.diffs (Tick<<Time.inSeconds)        
        ]
    )



