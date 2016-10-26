import Html.App as App
import Color exposing (rgb)
import Mouse
import Model exposing (Model)
import Update
import View 

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
            cellBaseColor = rgb 255 255 255
        },
        mousePosition = (0, 0)

    },
    Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions model = Mouse.moves (\position -> MouseMoved position)

