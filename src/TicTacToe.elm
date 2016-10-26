import Html.App as App
import Color exposing (rgb)
import Mouse
import Model exposing(Model)
import View 


main =
  App.program
    { 
        init = init, 
        update = update, 
        view = View.draw, 
        subscriptions = subscriptions
    }

type Msg
  = NoOp
  | MouseMoved Mouse.Position

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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    NoOp -> (model, Cmd.none)
    MouseMoved position -> 
        (
            { model | mousePosition = (toFloat position.x, toFloat position.y) }, 
            Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model = Mouse.moves onMouseMove

onMouseMove : Mouse.Position -> Msg
onMouseMove position = MouseMoved position

