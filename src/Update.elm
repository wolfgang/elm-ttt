module Update exposing (update)
import Msg exposing (Msg(..))
import Model exposing (Model)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    NoOp -> (model, Cmd.none)
    MouseMoved position -> 
        (
            { model | mousePosition = (toFloat position.x, toFloat position.y) }, 
            Cmd.none
        )

