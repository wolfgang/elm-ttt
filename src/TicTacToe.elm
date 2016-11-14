import Html.App as App
import Color exposing (rgb)
import Mouse
import Model exposing (Model)
import Update
import View 
import AnimationFrame
import Time
import Init

import Msg exposing (Msg(..))

main =
  App.program
    { 
        init = Init.init, 
        update = Update.update, 
        view = View.draw, 
        subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions =
    (\_ -> Sub.batch 
        [ 
            Mouse.moves MouseMoved,
            Mouse.clicks MouseClicked,
            AnimationFrame.diffs (Tick<<Time.inSeconds)        
        ]
    )



