module Msg exposing(Msg(..))
import Mouse
import Time exposing (Time)

type Msg
  = NoOp
  | MouseMoved Mouse.Position
  | MouseClicked Mouse.Position
  | RandomMove (Int, Int)
  | Tick Time
