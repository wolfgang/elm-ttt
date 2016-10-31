module Msg exposing(Msg(..))
import Mouse

type Msg
  = NoOp
  | MouseMoved Mouse.Position
  | MouseClicked Mouse.Position
  | RandomMove Int
