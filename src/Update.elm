module Update exposing (update)
import Msg exposing (Msg(..))
import Model exposing (..)
import Board
import BoardUI
import AI
import Game

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    NoOp -> (model, Cmd.none)
    MouseMoved position -> 
        case model.gameState of
        (IN_PROGRESS, _) ->
            let 
                newModel = { 
                    model | 
                        mousePosition = (toFloat position.x, toFloat position.y),
                        highlightedCell = Nothing
                    }
            in
                (BoardUI.setHighlightedCell newModel, Cmd.none)
        _ -> (model, Cmd.none)

    MouseClicked position -> 
        case model.gameState of
            (IN_PROGRESS, _) ->
                case model.highlightedCell of
                    Nothing -> (model, Cmd.none)
                    Just coords -> 
                        Game.makeMove X_ coords model AI.makeRandomMoveCmd
            _ -> ({ model | board = Board.getInitialCells, gameState = (IN_PROGRESS, []) }, Cmd.none)

    RandomMove coords -> 
        Game.makeMove O_ coords model (\model -> Cmd.none)

    Tick deltaSeconds -> 
        (
            { model | 
                elapsedTime = model.elapsedTime + deltaSeconds,
                winningAnimation = animateWinningLine model.winningAnimation deltaSeconds
            }
            ,
            Cmd.none
        )


animateWinningLine : WinningAnimation -> Float -> WinningAnimation
animateWinningLine winningAnimation deltaSeconds =
    let 
        (currentX, currentY) = winningAnimation.currentPoint
        (endX, endY) = winningAnimation.endPoint
        xDir = fsgn (endX - currentX)
        yDir = fsgn (endY - currentY)
        delta = winningAnimation.speed*deltaSeconds
        totalDistance = distanceSquared winningAnimation.startPoint winningAnimation.endPoint
        currentDistance = distanceSquared winningAnimation.currentPoint winningAnimation.startPoint
    in
        if currentDistance >= totalDistance then
            winningAnimation
        else
            { winningAnimation | 
                currentPoint = (currentX + xDir*delta, currentY + yDir*delta),
                speed = winningAnimation.speed + 80
            }


fsgn : Float -> Float 
fsgn x = 
    if x < 0 then -1
    else if x > 0 then 1
    else 0

distanceSquared : (Float, Float) -> (Float, Float) -> Float
distanceSquared (x0, y0) (x1, y1) =
    let
        diffX = x1 - x0
        diffY = y1 - y0
    in diffX*diffX + diffY*diffY






