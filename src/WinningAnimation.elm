module WinningAnimation exposing (init, create, animate)
import ListExt
import BoardUI
import Model exposing (Model, WinningAnimation)

gAnimBaseSpeed = 200
gAnimAcceleration = 300

init : WinningAnimation
init = {
    startPoint = (0, 0), 
    currentPoint = (0, 0),
    endPoint = (0, 0),
    speed = 0 }

create : List (Int, Int) -> Model -> WinningAnimation
create winningLine model =
    let
        startCoords = ListExt.nth 0 winningLine (-1, -1)
        endCoords = ListExt.nth 2 winningLine (-1, -1)
        startRect = BoardUI.getCellRectAt startCoords model
        endRect = BoardUI.getCellRectAt endCoords model
        (xOffset, yOffset) = getEdgeOffsets (endRect.size/2) startCoords endCoords
        (startX0, startY0) = startRect.position
        (endX0, endY0) = endRect.position
        (startX, startY) = (startX0 + xOffset, startY0 + yOffset)
        (endX, endY) = (endX0 + (endRect.size - xOffset), endY0 + (endRect.size - yOffset))
    in
        { 
            startPoint = (startX, startY), 
            currentPoint = (startX, startY),
            endPoint = (endX, endY),
            speed = gAnimBaseSpeed
        }

animate : WinningAnimation -> Float -> WinningAnimation
animate winningAnimation deltaSeconds =
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
                speed = winningAnimation.speed + gAnimAcceleration
            }

getEdgeOffsets : Float -> (Int, Int) -> (Int, Int) -> (Float, Float)
getEdgeOffsets mult (row0, col0) (row2, col2) =
    if row0 == row2 && col0 /= col2 then (mult, 0)
    else if row0 /=row2 && col0 == col2 then (0, mult)
    else if (row0, col0)==(0, 2) then (0, mult*2)
    else (0, 0)

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


