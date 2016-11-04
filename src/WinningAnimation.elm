module WinningAnimation exposing (create)
import ListExt
import BoardUI
import Model exposing (Model, WinningAnimation)

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
            speed = 300
        }

getEdgeOffsets : Float -> (Int, Int) -> (Int, Int) -> (Float, Float)
getEdgeOffsets mult (row0, col0) (row2, col2) =
    if row0 == row2 && col0 /= col2 then (mult, 0)
    else if row0 /=row2 && col0 == col2 then (0, mult)
    else if (row0, col0)==(0, 2) then (0, mult*2)
    else (0, 0)
