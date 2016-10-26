module Board exposing (CellRect, cellCoords, getCellRectAt)
import Model exposing (Model)

type alias CellRect = {
    position : (Float, Float),
    size: Float
}

cellCoords : List (Int, Int)
cellCoords = 
    [
        (0, 0), (1, 0), (2, 0), 
        (0, 1), (1, 1), (2, 1), 
        (0, 2), (1, 2), (2, 2)
    ]

getCellRectAt : (Int, Int) -> Model -> CellRect
getCellRectAt (row, col) model =
    let 
        gs = model.gridSettings
        cellSize = (gs.size - gs.gridLineThickness)/3
        cellX = (cellSize + gs.gridLineThickness/2)*(toFloat row)
        cellY = (cellSize + gs.gridLineThickness/2)*(toFloat col)
    in 
        { 
            position = (cellX, cellY), 
            size = cellSize 
        }
