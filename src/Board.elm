module Board exposing (CellRect, cellCoords)

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
