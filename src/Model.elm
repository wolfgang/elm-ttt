module Model exposing (Model, Cell, CellState(..))
import Color exposing (Color)


type alias Model = {
    gridSettings: {
        size: Float,
        gridLineColor: Color,
        gridLineThickness: Float,
        cellBaseColor: Color,
        cellHighlightColor: Color
    },
    mousePosition: (Float, Float),
    highlightedCell: Maybe (Int, Int),
    board: List Cell,
    debugIndex: Int
}

type alias Cell = {
    coords: (Int, Int),
    state: CellState
}

type CellState = O_ | X_ | Empty
