module Model exposing (Model, CellRect)
import Color exposing (Color)

type alias CellRect = {
    position : (Float, Float),
    size: Float
}

type alias Model = {
    gridSettings: {
        size: Float,
        gridLineColor: Color,
        gridLineThickness: Float,
        cellBaseColor: Color,
        cellHighlightColor: Color
    },
    mousePosition: (Float, Float),
    highlightedCell: Maybe (Int, Int)
}
