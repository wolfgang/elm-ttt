module Model exposing (Model)
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
    highlightedCell: Maybe (Int, Int)
}
