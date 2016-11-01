module Model exposing (Model, Cell, GameState(..), CellState(..))
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
    gameState: GameState
}

type alias Cell = {
    coords: (Int, Int),
    state: CellState
}

type CellState = O_ | X_ | Empty

type GameState
    = WIN_O
    | WIN_X
    | DRAW
    | IN_PROGRESS
