module Model exposing (..)
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
    gameState: (GameState, List (Int, Int)),
    winningAnimation: WinningAnimation,
    elapsedTime: Float
}

type alias Cell = {
    coords: (Int, Int),
    state: CellState
}

type CellState = O_ | X_ | Empty

type GameState
    = WIN
    | DRAW
    | IN_PROGRESS

type alias WinningAnimation = { 
        startPoint: (Float, Float), 
        currentPoint: (Float, Float),
        endPoint: (Float, Float),
        speed: Float
    }