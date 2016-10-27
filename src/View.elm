module View exposing(draw)

import Html exposing (Html)
import String
import Text

import Collage exposing (collage, rect, circle, filled, move, outlined, solid, Shape, Form)
import Color exposing (Color, rgb)
import Element exposing (toHtml)
import Model exposing (Model, CellState(..))
import Board exposing (CellRect)

draw : Model -> Html msg
draw model = 
    toHtml <| 
        let 
            gs = model.gridSettings
        in 
            collage (round gs.size) (round gs.size)
            (
                drawBackground model
                ++ drawBaseCells model
                ++ drawOccupiedCells model
                ++ drawHighlightedCell model
                ++ drawDebugText model
            )

drawBackground model =
    let gs = model.gridSettings
    in [ rect gs.size gs.size |> filled gs.gridLineColor ] 

drawBaseCells : Model -> List Form
drawBaseCells model = 
    let gs = model.gridSettings
    in
        List.map (\cell -> drawCellAt cell.coords gs.cellBaseColor model) model.board


drawOccupiedCells : Model -> List Form
drawOccupiedCells model =
    List.foldr (
        \cell rects -> 
            if (cell.state == Empty) then rects
            else rects ++ [(drawOccupiedCell cell.coords model)]
        )
        []
        model.board

drawOccupiedCell : (Int, Int) -> Model -> Form
drawOccupiedCell coords model =
        let 
            cellRect = Board.getCellRectAt coords model
            baseLineStyle  = solid Color.black
        in 
            drawShapeInCellRect cellRect (circle (cellRect.size/3)) (outlined { baseLineStyle | width = 8 }) model


drawHighlightedCell : Model -> List Form
drawHighlightedCell model = 
    case model.highlightedCell of
        Nothing -> []
        Just coords -> 
            let gs = model.gridSettings
            in [drawCellAt coords gs.cellHighlightColor model]

drawDebugText model = 
    [ debugPrintAt (0, 0) (toString model.mousePosition)]

drawCellAt : (Int, Int) -> Color -> Model -> Form
drawCellAt (row, col) color model = 
    let 
        cellRect = Board.getCellRectAt (row, col) model
    in 
        drawShapeInCellRect cellRect (rect cellRect.size cellRect.size) (filled color) model


drawShapeInCellRect : CellRect -> Shape -> (Shape -> Form) -> Model -> Form
drawShapeInCellRect cellRect shape styleFn model = 
        shape
            |> styleFn
            |> move (toCollageCoords cellRect model)


toCollageCoords : CellRect -> Model -> (Float, Float)
toCollageCoords rect model = 
    let 
        gridSize = model.gridSettings.size
        (x, y) = rect.position
    in (x - gridSize/2 + rect.size/2, -(y - gridSize/2 + rect.size/2))

debugPrintAt pos string =
  let
    textWidth = (toFloat (String.length string)*4)
  in
    Text.fromString string
    |> Text.color Color.black
    |> Text.monospace
    |> Text.height 12
    |> Element.leftAligned
    |> Collage.toForm
    |> move pos