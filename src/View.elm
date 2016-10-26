module View exposing(draw)

import Html exposing (Html)

import Collage exposing (collage, rect, filled, move, Form)
import Color exposing (Color, rgb)
import Element exposing (toHtml)
import String
import Text
import Model exposing(Model)

type alias CellRect = {
    position : (Float, Float),
    size: Float
}

draw : Model -> Html msg
draw model = 
    toHtml <| 
        let 
            gs = model.gridSettings
        in 
            collage (round gs.size) (round gs.size)
            (
                drawBackground model
                ++ drawCells model
                ++ drawDebugText model
            )

drawBackground model =
    let gs = model.gridSettings
    in [ rect gs.size gs.size |> filled gs.gridLineColor ] 

drawCells : Model -> List Form
drawCells model = 
    let 
        coords = [
            (0, 0), (1, 0), (2, 0), 
            (0, 1), (1, 1), (2, 1), 
            (0, 2), (1, 2), (2, 2)
        ]
    in
        List.map (\coord -> drawCellAt coord model) coords

drawDebugText model = 
    [ debugPrintAt (0, 0) (toString model.mousePosition)]

drawCellAt : (Int, Int) -> Model -> Form
drawCellAt (row, col) model = 
    let 
        cellRect = getCellRectAt (row, col) model
    in 
        rect cellRect.size cellRect.size
            |> filled model.gridSettings.cellBaseColor
            |> move (toCollageCoords cellRect model)

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