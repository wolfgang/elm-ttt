module View exposing(draw)

import Html exposing (Html)

import Collage exposing (collage, rect, filled, move, Form)
import Color exposing (Color, rgb)
import Element exposing (toHtml)
import String
import Text
import Model exposing(Model)

type alias CellScreenRect = {
    screenCoords : (Float, Float),
    screenSize: Float
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

drawDebugText model = 
    [ debugPrintAt (0, 0) (toString model.mousePosition)]

getCellRectAt : (Int, Int) -> Model -> CellScreenRect

getCellRectAt (row, col) model =
    let 
        gs = model.gridSettings
        cellSize = (gs.size - gs.gridLineThickness)/3
        cellX = (cellSize + gs.gridLineThickness/2)*(toFloat row)
        cellY = (cellSize + gs.gridLineThickness/2)*(toFloat col)
    in 
        { 
            screenCoords = (cellX + cellSize/2, cellY + cellSize/2), 
            screenSize = cellSize 
        }


toCollage (x, y) gridSize = 
    (x - gridSize/2, -(y - gridSize/2))

drawCellAt : (Int, Int) -> Model -> Form
drawCellAt (row, col) model = 
    let 
        screenRect = getCellRectAt (row, col) model
    in 
        rect screenRect.screenSize screenRect.screenSize
            |> filled model.gridSettings.cellBaseColor
            |> move (toCollage screenRect.screenCoords model.gridSettings.size)

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



