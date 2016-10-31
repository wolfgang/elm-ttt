module View exposing(draw)

import Html exposing (Html)
import String
import Text

import Collage exposing (..)
import Color exposing (Color, rgb)
import Element exposing (toHtml)
import Model exposing (Model, Cell, CellState(..))
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
            else rects ++ [(drawOccupiedCell cell model)]
        )
        []
        model.board

drawOccupiedCell : Cell -> Model -> Form
drawOccupiedCell cell model =
        let 
            cellRect = Board.getCellRectAt cell.coords model
            baseLineStyle  = solid Color.black
        in  
            if cell.state == O_ then drawOInCell cellRect model
            else drawXInCell cellRect model

drawOInCell : CellRect -> Model -> Form
drawOInCell cellRect model =
    let baseLineStyle  = solid Color.black
    in 
        drawFormInCellRect 
            cellRect 
            (circle (cellRect.size/3) |> outlined { baseLineStyle | width = 8 })
            model   

drawXInCell : CellRect -> Model -> Form
drawXInCell cellRect model = 
    let baseRect = rect 8 cellRect.size |> filled Color.black
    in 
        group [
            drawFormInCellRect 
                cellRect 
                (rotate (degrees 45) baseRect)
                model,
            drawFormInCellRect 
                cellRect 
                (rotate (degrees -45) baseRect)
                model
            ]

drawHighlightedCell : Model -> List Form
drawHighlightedCell model = 
    case model.highlightedCell of
        Nothing -> []
        Just coords -> 
            let gs = model.gridSettings
            in [drawCellAt coords gs.cellHighlightColor model]

drawDebugText model = 
    [ 
        debugPrintAt (0, -50) (toString model.mousePosition),
        debugPrintAt (0, -62) (toString model.debugCoords)
    ]

drawCellAt : (Int, Int) -> Color -> Model -> Form
drawCellAt (row, col) color model = 
    let 
        cellRect = Board.getCellRectAt (row, col) model
    in 
        drawFormInCellRect 
            cellRect 
            (filled color (rect cellRect.size cellRect.size)) 
            model

drawFormInCellRect : CellRect -> Form -> Model -> Form
drawFormInCellRect cellRect form model = 
    form |> move (toCollageCoords cellRect model)



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
    |> Text.color Color.red
    |> Text.monospace
    |> Text.height 12
    |> Element.leftAligned
    |> Collage.toForm
    |> move pos
