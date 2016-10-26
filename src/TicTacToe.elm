import Html exposing (Html)
import Html.App as App

import Collage exposing (collage, rect, filled, move, Form)
import Color exposing (Color, rgb)
import Element exposing (toHtml)


main =
  App.program
    { 
        init = init, 
        view = view, 
        update = update, 
        subscriptions = (\_ -> Sub.batch [])
    }

type Msg
  = NoOp

type alias Model = {
    gridSettings: {
        size: Float,
        gridLineColor: Color,
        gridLineThickness: Float,
        cellBaseColor: Color
    }
}

type alias CellScreenRect = {
    screenCoords : (Float, Float),
    screenSize: Float
}

init : (Model, Cmd Msg)
init =
  (
    {
        gridSettings = {
            size = 600,
            gridLineColor = rgb 0 0 0,
            gridLineThickness = 20,
            cellBaseColor = rgb 255 255 255
        }
    },
    Cmd.none
  )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)


getCellScreenRectAt : (Int, Int) -> Model -> CellScreenRect

getCellScreenRectAt (row, col) model =
    let 
        gs = model.gridSettings
        cellSize = (gs.size - gs.gridLineThickness)/3
        cellX = (cellSize + gs.gridLineThickness/2)*(toFloat row)
        cellY = (cellSize + gs.gridLineThickness/2)*(toFloat col)
        (baseX, baseY) = (-gs.size/2 + cellSize/2, gs.size/2 - cellSize/2)

    in 
        { screenCoords = (cellX + baseX, -cellY + baseY), screenSize = cellSize }

drawCellAt : (Int, Int) -> Model -> Form
drawCellAt (row, col) model = 
    let 
        screenRect = getCellScreenRectAt (row, col) model
    in 
        rect screenRect.screenSize screenRect.screenSize
            |> filled model.gridSettings.cellBaseColor
            |> move screenRect.screenCoords

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




view : Model -> Html msg
view model = 
    toHtml <| 
        let 
            gs = model.gridSettings

        in 
            collage (round gs.size) (round gs.size)
            ([ rect gs.size gs.size |> filled gs.gridLineColor ] ++ (drawCells model))

