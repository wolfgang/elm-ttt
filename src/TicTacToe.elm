import Html exposing (Html)
import Html.App as App

import Collage exposing (collage, rect, filled, move, Form)
import Color exposing (Color, rgb)
import Element exposing (toHtml)
import Mouse
import String
import Text


main =
  App.program
    { 
        init = init, 
        update = update, 
        view = view, 
        subscriptions = subscriptions
    }

type Msg
  = NoOp
  | MouseMoved Mouse.Position

type alias Model = {
    gridSettings: {
        size: Float,
        gridLineColor: Color,
        gridLineThickness: Float,
        cellBaseColor: Color
    },
    mousePosition: (Float, Float)
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
        },
        mousePosition = (0, 0)

    },
    Cmd.none
  )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    NoOp -> (model, Cmd.none)
    MouseMoved position -> 
        (
            { model | mousePosition = (toFloat position.x, toFloat position.y) }, 
            Cmd.none
        )

view : Model -> Html msg
view model = 
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
    [ debugPrintAt (0, 0) (toString (mousePosToCollage model))]

mousePosToCollage : Model -> (Float, Float)
mousePosToCollage model = 
    let 
        mp = model.mousePosition
        gs = model.gridSettings
    in
        toCollage model.mousePosition gs.size

subscriptions : Model -> Sub Msg
subscriptions model = Mouse.moves onMouseMove

onMouseMove : Mouse.Position -> Msg
onMouseMove position = MouseMoved position


getCellRectAt : (Int, Int) -> Model -> CellScreenRect

getCellRectAt (row, col) model =
    let 
        gs = model.gridSettings
        cellSize = (gs.size - gs.gridLineThickness)/3
        cellX = (cellSize + gs.gridLineThickness/2)*(toFloat row)
        cellY = (cellSize + gs.gridLineThickness/2)*(toFloat col)
    in 
        { 
            screenCoords = (toCollage (cellX + cellSize/2, cellY + cellSize/2) gs.size), 
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



