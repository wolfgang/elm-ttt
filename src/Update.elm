module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (..)
import Board
import BoardUI
import AI
import Game
import WinningAnimation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MouseMoved position ->
            case model.gameState of
                IN_PROGRESS ->
                    let
                        newModel =
                            { model
                                | mousePosition = ( toFloat position.x, toFloat position.y )
                                , highlightedCell = Nothing
                            }
                    in
                        ( BoardUI.setHighlightedCell newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseClicked position ->
            case model.gameState of
                IN_PROGRESS ->
                    case model.highlightedCell of
                        Nothing ->
                            ( model, Cmd.none )

                        Just coords ->
                            Game.makeMove X_ coords model AI.makeRandomMoveCmd

                _ ->
                    ( { model
                        | mousePosition = ( toFloat position.x, toFloat position.y )
                        , board = Board.getInitialCells
                        , gameState = IN_PROGRESS
                        , winningAnimation = WinningAnimation.init
                      }
                    , Cmd.none
                    )

        RandomMove coords ->
            Game.makeMove O_ coords model (\model -> Cmd.none)

        Tick deltaSeconds ->
            ( { model | winningAnimation = WinningAnimation.animate model.winningAnimation deltaSeconds }
            , Cmd.none
            )
