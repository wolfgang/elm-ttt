port module Main exposing (..)

import ElmTestWorks
import ListExtTest
import InitTest
import BoardTest
import ElmFunctions
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import Test exposing (describe)


allTests =
    describe "All tests"
        [ ElmTestWorks.all
        , ListExtTest.all
        , InitTest.all
        , BoardTest.all
        , ElmFunctions.all
        ]


main =
    run emit allTests


port emit : ( String, Value ) -> Cmd msg
