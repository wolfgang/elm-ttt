port module Main exposing (..)

import ElmTestWorks
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import Test exposing (describe)

allTests =  
    describe "All tests"  [
        ElmTestWorks.all
    ]


main : Program Value
main =
    run emit allTests


port emit : ( String, Value ) -> Cmd msg
