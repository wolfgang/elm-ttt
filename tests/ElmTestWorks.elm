module ElmTestWorks exposing (..)

import Test exposing (..)
import Expect
import String


all : Test
all =
    describe "Elm-Test Some Test"
        [ test "Addition" <|
            \() ->
                Expect.equal (3 + 7) 11
        ]
