module ListExtTest exposing (..)

import Test exposing (..)
import Expect
import ListExt
import Fuzz


myFuzz =
    fuzzWith { runs = 10 } (Fuzz.intRange 0 9)


all : Test
all =
    describe "ListExt"
        [ describe "nth should"
            [ test "return given default if index out of range" <|
                \() ->
                    Expect.equal -1 (ListExt.nth 5 [ 1, 2 ] -1)
            , test "return first element for index 0" <|
                \() -> Expect.equal 1234 (ListExt.nth 0 [ 1234, 5678 ] -1)
            , myFuzz "return nth element of int list" <|
                \index ->
                    let
                        lst =
                            [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
                    in
                        Expect.equal index (ListExt.nth index lst -1)
            , myFuzz "return nth element of string list" <|
                \index ->
                    let
                        lst =
                            [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
                    in
                        Expect.equal (toString index) (ListExt.nth index lst "")
            ]
        ]
