module ListExtTest exposing (..)

import Test exposing (..)
import Expect

import ListExt


all : Test
all =
    describe "ListExt" [
        describe  "nth should" [
            test "return given default if index out of range" <|
                \() ->
                    Expect.equal -1 (ListExt.nth 5 [1, 2] -1)
            ,
            test "return first element for index 0" <|
                \() -> Expect.equal 1234 (ListExt.nth 0 [1234, 5678] -1)
            ,
            test "return nth element for given index" <|
                \() -> Expect.equal 2 (ListExt.nth 2 [0, 1, 2, 3, 4] -1)
        ]
    ]
