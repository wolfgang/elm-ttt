module ElmFunctions exposing (..)

import Test exposing (..)
import Expect

addInts : Int -> Int -> Int
addInts n1 n2 = n1 + n2

all : Test
all = 
    describe "Elm Functions" [
        test "call a function" <|
            \() -> Expect.equal 5 (addInts 3 2)
        ,
        test "call a function 2" <|
            \() -> Expect.equal 5 (3 |> addInts 2)
        ,
        test "call a function 3" <|
            \() -> Expect.equal 5 (addInts 2 <| 3)
        ,
        test "call a function 4" <|
            \() -> Expect.equal 15 (addInts 2 (addInts 3 (addInts 5 5)))
        ,
        test "call a function 5" <|
            \() -> Expect.equal 15 (5 |> addInts 5 |> addInts 3 |> addInts 2)
        ,
        test "call a function 6" <|
            \() -> Expect.equal 15 ( addInts 2 <| addInts 3 <| addInts 5 <| 5)
        ,
        test "call a function 7" <|
            \() -> Expect.equal 15 ((addInts 2 >> addInts 3 >> addInts) 5 5)

    ]