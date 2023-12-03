module Day3test exposing (..)

import Array exposing (Array)
import Day3 exposing (parseRow)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, floatRange, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "2023 - Day 3"
        [ describe "parseRow"
            [ test "should get a list of numbers and symbols for a row" <|
                \_ ->
                    let
                        row =
                            parseRow 0 "467..114.."
                    in
                    Expect.equal (List.length row.partNumbers) 2
            ]
        ]
