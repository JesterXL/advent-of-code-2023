module Day3test exposing (..)

import Array exposing (Array)
import Day3 exposing (parsePartNumbersFromRows, parseRow)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, floatRange, int, list, string)
import Test exposing (..)


parseRowSuite : Test
parseRowSuite =
    describe "parseRow"
        [ test "should get a list of numbers and symbols for a row" <|
            \_ ->
                let
                    row =
                        parseRow 0 "467..114.."
                in
                Expect.equal (List.length row.partNumbers) 2
        , test "should find 467" <|
            \_ ->
                let
                    partNumber =
                        parseRow 0 "467..114.."
                            |> .partNumbers
                            |> Array.fromList
                            |> Array.get 0
                            |> Maybe.withDefault { startIndex = 0, endIndex = 0, value = 0 }
                in
                Expect.equal partNumber.value 467
        , test "should find 467 at start index of 0" <|
            \_ ->
                let
                    partNumber =
                        parseRow 0 "467..114.."
                            |> .partNumbers
                            |> Array.fromList
                            |> Array.get 0
                            |> Maybe.withDefault { startIndex = 0, endIndex = 0, value = 0 }
                in
                Expect.equal partNumber.startIndex 0
        , test "should find 467 at endIndex of 2" <|
            \_ ->
                let
                    partNumber =
                        parseRow 0 "467..114.."
                            |> .partNumbers
                            |> Array.fromList
                            |> Array.get 0
                            |> Maybe.withDefault { startIndex = 0, endIndex = 0, value = 0 }
                in
                Expect.equal partNumber.endIndex 2
        , test "should find 114" <|
            \_ ->
                let
                    partNumber =
                        parseRow 0 "467..114.."
                            |> .partNumbers
                            |> Array.fromList
                            |> Array.get 1
                            |> Maybe.withDefault { startIndex = 0, endIndex = 0, value = 0 }
                in
                Expect.equal partNumber.value 114
        , test "should find 114 at index 5" <|
            \_ ->
                let
                    partNumber =
                        parseRow 0 "467..114.."
                            |> .partNumbers
                            |> Array.fromList
                            |> Array.get 1
                            |> Maybe.withDefault { startIndex = 0, endIndex = 0, value = 0 }
                in
                Expect.equal partNumber.startIndex 5
        , test "should find 114 at end index 7" <|
            \_ ->
                let
                    partNumber =
                        parseRow 0 "467..114.."
                            |> .partNumbers
                            |> Array.fromList
                            |> Array.get 1
                            |> Maybe.withDefault { startIndex = 0, endIndex = 0, value = 0 }
                in
                Expect.equal partNumber.endIndex 7
        , test "should get a star symbol" <|
            \_ ->
                let
                    row =
                        parseRow 0 ".....+.58."
                in
                Expect.equal (List.length row.symbols) 1
        , test "should get a star symbol at start index 5" <|
            \_ ->
                let
                    symbol =
                        parseRow 0 ".....+.58."
                            |> .symbols
                            |> Array.fromList
                            |> Array.get 0
                            |> Maybe.withDefault { index = 0, value = '!' }
                in
                Expect.equal symbol.index 5
        ]


parsePartNumbersFromRowsSuite : Test
parsePartNumbersFromRowsSuite =
    describe "parsePartNumbersFromRows"
        [ test "should get a single outlier" <|
            \_ ->
                let
                    partNumbersFromRows =
                        parsePartNumbersFromRows
                            """467..114..
...*......
..35..633."""
                in
                Expect.equal (List.length partNumbersFromRows.rogueNumbers) 1
        , test "should get 3 part numbers" <|
            \_ ->
                let
                    partNumbersFromRows =
                        parsePartNumbersFromRows
                            """467..114..
...*......
..35..633."""
                in
                Expect.equal (List.length partNumbersFromRows.partNumbers) 3
        ]
