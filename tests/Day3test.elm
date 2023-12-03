module Day3test exposing (..)

import Array exposing (Array)
import Day3 exposing (numberNextToSymbol, parsePartNumbersFromRows, parseRow)
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
                            |> Maybe.withDefault { rowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
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
                            |> Maybe.withDefault { rowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
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
                            |> Maybe.withDefault { rowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
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
                            |> Maybe.withDefault { rowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
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
                            |> Maybe.withDefault { rowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
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
                            |> Maybe.withDefault { rowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
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
                            |> Maybe.withDefault { rowIndex = 0, index = 0, value = '!' }
                in
                Expect.equal symbol.index 5
        ]


parsePartNumbersFromRowsSuite : Test
parsePartNumbersFromRowsSuite =
    describe "parsePartNumbersFromRows"
        [ test "should get 2 outliers" <|
            \_ ->
                let
                    partNumbersFromRows =
                        parsePartNumbersFromRows
                            """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
                in
                Expect.equal (List.length partNumbersFromRows.rogueNumbers) 2

        --         , test "should get 3 part numbers" <|
        --             \_ ->
        --                 let
        --                     partNumbersFromRows =
        --                         parsePartNumbersFromRows
        --                             """467..114..
        -- ...*......
        -- ..35..633.
        -- ......#...
        -- 617*......
        -- .....+.58.
        -- ..592.....
        -- ......755.
        -- ...$.*....
        -- .664.598.."""
        --                 in
        --                 Expect.equal (List.length partNumbersFromRows.partNumbers) 10
        ]


numberNextToSymbolSuite : Test
numberNextToSymbolSuite =
    describe "numberNextToSymbol"
        [ test "should find a symbol bottom right" <|
            \_ ->
                let
                    -- 467..114..
                    -- ...*......
                    nextTo =
                        numberNextToSymbol
                            { rowIndex = 0
                            , startIndex = 0
                            , endIndex = 2
                            , value = 467
                            }
                            { rowIndex = 1
                            , index = 3
                            , value = '*'
                            }
                in
                Expect.equal nextTo True
        , test "should find symbol top right" <|
            \_ ->
                let
                    -- ...*......
                    -- ..35..633.
                    nextTo =
                        numberNextToSymbol
                            { rowIndex = 1
                            , startIndex = 2
                            , endIndex = 3
                            , value = 35
                            }
                            { rowIndex = 0
                            , index = 3
                            , value = '*'
                            }
                in
                Expect.equal nextTo True
        , test "should not find symbol top left" <|
            \_ ->
                let
                    -- ...*......
                    -- ..35..633.
                    nextTo =
                        numberNextToSymbol
                            { rowIndex = 1
                            , startIndex = 6
                            , endIndex = 8
                            , value = 633
                            }
                            { rowIndex = 0
                            , index = 3
                            , value = '*'
                            }
                in
                Expect.equal nextTo False
        ]
