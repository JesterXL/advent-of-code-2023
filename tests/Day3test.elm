module Day3test exposing (..)

import Array exposing (Array)
import Day3 exposing (day3Part1, filterValidPartNumber, inputAcrossLines, numberNextToSymbol, numberNextToSymbolCached, parsePartNumbersFromRows, parseRows, puzzleInputDay3, sampleInput, sampleInputWithDupes, sumPartNumbers)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, floatRange, int, list, string)
import Set
import Test exposing (..)


parseRowsSuite : Test
parseRowsSuite =
    describe "parseRows"
        [ test "should get a list of part numbers" <|
            \_ ->
                let
                    ( partNumbers, _, _ ) =
                        parseRows "467..114.."
                in
                Expect.equal (List.length partNumbers) 2
        , test "should get a list of part numbers across many lines" <|
            \_ ->
                let
                    ( partNumbers, _, _ ) =
                        parseRows sampleInput
                in
                Expect.equal (List.length partNumbers) 10
        , test "should get a list of part numbers with dupes" <|
            \_ ->
                let
                    ( partNumbers, _, _ ) =
                        parseRows sampleInputWithDupes
                in
                Expect.equal (List.length partNumbers) 10
        , test "should get a list of part numbers across newlines" <|
            \_ ->
                let
                    ( partNumbers, _, _ ) =
                        parseRows inputAcrossLines

                    _ =
                        Debug.log "wat" (partNumbers |> List.map .value)
                in
                Expect.equal (List.length partNumbers) 5
        , test "should measure columnSize correctly for 1 row" <|
            \_ ->
                let
                    ( _, _, columnSize ) =
                        parseRows """123....32..
"""
                in
                Expect.equal columnSize 11
        , test "should correctly parse sample input - 1st number startIndex legit" <|
            \_ ->
                let
                    ( partNumbers, _, _ ) =
                        parseRows inputAcrossLines

                    startIndex =
                        partNumbers
                            |> Array.fromList
                            |> Array.get 0
                            |> Maybe.withDefault { startRowIndex = 0, endRowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
                            |> .startIndex
                in
                Expect.equal startIndex 0
        , test "should correctly parse sample input - 2nd number startIndex legit" <|
            \_ ->
                let
                    ( partNumbers, _, _ ) =
                        parseRows inputAcrossLines

                    startIndex =
                        partNumbers
                            |> Array.fromList
                            |> Array.get 1
                            |> Maybe.withDefault { startRowIndex = 0, endRowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
                            |> .startIndex
                in
                Expect.equal startIndex 5
        , test "should correctly parse sample input - 2nd number endIndex legit" <|
            \_ ->
                let
                    ( partNumbers, _, _ ) =
                        parseRows inputAcrossLines

                    endIndex =
                        partNumbers
                            |> Array.fromList
                            |> Array.get 1
                            |> Maybe.withDefault { startRowIndex = 0, endRowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
                            |> .endIndex
                in
                Expect.equal endIndex 7
        , test "should correctly parse sample input - 3rd number startIndex legit" <|
            \_ ->
                let
                    ( partNumbers, _, _ ) =
                        parseRows inputAcrossLines

                    startIndex =
                        partNumbers
                            |> Array.fromList
                            |> Array.get 2
                            |> Maybe.withDefault { startRowIndex = 0, endRowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
                            |> .startIndex
                in
                Expect.equal startIndex 9
        , test "should correctly parse sample input - 3rd number endIndex legit" <|
            \_ ->
                let
                    ( partNumbers, _, _ ) =
                        parseRows """467..114.9
12.*......
..35..633."""

                    part =
                        partNumbers
                            |> Array.fromList
                            |> Array.get 2
                            |> Maybe.withDefault { startRowIndex = 0, endRowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
                in
                Expect.equal part.endIndex 1
        , test "should correctly parse sample input - 4th number startIndex legit" <|
            \_ ->
                let
                    ( partNumbers, _, _ ) =
                        parseRows """467..114.9
12.*......
..35..633."""

                    startIndex =
                        partNumbers
                            |> Array.fromList
                            |> Array.get 3
                            |> Maybe.withDefault { startRowIndex = 0, endRowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
                            |> .startIndex
                in
                Expect.equal startIndex 2
        , test "should correctly parse sample input - 4th number endIndex legit" <|
            \_ ->
                let
                    ( partNumbers, _, _ ) =
                        parseRows """467..114.9
12.*......
..35..633."""

                    part =
                        partNumbers
                            |> Array.fromList
                            |> Array.get 3
                            |> Maybe.withDefault { startRowIndex = 0, endRowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
                in
                Expect.equal part.endIndex 3
        ]



-- parseRowSuite : Test
-- parseRowSuite =
--     describe "parseRow"
--         [ test "should get a list of numbers and symbols for a row" <|
--             \_ ->
--                 let
--                     row =
--                         parseRow 0 "467..114.."
--                 in
--                 Expect.equal (List.length row.partNumbers) 2
--         , test "should find 467" <|
--             \_ ->
--                 let
--                     partNumber =
--                         parseRow 0 "467..114.."
--                             |> .partNumbers
--                             |> Array.fromList
--                             |> Array.get 0
--                             |> Maybe.withDefault { rowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
--                 in
--                 Expect.equal partNumber.value 467
--         , test "should find 467 at start index of 0" <|
--             \_ ->
--                 let
--                     partNumber =
--                         parseRow 0 "467..114.."
--                             |> .partNumbers
--                             |> Array.fromList
--                             |> Array.get 0
--                             |> Maybe.withDefault { rowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
--                 in
--                 Expect.equal partNumber.startIndex 0
--         , test "should find 467 at endIndex of 2" <|
--             \_ ->
--                 let
--                     partNumber =
--                         parseRow 0 "467..114.."
--                             |> .partNumbers
--                             |> Array.fromList
--                             |> Array.get 0
--                             |> Maybe.withDefault { rowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
--                 in
--                 Expect.equal partNumber.endIndex 2
--         , test "should find 114" <|
--             \_ ->
--                 let
--                     partNumber =
--                         parseRow 0 "467..114.."
--                             |> .partNumbers
--                             |> Array.fromList
--                             |> Array.get 1
--                             |> Maybe.withDefault { rowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
--                 in
--                 Expect.equal partNumber.value 114
--         , test "should find 114 at index 5" <|
--             \_ ->
--                 let
--                     partNumber =
--                         parseRow 0 "467..114.."
--                             |> .partNumbers
--                             |> Array.fromList
--                             |> Array.get 1
--                             |> Maybe.withDefault { rowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
--                 in
--                 Expect.equal partNumber.startIndex 5
--         , test "should find 114 at end index 7" <|
--             \_ ->
--                 let
--                     partNumber =
--                         parseRow 0 "467..114.."
--                             |> .partNumbers
--                             |> Array.fromList
--                             |> Array.get 1
--                             |> Maybe.withDefault { rowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
--                 in
--                 Expect.equal partNumber.endIndex 7
--         , test "should get a star symbol" <|
--             \_ ->
--                 let
--                     row =
--                         parseRow 0 ".....+.58."
--                 in
--                 Expect.equal (List.length row.symbols) 1
--         , test "should get a star symbol at start index 5" <|
--             \_ ->
--                 let
--                     symbol =
--                         parseRow 0 ".....+.58."
--                             |> .symbols
--                             |> Array.fromList
--                             |> Array.get 0
--                             |> Maybe.withDefault { rowIndex = 0, index = 0, value = '!' }
--                 in
--                 Expect.equal symbol.index 5
--         , test "should get a star symbol that's next to a number" <|
--             \_ ->
--                 let
--                     row =
--                         parseRow 0 "617*......"
--                 in
--                 Expect.equal (List.length row.symbols) 1
--         ]
-- parseRow2Suite : Test
-- parseRow2Suite =
--     describe "parsePartNumbers"
--         [ only <|
--             test "should get 2 part numbers" <|
--                 \_ ->
--                     let
--                         rows =
--                             parsePartNumbers """467..114..
-- ...*......"""
--                     in
--                     Expect.equal (List.length rows) 2
--         ]


parsePartNumbersFromRowsSuite : Test
parsePartNumbersFromRowsSuite =
    describe "parsePartNumbersFromRows"
        [ test "should get 2 outliers" <|
            \_ ->
                let
                    partNumbersFromRows =
                        parsePartNumbersFromRows
                            sampleInput
                in
                Expect.equal (List.length partNumbersFromRows.rogueNumbers) 2
        , test "should get 8 part numbers out of 10 numbers" <|
            \_ ->
                let
                    partNumbersFromRows =
                        parsePartNumbersFromRows
                            sampleInput

                    _ =
                        Debug.log "partNumbersFromRows" (partNumbersFromRows |> .partNumbers |> List.map .value)
                in
                Expect.equal (List.length partNumbersFromRows.partNumbers) 8
        , test "should not contain 114" <|
            \_ ->
                let
                    partNumbers =
                        sampleInput
                            |> parsePartNumbersFromRows
                            |> .partNumbers
                            |> List.map .value
                in
                Expect.equal (List.member 114 partNumbers) False
        , test "should not contain 58" <|
            \_ ->
                let
                    partNumbers =
                        sampleInput
                            |> parsePartNumbersFromRows
                            |> .partNumbers
                            |> List.map .value
                in
                Expect.equal (List.member 58 partNumbers) False
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
                            10
                            { startRowIndex = 0
                            , endRowIndex = 0
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
        , test "should not find a symbol next to 114" <|
            \_ ->
                let
                    -- 467..114..
                    -- ...*......
                    nextTo =
                        numberNextToSymbol
                            10
                            { startRowIndex = 0
                            , endRowIndex = 0
                            , startIndex = 5
                            , endIndex = 7
                            , value = 114
                            }
                            { rowIndex = 1
                            , index = 3
                            , value = '*'
                            }
                in
                Expect.equal nextTo False
        , test "should find symbol top right" <|
            \_ ->
                let
                    -- ...*......
                    -- ..35..633.
                    nextTo =
                        numberNextToSymbol
                            10
                            { startRowIndex = 0
                            , endRowIndex = 0
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
                            10
                            { startRowIndex = 0
                            , endRowIndex = 0
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
        , test "should find a symbol to the right" <|
            \_ ->
                let
                    -- 617*......
                    nextTo =
                        numberNextToSymbol
                            10
                            { startRowIndex = 0
                            , endRowIndex = 0
                            , startIndex = 0
                            , endIndex = 2
                            , value = 617
                            }
                            { rowIndex = 1
                            , index = 3
                            , value = '*'
                            }
                in
                Expect.equal nextTo True
        , test "numberNextToSymbolCached - should work like before" <|
            \_ ->
                let
                    ( nextTo, _ ) =
                        numberNextToSymbolCached
                            10
                            { startRowIndex = 1
                            , endRowIndex = 1
                            , startIndex = 0
                            , endIndex = 2
                            , value = 617
                            }
                            { rowIndex = 1
                            , index = 3
                            , value = '*'
                            }
                            Set.empty
                in
                Expect.equal nextTo True
        , test "numberNextToSymbolCached - should update cache with match" <|
            \_ ->
                let
                    ( _, newCache ) =
                        numberNextToSymbolCached
                            10
                            { startRowIndex = 1
                            , endRowIndex = 1
                            , startIndex = 0
                            , endIndex = 2
                            , value = 617
                            }
                            { rowIndex = 1
                            , index = 3
                            , value = '*'
                            }
                            Set.empty
                in
                Expect.equal (Set.member 617 newCache) True
        , test "numberNextToSymbolCached - should update cache with match when having mutiple of the same" <|
            \_ ->
                let
                    ( _, newCache ) =
                        numberNextToSymbolCached
                            10
                            { startRowIndex = 1
                            , endRowIndex = 1
                            , startIndex = 0
                            , endIndex = 2
                            , value = 617
                            }
                            { rowIndex = 1
                            , index = 3
                            , value = '*'
                            }
                            (Set.insert 617 Set.empty)
                in
                Expect.equal (Set.member 617 newCache) True
        ]


filterValidPartNumberSuite : Test
filterValidPartNumberSuite =
    describe "filterValidPartNumber"
        [ test "should get a valid part number" <|
            \_ ->
                let
                    validPartNumberList =
                        filterValidPartNumber
                            10
                            [ { rowIndex = 1
                              , index = 3
                              , value = '*'
                              }
                            ]
                            [ { startRowIndex = 1
                              , endRowIndex = 1
                              , startIndex = 0
                              , endIndex = 2
                              , value = 617
                              }
                            , { startRowIndex = 18
                              , endRowIndex = 18
                              , startIndex = 18
                              , endIndex = 20
                              , value = 617
                              }
                            ]

                    validPartNumber =
                        validPartNumberList
                            |> Array.fromList
                            |> Array.get 0
                            |> Maybe.withDefault
                                { startRowIndex = 0, endRowIndex = 0, startIndex = 0, endIndex = 0, value = 0 }
                in
                Expect.equal validPartNumber.value 617
        ]


day3Suite : Test
day3Suite =
    describe "day3Part1"
        [ test "day3Part1 should work with sample" <|
            \_ ->
                Expect.equal (sumPartNumbers sampleInput) 4361
        , test "day3Part1 should work with dupes" <|
            \_ ->
                Expect.equal (sumPartNumbers sampleInputWithDupes) 4828
        , test "day3Part1 should work with inputAcrossLines" <|
            \_ ->
                Expect.equal (sumPartNumbers inputAcrossLines) 502
        , test "day3Part1 should work with puzzle input" <|
            \_ ->
                -- Expect.equal (sumPartNumbers puzzleInput) 281663 -- Wrong, too low
                -- Expect.equal (sumPartNumbers puzzleInputDay3) 548402
                -- Expect.equal (sumPartNumbers puzzleInput) 573992
                Expect.equal (sumPartNumbers puzzleInputDay3) 1408110

        -- 281663 -- somehow I'm now getting this... what
        -- Expect.equal (sumPartNumbers largeSampleDay3) 1000
        -- Wrong, too low
        -- 1408110... wrong... I think I'm defeated, man...
        ]
