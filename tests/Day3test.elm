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
            ]
        ]
