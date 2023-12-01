module Day1Test exposing (..)

import Day1 exposing (getCalibration, numberWordsStringToNumbers, puzzleInput, sampleInput)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, floatRange, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "2023 - Day 1, Part 1"
        [ describe "getCalibration"
            [ test "getCalibration should use puzzle example" <|
                \_ ->
                    let
                        calibration =
                            getCalibration sampleInput
                    in
                    Expect.equal calibration 142
            , test "getCalibration should use puzzle input" <|
                \_ ->
                    let
                        calibration =
                            getCalibration puzzleInput
                    in
                    Expect.equal calibration 54927
            ]
        , describe "numberWordsStringToNumbers"
            [ test "numberWordsStringToNumbers should parse basic input two1nine" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "two1nine") 29
            , test "numberWordsStringToNumbers should parse basic input eightwothree" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "eightwothree") 83
            , test "numberWordsStringToNumbers should parse basic input abcone2threexyz" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "abcone2threexyz") 13
            , test "numberWordsStringToNumbers should parse basic input xtwone3four" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "xtwone3four") 24
            , test "numberWordsStringToNumbers should parse basic input 4nineeightseven2" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "4nineeightseven2") 42
            , test "numberWordsStringToNumbers should parse basic input zoneight234" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "zoneight234") 14
            , test "numberWordsStringToNumbers should parse basic input 7pqrstsixteen" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "7pqrstsixteen") 76
            ]
        ]
