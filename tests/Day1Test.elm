module Day1Test exposing (..)

import Day1 exposing (enhancedSampleInput, getCalibration, getCalibrationEnhanced, numberWordsStringToNumbers, puzzleInput, sampleInput)
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
            [ test "should parse basic input two1nine" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "two1nine") 29
            , test "should parse basic input eightwothree" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "eightwothree") 83
            , test "should parse basic input abcone2threexyz" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "abcone2threexyz") 13
            , test "should parse basic input xtwone3four" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "xtwone3four") 24
            , test "should parse basic input 4nineeightseven2" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "4nineeightseven2") 42
            , test "should parse basic input zoneight234" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "zoneight234") 14
            , test "should parse basic input 7pqrstsixteen" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "7pqrstsixteen") 76
            , test "should work with sample from puzzleInput 1" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "14gxqgqsqqbxfpxnbccjc33eight") 18
            , test "should work with sample from puzzleInput 2" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "mrjstg5onetwoeightgcczx8vgrgl") 58
            , test "should work with sample from puzzleInput 3" <|
                \_ ->
                    Expect.equal (numberWordsStringToNumbers "xqfcdfdsrlhdktdjshllqgqshzmf7hpcdgdfcvntczdxxfqbvz") 77
            ]
        , describe "getCalibrationEnhanced"
            [ test "getCalibrationEnhanced should parse basic input" <|
                \_ ->
                    Expect.equal (getCalibrationEnhanced enhancedSampleInput) 281
            , test "getCalibrationEnhanced should work with part 1 sample input" <|
                \_ ->
                    Expect.equal (getCalibrationEnhanced sampleInput) 142

            -- ,
            --     test "getCalibrationEnhanced should parse puzzle input" <|
            --         \_ ->
            --             Expect.equal (getCalibrationEnhanced puzzleInput) 54663
            ]
        ]
