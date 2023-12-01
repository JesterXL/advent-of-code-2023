module Day1Test exposing (..)

import Day1 exposing (getCalibration, puzzleInput, sampleInput)
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
        ]
