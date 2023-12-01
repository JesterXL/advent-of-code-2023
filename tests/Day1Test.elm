module Day1Test exposing (..)

import Day1 exposing (getCalibration)
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
                            getCalibration """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""
                    in
                    Expect.equal calibration 142
            ]
        ]
