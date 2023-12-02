module Day2Test exposing (..)

import Day2 exposing (parseGame)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, floatRange, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "2023 - Day 2"
        [ describe "parseGame"
            [ test "getCalibration should use puzzle example" <|
                \_ ->
                    let
                        game =
                            parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
                    in
                    Expect.equal game.id 1
            ]
        ]
