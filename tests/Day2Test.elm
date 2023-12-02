module Day2Test exposing (..)

import Array exposing (Array)
import Day2 exposing (parseGame)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, floatRange, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "2023 - Day 2"
        [ describe "parseGame"
            [ test "parseGame have an id of 1" <|
                \_ ->
                    let
                        game =
                            parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
                    in
                    Expect.equal game.id 1
            , test "parseGame should have a 2nd set of 6 blues" <|
                \_ ->
                    let
                        game =
                            parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

                        secondSet =
                            Array.fromList game.sets
                                |> Array.get 1
                                |> Maybe.withDefault { red = 0, blue = 0, green = 0 }
                    in
                    Expect.equal secondSet.blue 6
            ]
        ]