module Day2Test exposing (..)

import Array exposing (Array)
import Day2 exposing (Game, filterByCubeThreshold, minimumCubeAmounts, parseAllGames, parseGame, powerOfAllGames, powerOfCubeSet, puzzleInput, sampleInput, summarizeGamesUnderThreshold)
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
        , describe "parseAllGames"
            [ test "should parse all games and find our game 3" <|
                \_ ->
                    let
                        game3 =
                            parseAllGames sampleInput
                                |> Array.fromList
                                |> Array.get 2
                                |> Maybe.withDefault (Game 12 [])
                    in
                    Expect.equal game3.id 3
            , test "should parse all games and find our game 3's green 8" <|
                \_ ->
                    let
                        game3FirstSetGreen =
                            parseAllGames sampleInput
                                |> Array.fromList
                                |> Array.get 2
                                |> Maybe.withDefault (Game 12 [])
                                |> .sets
                                |> List.head
                                |> Maybe.withDefault { red = 0, green = 0, blue = 0 }
                                |> .green
                    in
                    Expect.equal game3FirstSetGreen 8
            ]
        , describe "filterByCubeThreshold"
            [ test "should only find 3 games" <|
                \_ ->
                    let
                        gamesTotal =
                            parseAllGames sampleInput
                                |> List.filter filterByCubeThreshold
                                |> List.length
                    in
                    Expect.equal gamesTotal 3
            ]
        , describe "summarizeGamesUnderThreshold"
            [ test "should find 3 games under threshold matching the 8 in sampleInput" <|
                \_ ->
                    Expect.equal (summarizeGamesUnderThreshold sampleInput) 8
            , test "should find X amount based in puzzleInput" <|
                \_ ->
                    Expect.equal (summarizeGamesUnderThreshold puzzleInput) 2237
            ]
        , describe "minimumCubeAmounts"
            [ test "should get the minimum amount matching the sample input - red" <|
                \_ ->
                    let
                        sets =
                            [ { blue = 3, red = 4, green = 0 }
                            , { red = 1, green = 2, blue = 6 }
                            , { green = 2, red = 0, blue = 0 }
                            ]
                    in
                    Expect.equal (minimumCubeAmounts sets).red 4
            , test "should get the minimum amount matching the sample input - green" <|
                \_ ->
                    let
                        sets =
                            [ { blue = 3, red = 4, green = 0 }
                            , { red = 1, green = 2, blue = 6 }
                            , { green = 2, red = 0, blue = 0 }
                            ]
                    in
                    Expect.equal (minimumCubeAmounts sets).green 2
            , test "should get the minimum amount matching the sample input - blue" <|
                \_ ->
                    let
                        sets =
                            [ { blue = 3, red = 4, green = 0 }
                            , { red = 1, green = 2, blue = 6 }
                            , { green = 2, red = 0, blue = 0 }
                            ]
                    in
                    Expect.equal (minimumCubeAmounts sets).blue 6
            , test "should match game 2 max" <|
                \_ ->
                    let
                        -- 1 red, 3 green, and 4 blue cubes.
                        -- Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
                        sets =
                            [ { blue = 1, red = 0, green = 2 }
                            , { green = 3, blue = 4, red = 1 }
                            , { green = 1, blue = 1, red = 0 }
                            ]
                    in
                    -- Expect.equal (minimumCubeAmounts sets).red 1
                    -- Expect.equal (minimumCubeAmounts sets).green 3
                    Expect.equal (minimumCubeAmounts sets).blue 4
            ]
        , describe "powerOfCubeSet"
            [ test "should match 1st sample" <|
                \_ ->
                    -- 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                    Expect.equal
                        (powerOfCubeSet { red = 4, green = 2, blue = 6 })
                        48
            ]
        , describe "powerOfAllGames"
            [ test "should match total power of all games in sample input" <|
                \_ ->
                    Expect.equal (powerOfAllGames sampleInput) 2286
            ]
        ]
