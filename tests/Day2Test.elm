module Day2Test exposing (..)

import Array exposing (Array)
import Day2 exposing (Game, parseAllGames, parseGame, sampleInput)
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
                                |> (\sets ->
                                        let
                                            _ =
                                                Debug.log "sets" sets
                                        in
                                        sets
                                   )
                                |> List.head
                                |> Maybe.withDefault { red = 0, green = 0, blue = 0 }
                                |> .green
                    in
                    Expect.equal game3FirstSetGreen 8
            ]
        ]
