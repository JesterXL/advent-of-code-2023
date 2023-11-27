module TwoThousandFifteenTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, floatRange, int, list, string)
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (..)
import TwoThousandFifteen exposing (Direction(..), addPresentToHouse, getInitialPresentDelivery, updatePosition)


suite : Test
suite =
    describe "2015 - Day 3, Part 2"
        [ describe "updatePosition"
            [ test "updatePosition should move North correctly" <|
                \_ ->
                    let
                        updatedPosition =
                            updatePosition North { x = 0, y = 0 }
                    in
                    Expect.equal updatedPosition.y -1
            , test "updatePosition should move South correctly" <|
                \_ ->
                    let
                        updatedPosition =
                            updatePosition South { x = 0, y = 0 }
                    in
                    Expect.equal updatedPosition.y 1
            , test "updatePosition should move East correctly" <|
                \_ ->
                    let
                        updatedPosition =
                            updatePosition East { x = 0, y = 0 }
                    in
                    Expect.equal updatedPosition.x 1
            , test "updatePosition should move West correctly" <|
                \_ ->
                    let
                        updatedPosition =
                            updatePosition West { x = 0, y = 0 }
                    in
                    Expect.equal updatedPosition.x -1
            ]
        , describe "addPresentToHouse"
            [ test "should update a house with a present" <|
                \_ ->
                    let
                        presentDelivery =
                            getInitialPresentDelivery

                        updatedHouses =
                            addPresentToHouse presentDelivery.houses { x = 0, y = 0 }

                        house =
                            Dict.get "0_0" updatedHouses
                                |> Maybe.withDefault { point = { x = 0, y = 0 }, presents = 0 }
                    in
                    Expect.equal house.presents 1
            ]
        ]
