module Day2 exposing (Game, filterByCubeThreshold, parseAllGames, parseGame, sampleInput, summarizeGamesUnderThreshold)


parseGame : String -> Game
parseGame gameString =
    Game
        (parseGameID gameString)
        (parseCubeSets gameString)


type alias Game =
    { id : Int
    , sets : List CubeSet
    }


type alias CubeSet =
    { red : Int
    , green : Int
    , blue : Int
    }


parseGameID : String -> Int
parseGameID gameString =
    String.split ":" gameString
        |> List.take 1
        |> List.map
            (\str ->
                String.split " " str
                    |> List.drop 1
                    |> List.head
                    |> Maybe.withDefault "0"
            )
        |> List.filterMap String.toInt
        |> List.head
        |> Maybe.withDefault 0


parseCubeSets : String -> List CubeSet
parseCubeSets gameString =
    String.split ":" gameString
        |> List.drop 1
        |> List.map
            (\str ->
                String.split ";" str
                    |> List.map
                        (\colorsStr ->
                            String.split "," colorsStr
                                |> List.map String.trimLeft
                                |> List.map
                                    (\numberAndColor ->
                                        let
                                            numberAndColorList =
                                                String.split " " numberAndColor

                                            total =
                                                List.head numberAndColorList
                                                    |> Maybe.withDefault "0"
                                                    |> String.toInt
                                                    |> Maybe.withDefault 0

                                            color =
                                                List.drop 1 numberAndColorList
                                                    |> List.head
                                                    |> Maybe.withDefault "unknowncolor"
                                                    |> colorFromString
                                                    |> Maybe.withDefault Red
                                        in
                                        ( total, color )
                                    )
                                |> List.foldl
                                    (\( total, color ) cubeSet ->
                                        case color of
                                            Red ->
                                                { cubeSet | red = total }

                                            Green ->
                                                { cubeSet | green = total }

                                            Blue ->
                                                { cubeSet | blue = total }
                                    )
                                    { red = 0, blue = 0, green = 0 }
                        )
            )
        |> List.foldl (++) []


type Color
    = Red
    | Blue
    | Green


colorFromString : String -> Maybe Color
colorFromString colorString =
    case colorString of
        "red" ->
            Just Red

        "green" ->
            Just Green

        "blue" ->
            Just Blue

        _ ->
            Nothing


sampleInput : String
sampleInput =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""


parseAllGames : String -> List Game
parseAllGames input =
    String.lines input
        |> List.map parseGame


filterByCubeThreshold : Game -> Bool
filterByCubeThreshold game =
    -- only 12 red cubes, 13 green cubes, and 14 blue cubes
    List.all
        (\set ->
            if set.red <= 12 && set.green <= 13 && set.blue <= 14 then
                True

            else
                False
        )
        game.sets


summarizeGamesUnderThreshold : String -> Int
summarizeGamesUnderThreshold input =
    input
        |> parseAllGames
        |> List.filter filterByCubeThreshold
        |> List.map .id
        |> List.sum
