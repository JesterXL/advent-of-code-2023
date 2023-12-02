module Day2 exposing (parseGame)


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
