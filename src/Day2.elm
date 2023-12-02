module Day2 exposing (parseGame)


parseGame : String -> Game
parseGame gameString =
    let
        gameID =
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

        _ =
            Debug.log "gameID" gameID
    in
    Game gameID [ { red = 0, blue = 0, green = 0 }, { red = 0, blue = 6, green = 0 } ]


type alias Game =
    { id : Int
    , sets : List CubeSet
    }


type alias CubeSet =
    { red : Int
    , green : Int
    , blue : Int
    }
