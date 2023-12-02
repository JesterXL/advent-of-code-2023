module Day2 exposing (parseGame)


parseGame : String -> Game
parseGame gameString =
    Game 1 []


type alias Game =
    { id : Int
    , sets : List CubeSet
    }


type alias CubeSet =
    { red : Int
    , green : Int
    , blue : Int
    }
