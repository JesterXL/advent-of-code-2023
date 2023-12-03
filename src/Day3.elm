module Day3 exposing (..)


parseRow : Int -> String -> Row
parseRow rowIndex input =
    { partNumbers = [ { startIndex = 0, endIndex = 2, value = 23 }, { startIndex = 0, endIndex = 2, value = 23 } ]
    , symbols = []
    , rowIndex = rowIndex
    }


type alias Row =
    { partNumbers : List PartNumber
    , symbols : List Symbol
    , rowIndex : Int
    }


type alias PartNumber =
    { startIndex : Int
    , endIndex : Int
    , value : Int
    }


type alias Symbol =
    { index : Int
    , value : String
    }
