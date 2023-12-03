module Day3 exposing (parsePartNumbersFromRows, parseRow)

import Char exposing (isDigit)
import List.Extra exposing (indexedFoldl)


parseRow : Int -> String -> Row
parseRow rowIndex input =
    input
        |> String.toList
        |> indexedFoldl
            (\index char ( numTracker, syms ) ->
                if isDigit char then
                    if numTracker.tracking == False then
                        ( { numTracker
                            | tracking = True
                            , startIndex = index
                            , charDigits = [ char ]
                          }
                        , syms
                        )

                    else
                        ( { numTracker
                            | charDigits = numTracker.charDigits ++ [ char ]
                          }
                        , syms
                        )

                else if char == '.' then
                    if numTracker.tracking == True then
                        ( { numTracker
                            | tracking = False
                            , partNumbers = numTracker.partNumbers ++ [ PartNumber numTracker.startIndex (index - 1) (String.fromList numTracker.charDigits |> String.toInt |> Maybe.withDefault 0) ]
                          }
                        , syms
                        )

                    else
                        ( numTracker, syms )

                else if numTracker.tracking == True then
                    ( { numTracker
                        | tracking = False
                        , partNumbers = numTracker.partNumbers ++ [ PartNumber numTracker.startIndex (index - 1) (String.fromList numTracker.charDigits |> String.toInt |> Maybe.withDefault 0) ]
                      }
                    , syms
                    )

                else
                    ( numTracker, syms ++ [ Symbol index char ] )
            )
            ( { tracking = False, partNumbers = [], startIndex = -1, charDigits = [] }, [] )
        |> (\( numTracker, syms ) ->
                { partNumbers = numTracker.partNumbers
                , symbols = syms
                , rowIndex = rowIndex
                }
           )


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
    , value : Char
    }


parsePartNumbersFromRows : String -> PartNumbersFromRows
parsePartNumbersFromRows input =
    let
        rows =
            String.lines input
                |> List.indexedMap parseRow

        partNumbers =
            List.map .partNumbers rows
                |> List.concat

        _ =
            Debug.log "rows" rows
    in
    { rowIndexes = []
    , partNumbers = [ PartNumber 0 0 0, PartNumber 0 0 0, PartNumber 0 0 0 ]
    , rogueNumbers = [ 114 ]
    }


type alias PartNumbersFromRows =
    { rowIndexes : List Int
    , partNumbers : List PartNumber
    , rogueNumbers : List Int
    }
