module Day3 exposing (numberNextToSymbol, parsePartNumbersFromRows, parseRow, sampleInput, sumPartNumbers)

import Char exposing (isDigit)
import List.Extra exposing (gatherWith, indexedFoldl)


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
                            , partNumbers = numTracker.partNumbers ++ [ PartNumber rowIndex numTracker.startIndex (index - 1) (String.fromList numTracker.charDigits |> String.toInt |> Maybe.withDefault 0) ]
                          }
                        , syms
                        )

                    else
                        ( numTracker, syms )

                else if numTracker.tracking == True then
                    ( { numTracker
                        | tracking = False
                        , partNumbers =
                            numTracker.partNumbers
                                ++ [ PartNumber
                                        rowIndex
                                        numTracker.startIndex
                                        (index - 1)
                                        (String.fromList numTracker.charDigits |> String.toInt |> Maybe.withDefault 0)
                                   ]
                      }
                    , syms ++ [ Symbol rowIndex index char ]
                    )

                else
                    ( numTracker, syms ++ [ Symbol rowIndex index char ] )
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
    { rowIndex : Int
    , startIndex : Int
    , endIndex : Int
    , value : Int
    }


type alias Symbol =
    { rowIndex : Int
    , index : Int
    , value : Char
    }


parsePartNumbersFromRows : String -> PartNumbersFromRows
parsePartNumbersFromRows input =
    let
        rows =
            String.lines input
                |> List.indexedMap parseRow

        ( allPartNumbers, allSymbols ) =
            List.foldl
                (\row ( parts, syms ) ->
                    ( parts ++ row.partNumbers
                    , syms ++ row.symbols
                    )
                )
                ( [], [] )
                rows

        validPartNumbers =
            List.filter
                (\partNumber ->
                    List.any
                        (\symbol ->
                            numberNextToSymbol partNumber symbol
                        )
                        allSymbols
                )
                allPartNumbers

        validValues =
            List.map .value validPartNumbers

        allValues =
            List.map .value allPartNumbers

        rogueNumbers =
            allValues
                |> List.filter
                    (\value ->
                        List.member value validValues == False
                    )

        -- _ =
        --     Debug.log "validValues" validValues
        -- _ =
        --     Debug.log "rogueNumbers" rogueNumbers
    in
    { partNumbers = validPartNumbers
    , rogueNumbers = rogueNumbers
    }


type alias PartNumbersFromRows =
    { partNumbers : List PartNumber
    , rogueNumbers : List Int
    }


numberNextToSymbol : PartNumber -> Symbol -> Bool
numberNextToSymbol partNumber symbol =
    let
        atLeastOneIsNearby =
            List.range partNumber.startIndex partNumber.endIndex
                |> List.map
                    (\x ->
                        distance ( x, partNumber.rowIndex ) ( symbol.index, symbol.rowIndex )
                    )
                |> List.filter
                    (\value ->
                        value == 1
                    )
                |> List.length
                |> (\length ->
                        length > 0
                   )
    in
    atLeastOneIsNearby


distance : ( Int, Int ) -> ( Int, Int ) -> Int
distance ( x1, y1 ) ( x2, y2 ) =
    ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
        |> toFloat
        |> sqrt
        |> round


sumPartNumbers : String -> Int
sumPartNumbers input =
    let
        result =
            parsePartNumbersFromRows input

        wat =
            result
                |> .partNumbers
                |> List.map .value

        rogue =
            result
                |> .rogueNumbers

        _ =
            Debug.log "parts" wat

        _ =
            Debug.log "rogue" rogue
    in
    parsePartNumbersFromRows input
        |> .partNumbers
        |> List.map .value
        |> List.sum


sampleInput : String
sampleInput =
    """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
