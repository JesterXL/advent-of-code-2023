module Day1 exposing (getCalibration)


getCalibration : String -> Int
getCalibration puzzleInput =
    String.split "\n" puzzleInput
        |> List.map (\string -> String.filter Char.isDigit string)
        |> List.map
            (\str ->
                if String.length str == 1 then
                    String.append str str

                else
                    str
            )
        |> List.map (\str -> String.left 1 str ++ String.right 1 str)
        |> List.filterMap String.toInt
        |> List.sum
