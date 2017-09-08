module DateUtils exposing (formatDate)

import Date


{-
   Format a ISO date string to a human readable day/monty hh:mm string
-}


formatDate : String -> String
formatDate dateStr =
    let
        dateResult =
            Date.fromString dateStr
    in
        case dateResult of
            Ok date ->
                (Date.day date |> zeroed 2)
                    ++ " "
                    ++ (Date.month date |> toString)
                    ++ ", "
                    ++ (Date.hour date |> zeroed 2)
                    ++ ":"
                    ++ (Date.minute date |> zeroed 2)

            Err err ->
                err


zeroed : Int -> Int -> String
zeroed width num =
    num
        |> toString
        |> String.padLeft width '0'
