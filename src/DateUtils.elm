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
                (Date.day date |> toString)
                    ++ " "
                    ++ (Date.month date |> toString)
                    ++ ", "
                    ++ (Date.hour date |> toString)
                    ++ ":"
                    ++ (Date.minute date |> toString)

            Err err ->
                err
