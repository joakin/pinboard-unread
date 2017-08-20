module Tags exposing (..)

import Set exposing (Set)


type alias Tags =
    Set String


untagged : String
untagged =
    "[no tags]"


empty : Tags
empty =
    Set.empty


isEmpty : Tags -> Bool
isEmpty =
    Set.isEmpty


add : String -> Tags -> Tags
add tag tags =
    Set.insert tag tags


isMember : String -> Tags -> Bool
isMember =
    Set.member


merge : Tags -> Tags -> Tags
merge =
    Set.union


remove : String -> Tags -> Tags
remove =
    Set.remove


toList : Tags -> List String
toList =
    Set.toList
