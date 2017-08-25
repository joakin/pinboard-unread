module Tags exposing (..)

import Set exposing (Set)
import Html exposing (Html)
import Views exposing (tag)
import String


type Filter
    = Unfiltered
    | Tags Tags


updateFilter : String -> Filter -> Filter
updateFilter tag filter =
    case filter of
        Unfiltered ->
            Tags (add tag empty)

        Tags tags ->
            case isMember tag tags of
                True ->
                    let
                        newTags =
                            remove tag tags
                    in
                        if isEmpty newTags then
                            Unfiltered
                        else
                            Tags newTags

                False ->
                    Tags (add tag tags)


type alias Tags =
    Set String


untagged : Tags
untagged =
    add "[no tags]" empty


empty : Tags
empty =
    Set.empty


isEmpty : Tags -> Bool
isEmpty =
    Set.isEmpty


fromString : String -> Tags
fromString tagsStr =
    if String.isEmpty tagsStr then
        untagged
    else
        tagsStr
            |> String.words
            |> Set.fromList


add : String -> Tags -> Tags
add tag tags =
    Set.insert tag tags


isMember : String -> Tags -> Bool
isMember =
    Set.member


any : Tags -> Tags -> Bool
any t1 t2 =
    Set.intersect t1 t2
        |> (not << Set.isEmpty)


merge : Tags -> Tags -> Tags
merge =
    Set.union


remove : String -> Tags -> Tags
remove =
    Set.remove


toList : Tags -> List String
toList =
    Set.toList


toString : Tags -> String
toString tags =
    if tags == untagged then
        ""
    else
        toList tags
            |> String.join " "


viewTags : Filter -> (String -> msg) -> Tags -> List (Html msg)
viewTags filter onTagClick tags =
    let
        tagsList =
            toList tags
    in
        List.map (viewTag filter onTagClick) tagsList


viewTag : Filter -> (String -> msg) -> String -> Html msg
viewTag filter onTagClick t =
    case filter of
        Unfiltered ->
            tag { selected = False, onClick = onTagClick } t

        Tags tags ->
            if isMember t tags then
                tag { selected = True, onClick = onTagClick } t
            else
                tag { selected = False, onClick = onTagClick } t
