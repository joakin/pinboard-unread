module Views exposing (..)

import Html exposing (Html, a, span, text, i)
import Html.Attributes exposing (class, style, classList, title)
import Html.Events exposing (onClick)
import Util exposing ((=>))


type alias TagOptions msg =
    { selected : Bool
    , onClick : String -> msg
    }


tag : TagOptions msg -> String -> Html msg
tag options txt =
    span
        [ classList [ "tag" => True, "selected" => options.selected ]
        , onClick (options.onClick txt)
        ]
        [ text txt ]


info : String -> Html msg
info txt =
    span [ class "info" ] [ text txt ]


loadingIcon : () -> Html msg
loadingIcon _ =
    iconClassed "animated infinite rotate" "loop"


deleteBtn : msg -> Html msg
deleteBtn msg =
    a [ onClick msg, title "Delete" ] [ icon "delete_forever" ]


markReadBtn : msg -> Html msg
markReadBtn msg =
    a [ onClick msg, title "Mark as read" ] [ icon "visibility_off" ]


editBtn : msg -> Html msg
editBtn msg =
    a [ onClick msg, title "Edit" ] [ icon "edit" ]


okBtn : msg -> Html msg
okBtn click =
    a [ onClick click ] [ icon "done" ]


notOkBtn : String -> Html msg
notOkBtn title_ =
    a [ title title_ ] [ icon "error" ]


rightChevronBtn : Bool -> msg -> Html msg
rightChevronBtn expanded msg =
    a
        [ onClick msg ]
        [ icon <|
            if expanded then
                "expand_less"
            else
                "expand_more"
        ]


icon : String -> Html msg
icon name =
    iconClassed "" name


iconClassed : String -> String -> Html msg
iconClassed cls name =
    i [ class <| "material-icons " ++ cls ] [ text name ]
