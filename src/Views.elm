module Views exposing (..)

import Html exposing (Html, div, a, span, text, i)
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


upDownChevronBtn : Bool -> msg -> Html msg
upDownChevronBtn expanded msg =
    a
        [ onClick msg
        , class "transition-transform"
        , style <|
            if expanded then
                [ "transform" => "rotateZ(180deg)"
                ]
            else
                []
        ]
        [ icon "expand_more" ]


icon : String -> Html msg
icon name =
    iconClassed "" name


iconClassed : String -> String -> Html msg
iconClassed cls name =
    i [ class <| "material-icons " ++ cls ] [ text name ]


togglable : Bool -> Float -> List (Html msg) -> Html msg
togglable visible estimatedHeight html =
    div
        [ classList
            [ "hidden" => not visible
            , "transition-height" => True
            ]
        , style [ "max-height" => (toString estimatedHeight) ++ "rem" ]
        ]
        html
