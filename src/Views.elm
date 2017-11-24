module Views exposing (..)

import Html exposing (Html, div, a, span, text, i, label)
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


markReadBtn : Bool -> msg -> Html msg
markReadBtn unread msg =
    let
        t =
            if unread then
                "Mark as read"
            else
                "Mark as unread"

        i =
            if unread then
                "visibility"
            else
                "visibility_off"
    in
        a [ onClick msg, title t ] [ icon i ]


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


navBtn : { onClick : msg, icon : String, label : String, active : Bool } -> Html msg
navBtn opts =
    a
        [ class "nav-btn"
        , classList [ ( "active", opts.active ) ]
        , onClick opts.onClick
        ]
        [ icon opts.icon
        , label [] [ text opts.label ]
        ]


bookmarksBtn : Bool -> msg -> Html msg
bookmarksBtn active onClick =
    navBtn
        { onClick = onClick
        , icon = "collections_bookmark"
        , label = "Bookmarks"
        , active = active
        }


aboutBtn : Bool -> msg -> Html msg
aboutBtn active onClick =
    navBtn
        { onClick = onClick
        , icon = "info"
        , label = "About"
        , active = active
        }
