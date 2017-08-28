module Views exposing (..)

import Html exposing (Html, a, span, text)
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
    span [ class "emoji-icon animated infinite rotate" ] [ text "🌀" ]


deleteBtn : msg -> Html msg
deleteBtn msg =
    a [ class "emoji-icon", onClick msg, title "Delete" ] [ text "✖️" ]


markReadBtn : msg -> Html msg
markReadBtn msg =
    a [ class "emoji-icon", onClick msg, title "Mark as read" ] [ text "️👁" ]


editBtn : msg -> Html msg
editBtn msg =
    a [ class "emoji-icon", onClick msg, title "Edit" ] [ text "✏️" ]


okBtn : msg -> Html msg
okBtn click =
    a [ class "emoji-icon", onClick click ] [ text "✅" ]


notOkBtn : String -> Html msg
notOkBtn title_ =
    a [ class "emoji-icon", title title_ ] [ text "❌" ]


rightChevronBtn : Bool -> msg -> Html msg
rightChevronBtn expanded msg =
    a
        [ class "emoji-icon chevron"
        , style
            [ "transform"
                => (if expanded then
                        "rotate(-90deg)"
                    else
                        "rotate(0deg)"
                   )
            ]
        , onClick msg
        ]
        [ text "◀️" ]
