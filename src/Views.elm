module Views exposing (..)

import Html exposing (Html, a, span, text)
import Html.Attributes exposing (class, classList, title)
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
    a [ class "emoji-icon", onClick msg ] [ text "✖️" ]


okBtn : msg -> Html msg
okBtn click =
    a [ class "emoji-icon", onClick click ] [ text "✅" ]


notOkBtn : String -> Html msg
notOkBtn title_ =
    a [ class "emoji-icon", title title_ ] [ text "❌" ]
