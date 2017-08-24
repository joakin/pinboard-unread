module Views exposing (..)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class, classList)
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
