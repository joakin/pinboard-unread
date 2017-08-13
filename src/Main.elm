module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


---- MODEL ----


type alias Model =
    { unread : List Bookmark }


type alias Bookmark =
    { description : String
    , extended : String
    , hash : String
    , href : String
    , shared : String
    , tags : String
    , time : String
    , toread : String
    }


type alias Flags =
    { unread : List Bookmark
    }


init : Flags -> ( Model, Cmd Msg )
init { unread } =
    ( { unread = unread }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { unread } =
    div [ class "App" ]
        [ h1 [] [ text "Unread bookmarks" ]

        -- , section [] <| List.map viewBookmark unread
        ]


viewBookmark : Bookmark -> Html Msg
viewBookmark bookmark =
    div []
        [ h3 [] [ text bookmark.description ] ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
