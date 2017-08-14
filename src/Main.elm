module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


---- MODEL ----


type alias Model =
    { unread : List Bookmark }


type alias BookmarkJSON =
    { description : String
    , extended : String
    , hash : String
    , href : String
    , shared : String
    , tags : String
    , time : String
    , toread : String
    }


type alias Bookmark =
    { description : String
    , extended : String
    , href : String
    , tags : List String
    , toread : Bool
    }


type alias Flags =
    { unread : List BookmarkJSON
    }


init : Flags -> ( Model, Cmd Msg )
init { unread } =
    ( { unread = List.map bookmarkFromJSON unread }, Cmd.none )


bookmarkFromJSON : BookmarkJSON -> Bookmark
bookmarkFromJSON bj =
    { description = bj.description
    , extended = bj.extended
    , href = bj.href
    , tags =
        if String.isEmpty bj.tags then
            []
        else
            String.words bj.tags
    , toread = bj.toread == "yes"
    }



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { unread } =
    div [ class "app" ]
        [ h1 [] [ text "Unread bookmarks" ]
        , section [] <| List.map viewBookmark unread
        ]


viewBookmark : Bookmark -> Html Msg
viewBookmark bookmark =
    div [ class "bookmark" ]
        [ div [ class "bookmark-header" ]
            [ a
                [ href bookmark.href
                , target "_blank"
                , title bookmark.description
                , class "bookmark-header-link"
                ]
                [ text
                    bookmark.description
                ]
            ]
        , div [ class "bookmark-separator" ] []
        , div [ class "bookmark-description" ] [ text bookmark.extended ]
        , div [ class "bookmark-separator" ] []
        , div [ class "bookmark-footer" ] <| List.map (\t -> span [] [ text ("[" ++ t ++ "]") ]) bookmark.tags
        ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
