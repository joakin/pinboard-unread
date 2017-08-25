module Bookmarks exposing (..)

import Json.Decode as D
import Json.Encode as E
import Tags exposing (Tags, Filter(..))
import Html exposing (Html, div, a, text)
import Html.Attributes exposing (class, href, target, title)
import Views exposing (info, deleteBtn)
import Util exposing ((=>))


type alias Bookmark =
    { description : String
    , extended : String
    , hash : String
    , href : String
    , shared : Bool
    , tags : Tags
    , time : String
    , toread : Bool
    }


type alias BookmarkJSON =
    { description : String
    , extended : String
    , hash : String
    , href : String
    , shared : Bool
    , tags : Tags
    , time : String
    , toread : Bool
    }


decodeBookmarkJSONList : D.Decoder (List BookmarkJSON)
decodeBookmarkJSONList =
    D.list decodeBookmarkJSON


decodeBookmarkJSON : D.Decoder BookmarkJSON
decodeBookmarkJSON =
    D.map8 BookmarkJSON
        (D.field "description" (D.string))
        (D.field "extended" (D.string))
        (D.field "hash" (D.string))
        (D.field "href" (D.string))
        (D.field "shared" (D.map yesToBool (D.string)))
        (D.field "tags" (D.map Tags.fromString (D.string)))
        (D.field "time" (D.string))
        (D.field "toread" (D.map yesToBool (D.string)))


encodeBookmarkJSONList : List BookmarkJSON -> E.Value
encodeBookmarkJSONList list =
    E.list (List.map encodeBookmarkJSON list)


encodeBookmarkJSON : BookmarkJSON -> E.Value
encodeBookmarkJSON b =
    E.object
        [ "description" => E.string b.description
        , "extended" => E.string b.extended
        , "hash" => E.string b.hash
        , "href" => E.string b.href
        , "shared" => E.string (boolToYes b.shared)
        , "tags" => E.string (Tags.toString b.tags)
        , "time" => E.string b.time
        , "toread" => E.string (boolToYes b.toread)
        ]


fromJSON : BookmarkJSON -> Bookmark
fromJSON bj =
    { description = bj.description
    , extended = bj.extended
    , hash = bj.hash
    , href = bj.href
    , shared = bj.shared
    , tags = bj.tags
    , time = bj.time
    , toread = bj.toread
    }


yesToBool : String -> Bool
yesToBool yes =
    yes == "yes"


boolToYes : Bool -> String
boolToYes b =
    if b then
        "yes"
    else
        "no"


filter : Filter -> Bookmark -> Bool
filter f bookmark =
    case f of
        Unfiltered ->
            True

        Tags selectedTags ->
            Tags.any selectedTags bookmark.tags


tagsFrom : List Bookmark -> Tags
tagsFrom bookmarks =
    List.foldl
        (\b s -> Tags.merge s b.tags)
        Tags.empty
        bookmarks


type alias BookmarkOptions msg =
    { onDelete : String -> msg
    , onTagSelect : String -> msg
    }


viewBookmark : Filter -> BookmarkOptions msg -> Bookmark -> Html msg
viewBookmark filter options bookmark =
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
            , div [ class "bookmark-actions" ]
                [ deleteBtn (options.onDelete bookmark.href)
                ]
            ]
        , div [ class "bookmark-separator" ] []
        , div [ class "bookmark-description" ]
            [ if String.isEmpty bookmark.extended then
                info "No description"
              else
                text bookmark.extended
            ]
        , div [ class "bookmark-separator" ] []
        , div [ class "bookmark-footer" ] <|
            if Tags.untagged == bookmark.tags then
                [ info "No tags" ]
            else
                Tags.viewTags filter options.onTagSelect bookmark.tags
        ]
