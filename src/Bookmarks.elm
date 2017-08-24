module Bookmarks exposing (..)

import Json.Decode as D
import Tags exposing (Tags, Filter(..))


type alias Bookmark =
    { description : String
    , extended : String
    , href : String
    , tags : List String
    , toread : Bool
    }


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
        (D.field "shared" (D.string))
        (D.field "tags" (D.string))
        (D.field "time" (D.string))
        (D.field "toread" (D.string))


fromJSON : BookmarkJSON -> Bookmark
fromJSON bj =
    { description = bj.description
    , extended = bj.extended
    , href = bj.href
    , tags =
        if String.isEmpty bj.tags then
            [ Tags.untagged ]
        else
            String.words bj.tags
    , toread = bj.toread == "yes"
    }


filter : Filter -> Bookmark -> Bool
filter f bookmark =
    case f of
        Unfiltered ->
            True

        Tags tags ->
            List.any (\t -> Tags.isMember t tags) bookmark.tags


tagsFrom : List Bookmark -> Tags
tagsFrom bookmarks =
    List.foldl
        (\b s -> Tags.merge s (tagsSetFromBookmark b))
        Tags.empty
        bookmarks


tagsSetFromBookmark : Bookmark -> Tags
tagsSetFromBookmark b =
    List.foldl (\t s -> Tags.add t s) Tags.empty b.tags
