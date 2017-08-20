module Bookmarks exposing (..)

import Tags exposing (Tags)


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


type Filter
    = Unfiltered
    | Tags Tags


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
