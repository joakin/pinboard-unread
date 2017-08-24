module Types
    exposing
        ( Token
        , Status(..)
        , DataJSON
        )

import Bookmarks exposing (BookmarkJSON)


type Status e
    = Initial
    | Trying
    | Error e


type alias Token =
    { value : String }


type alias DataJSON =
    { unread : Maybe (List BookmarkJSON)
    , token : String
    , lastUpdateTime : String
    }
