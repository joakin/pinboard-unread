port module Ports exposing (..)

import Bookmarks exposing (BookmarkJSON)


port save : ( String, String, List BookmarkJSON ) -> Cmd msg


port saveLastUpdateTime : String -> Cmd msg


port logOut : () -> Cmd msg
