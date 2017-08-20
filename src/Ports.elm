port module Ports exposing (..)


port saveToken : String -> Cmd msg


port saveLastUpdateTime : String -> Cmd msg


port logOut : () -> Cmd msg
