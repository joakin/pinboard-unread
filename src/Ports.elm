port module Ports exposing (..)

import Json.Encode as Json


port save : ( String, String, Json.Value ) -> Cmd msg


port logOut : () -> Cmd msg
