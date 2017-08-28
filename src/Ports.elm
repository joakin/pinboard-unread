port module Ports exposing (..)

import Json.Encode as Json


port ready : () -> Cmd msg


port save : ( String, String, Json.Value ) -> Cmd msg


port logOut : () -> Cmd msg
