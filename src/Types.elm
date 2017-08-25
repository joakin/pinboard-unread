module Types
    exposing
        ( Token
        , Status(..)
        , FlagsData
        )

import Json.Decode


type Status e
    = Initial
    | Trying
    | Error e


type alias Token =
    { value : String }


type alias FlagsData =
    { unread : Maybe Json.Decode.Value
    , token : String
    , lastUpdateTime : String
    }
