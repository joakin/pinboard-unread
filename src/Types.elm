module Types
    exposing
        ( Token
        , Status(..)
        , DataJSON
        )

import Json.Decode


type Status e
    = Initial
    | Trying
    | Error e


type alias Token =
    { value : String }


type alias DataJSON =
    { unread : Maybe Json.Decode.Value
    , token : String
    , lastUpdateTime : String
    }
