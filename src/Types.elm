module Types
    exposing
        ( Token
        , Status(..)
        )


type Status e
    = Initial
    | Trying
    | Error e


type alias Token =
    { value : String }
