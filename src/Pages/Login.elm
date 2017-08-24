module Pages.Login
    exposing
        ( LoginData
        , initEmpty
        , updateTokenInput
        )

import Types exposing (Token, Status(..))
import Net exposing (FetchBookmarksError)


type alias LoginData =
    { tokenInput : Token
    , status : Status FetchBookmarksError
    }


initEmpty : LoginData
initEmpty =
    { tokenInput = { value = "" }
    , status = Initial
    }


updateTokenInput : String -> LoginData -> LoginData
updateTokenInput token data =
    { data | tokenInput = { value = token } }
