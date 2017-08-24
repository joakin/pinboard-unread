module Pages.Login
    exposing
        ( LoginData
        , initEmpty
        , updateTokenInput
        , updateFormSubmit
        )

import Types exposing (Token, Status(..))
import Bookmarks exposing (BookmarkJSON)
import Net exposing (FetchBookmarksError)
import Task


type alias LoginData =
    { tokenInput : Token
    , status : Status FetchBookmarksError
    }


type alias OnResponse msg =
    Result FetchBookmarksError ( String, List BookmarkJSON ) -> msg


initEmpty : LoginData
initEmpty =
    { tokenInput = { value = "" }
    , status = Initial
    }


updateTokenInput : String -> LoginData -> LoginData
updateTokenInput token data =
    { data | tokenInput = { value = token } }


updateFormSubmit : OnResponse msg -> LoginData -> ( LoginData, Cmd msg )
updateFormSubmit onResponse data =
    if String.isEmpty data.tokenInput.value then
        data ! []
    else
        { data | status = Trying }
            ! [ Net.fetchUnreadBookmarks data.tokenInput.value ""
                    |> Task.attempt onResponse
              ]
