module Net
    exposing
        ( deleteBookmark
        , fetchUnreadBookmarks
        , httpErrorToString
        , FetchBookmarksError(..)
        )

import Json.Decode as D exposing (Decoder)
import Http exposing (stringBody)
import Util exposing ((=>))
import Bookmarks exposing (Bookmark, decodeBookmarkList, bookmarkToQueryString)
import Task exposing (Task)


type alias UpdateTimeResponse =
    { updateTime : String
    }


decodeUpdateTime : Decoder UpdateTimeResponse
decodeUpdateTime =
    D.map UpdateTimeResponse
        (D.field "update_time" D.string)


type alias ActionResult =
    Result String ()


decodeActionResult : Decoder ActionResult
decodeActionResult =
    D.field "result_code" D.string
        |> D.andThen
            (\resultCode ->
                D.succeed <|
                    case resultCode of
                        "done" ->
                            Ok ()

                        str ->
                            Err str
            )


decodeGetBookmarkResponse : Decoder (List Bookmark)
decodeGetBookmarkResponse =
    D.at [ "posts" ] decodeBookmarkList


lastUpdateTime : String -> Http.Request UpdateTimeResponse
lastUpdateTime token =
    get "posts/update" token [] decodeUpdateTime


unreadBookmarks : String -> Http.Request (List Bookmark)
unreadBookmarks token =
    get "posts/all" token [ "toread" => "yes" ] decodeBookmarkList


deleteBookmark : String -> String -> Http.Request ActionResult
deleteBookmark token uri =
    get "posts/delete" token [ "url" => uri ] decodeActionResult


type FetchBookmarksError
    = UpdateSkippedError String
    | HttpError Http.Error


fetchUnreadBookmarks : String -> String -> Task FetchBookmarksError ( String, List Bookmark )
fetchUnreadBookmarks token lastUpdateTimeString =
    Http.toTask (lastUpdateTime token)
        |> Task.mapError HttpError
        |> Task.andThen
            (\{ updateTime } ->
                if updateTime /= lastUpdateTimeString then
                    unreadBookmarks token
                        |> Http.toTask
                        |> Task.mapError HttpError
                        |> Task.map (\bms -> ( updateTime, bms ))
                else
                    Task.fail (UpdateSkippedError "Update time is the same.")
            )


type UpdateBookmarkError
    = UnexpectedNumberOfBookmarksFound (List Bookmark)
    | UpdateFailed String
    | RequestFailed Http.Error


updateBookmark : String -> String -> Task UpdateBookmarkError Bookmark
updateBookmark token uri =
    Http.toTask (getBookmark token uri)
        |> Task.mapError RequestFailed
        |> Task.andThen
            (\bookmarkList ->
                case bookmarkList of
                    [ bookmark ] ->
                        addBookmark token bookmark
                            |> Http.toTask
                            |> Task.mapError RequestFailed
                            |> Task.andThen
                                (\resp ->
                                    case resp of
                                        Ok () ->
                                            Task.succeed bookmark

                                        Err str ->
                                            Task.fail (UpdateFailed str)
                                )

                    _ ->
                        Task.fail (UnexpectedNumberOfBookmarksFound bookmarkList)
            )


addBookmark : String -> Bookmark -> Http.Request ActionResult
addBookmark token bookmark =
    get "posts/add" token (bookmarkToQueryString bookmark) decodeActionResult


getBookmark : String -> String -> Http.Request (List Bookmark)
getBookmark token uri =
    get "posts/get" token [ "url" => uri ] decodeGetBookmarkResponse



-- HELPERS --


corsProxy : String
corsProxy =
    "https://cors-proxy-mhwanfbyyu.now.sh/"


apiUrl : String
apiUrl =
    corsProxy ++ "https://api.pinboard.in/v1"


get : String -> String -> List ( String, String ) -> Decoder a -> Http.Request a
get path token params decoder =
    let
        allParams =
            [ "auth_token" => token
            , "format" => "json"
            ]
                ++ params

        uri =
            url (apiUrl ++ "/" ++ path) allParams
    in
        Http.get uri decoder


url : String -> List ( String, String ) -> String
url baseUrl args =
    case args of
        [] ->
            baseUrl

        _ ->
            baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    queryEscape key ++ "=" ++ queryEscape value


queryEscape : String -> String
queryEscape string =
    String.join "+" (String.split "%20" (Http.encodeUri string))


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl str ->
            "URL invalid"

        Http.Timeout ->
            "Request timed out. Try again later"

        Http.NetworkError ->
            "Network error. There was a problem with the request"

        Http.BadStatus res ->
            "Request failed with code "
                ++ toString res.status.code
                ++ ", "
                ++ res.status.message
                ++ ". "
                ++ res.body

        Http.BadPayload err res ->
            "Error decoding the response. "
                ++ err
