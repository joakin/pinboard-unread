module Pages.Unread
    exposing
        ( Data
        , initWithJSON
        , dataWithBookmarksJSON
        , updateUnreadBookmarks
        )

import Types exposing (Token, Status(..), DataJSON)
import Net exposing (FetchBookmarksError)
import Tags exposing (Tags, Filter(..))
import Bookmarks exposing (BookmarkJSON, Bookmark)
import Task


type alias Data =
    { unread : Maybe (List Bookmark)
    , tags : Tags
    , filter : Filter
    , token : Token
    , user : String
    , lastUpdateTime : String
    , status : Status FetchBookmarksError
    }


type alias OnResponse msg =
    Result FetchBookmarksError ( String, List BookmarkJSON ) -> msg


initWithJSON : DataJSON -> OnResponse msg -> ( Data, Cmd msg )
initWithJSON { token, unread, lastUpdateTime } onResponse =
    let
        processData : Data -> Data
        processData =
            case unread of
                Just unreadBookmarks ->
                    dataWithBookmarksJSON unreadBookmarks

                Nothing ->
                    identity

        data : Data
        data =
            dataWithTokenAndLastUpdate token lastUpdateTime
                |> processData
    in
        updateUnreadBookmarks onResponse data


dataWithBookmarksJSON : List BookmarkJSON -> Data -> Data
dataWithBookmarksJSON unreadBookmarks data =
    let
        bookmarks =
            List.map Bookmarks.fromJSON unreadBookmarks

        tags =
            Bookmarks.tagsFrom bookmarks
    in
        { data
            | unread = Just bookmarks
            , tags = tags
        }


dataWithTokenAndLastUpdate : String -> String -> Data
dataWithTokenAndLastUpdate token lastUpdateTime =
    { unread = Nothing
    , tags = Tags.empty
    , filter = Unfiltered
    , token = { value = token }
    , user =
        token
            |> String.split ":"
            |> List.head
            |> Maybe.withDefault "Unknown user"
    , lastUpdateTime = lastUpdateTime
    , status = Initial
    }


updateUnreadBookmarks : OnResponse msg -> Data -> ( Data, Cmd msg )
updateUnreadBookmarks onResponse data =
    { data | status = Trying }
        ! [ Net.fetchUnreadBookmarks data.token.value data.lastUpdateTime
                |> Task.attempt onResponse
          ]
