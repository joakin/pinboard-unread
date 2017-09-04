module Pages.Unread
    exposing
        ( Data
        , Msg
        , Response(..)
        , initWithDecodedBookmarksAndSave
        , initWithFlagsAndFetch
        , viewBookmarks
        , update
        )

import Types exposing (Token, Status(..), FlagsData)
import Net exposing (FetchBookmarksError(..))
import Tags exposing (Tags, Filter(..))
import Bookmarks exposing (Bookmark, Bookmark)
import Task
import Http
import Html exposing (Html, div, section, span, text, header, h2, a)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)
import Net exposing (httpErrorToString)
import Views exposing (info, loadingIcon, okBtn, notOkBtn, icon, iconClassed)
import DateUtils exposing (formatDate)
import Util exposing ((=>))
import Ports
import Tuple2 as T
import Json.Decode as D
import Dict exposing (Dict)


type alias Data =
    { options : Dict String BookmarkUIOptions
    , unread : Maybe (List Bookmark)
    , tags : Tags
    , filter : Filter
    , token : Token
    , user : String
    , lastUpdateTime : String
    , status : Status FetchBookmarksError
    }


type alias BookmarkUIOptions =
    { actionsExpanded : Bool }


defaultOptions =
    { actionsExpanded = False }


initWithDecodedBookmarksAndSave : { token : String, lastUpdateTime : String, unread : Maybe (List Bookmark) } -> ( Data, Cmd Msg )
initWithDecodedBookmarksAndSave { token, unread, lastUpdateTime } =
    dataWithTokenAndLastUpdate token lastUpdateTime
        |> (\data ->
                case unread of
                    Just unreadBookmarks ->
                        dataWithBookmarks unreadBookmarks data
                            => save token lastUpdateTime unreadBookmarks

                    Nothing ->
                        data => Cmd.none
           )


initWithFlagsAndFetch : FlagsData -> ( Data, Cmd Msg )
initWithFlagsAndFetch { token, unread, lastUpdateTime } =
    let
        processData : Data -> Data
        processData =
            case unread of
                Just jsonValue ->
                    case D.decodeValue Bookmarks.decodeBookmarkList jsonValue of
                        Ok unreadBookmarks ->
                            dataWithBookmarks unreadBookmarks

                        Err err ->
                            let
                                _ =
                                    Debug.log "Decoding bookmarks failed" err
                            in
                                identity

                Nothing ->
                    identity
    in
        dataWithTokenAndLastUpdate token lastUpdateTime
            |> processData
            |> updateUnreadBookmarks


dataWithBookmarks : List Bookmark -> Data -> Data
dataWithBookmarks bookmarks data =
    let
        tags =
            Bookmarks.tagsFrom bookmarks
    in
        { data
            | unread = Just bookmarks
            , tags = tags
        }


dataWithTokenAndLastUpdate : String -> String -> Data
dataWithTokenAndLastUpdate token lastUpdateTime =
    { options = Dict.empty
    , unread = Nothing
    , tags = Tags.empty
    , filter = Unfiltered
    , token = Token token
    , user =
        token
            |> String.split ":"
            |> List.head
            |> Maybe.withDefault "Unknown user"
    , lastUpdateTime = lastUpdateTime
    , status = Initial
    }


updateUnreadBookmarks : Data -> ( Data, Cmd Msg )
updateUnreadBookmarks data =
    { data | status = Trying }
        => (Net.fetchUnreadBookmarks data.token.value data.lastUpdateTime
                |> Task.attempt UnreadBookmarksResponse
           )


removeBookmark : String -> Data -> Data
removeBookmark url data =
    case data.unread of
        Just bs ->
            let
                bookmarks =
                    List.filter (\b -> b.href /= url) bs

                tags =
                    Bookmarks.tagsFrom bookmarks
            in
                { data
                    | unread = Just bookmarks
                    , tags = tags
                }

        Nothing ->
            data


save : String -> String -> List Bookmark -> Cmd Msg
save token updateTime bookmarks =
    Ports.save
        ( token
        , updateTime
        , (Bookmarks.encodeBookmarkList bookmarks)
        )


type Msg
    = TagSelected String
    | FetchBookmarks
    | UnreadBookmarksResponse (Result FetchBookmarksError ( String, List Bookmark ))
    | ExpandActions String
    | EditBookmark String
    | MarkReadBookmark String
    | DeleteBookmark String
    | DeleteBookmarkResponse String (Result Http.Error (Result String ()))
    | SignOff


type Response
    = Idle
    | Cmd (Cmd Msg)
    | LogOut


update : Msg -> Data -> ( Data, Response )
update msg data =
    case msg of
        UnreadBookmarksResponse (Ok ( updateTime, bookmarks )) ->
            dataWithBookmarks bookmarks
                { data
                    | lastUpdateTime = updateTime
                    , status = Initial
                }
                => Cmd (save data.token.value updateTime bookmarks)

        UnreadBookmarksResponse (Err err) ->
            { data | status = Error err } => Idle

        TagSelected t ->
            { data | filter = Tags.updateFilter t data.filter } => Idle

        SignOff ->
            data => LogOut

        FetchBookmarks ->
            updateUnreadBookmarks data
                |> T.mapSecond Cmd

        DeleteBookmark url ->
            data
                => Cmd
                    (Net.deleteBookmark data.token.value url
                        |> Http.send (DeleteBookmarkResponse url)
                    )

        DeleteBookmarkResponse url (Ok _) ->
            -- It doesn't matter if the API responded OK or not. If it was OK,
            -- then it removed it, if it was not, then the URL didn't exist on
            -- the bookmarks, so we remove it anyways from the model.
            -- We also save the removed bookmark to storage
            let
                newData =
                    removeBookmark url data
            in
                newData
                    => Cmd
                        (save
                            newData.token.value
                            newData.lastUpdateTime
                            (Maybe.withDefault [] newData.unread)
                        )

        DeleteBookmarkResponse url (Err err) ->
            -- If the request failed don't do anything for now
            -- TODO: Have a deleteStatus: Status per bookmark to indicate errors?
            -- Or toasts?
            data => Idle

        EditBookmark url ->
            data => Idle

        MarkReadBookmark url ->
            data => Idle

        ExpandActions url ->
            let
                options =
                    Dict.get url data.options |> Maybe.withDefault defaultOptions

                newOptions =
                    Dict.insert url
                        { options | actionsExpanded = not options.actionsExpanded }
                        data.options
            in
                { data | options = newOptions } => Idle


viewBookmarks : Data -> Html Msg
viewBookmarks { options, unread, tags, filter, user, lastUpdateTime, status } =
    let
        unreadBookmarks =
            Maybe.withDefault [] unread

        total =
            List.length unreadBookmarks |> toString

        filteredUnread =
            List.filter (Bookmarks.filter filter) unreadBookmarks

        filteredTotal =
            List.length filteredUnread |> toString
    in
        div [] <|
            [ header [ class "unread-page-header" ]
                [ section [ class "menu-bar content" ]
                    [ span [ title "Last update date from pinboard.in" ]
                        [ viewRefresh status
                        , span [] [ text <| formatDate lastUpdateTime ]
                        ]
                    , div []
                        [ div [ class "user-badge" ]
                            [ span []
                                [ iconClassed "user-badge-icon" "account_circle"
                                , text user
                                ]
                            , a [ onClick SignOff, title "Log out" ] [ icon "exit_to_app" ]
                            ]
                        ]
                    ]
                , h2 [ class "content" ] [ text "Unread bookmarks" ]
                ]
            ]
                ++ [ div [ class "content" ]
                        (case unread of
                            Just unreadBookmarks ->
                                [ section [ class "unread-tags" ] <|
                                    Tags.viewTags filter TagSelected tags
                                , section [ class "stats" ]
                                    [ span [] []
                                    , span [] [ text <| filteredTotal ++ " / " ++ total ]
                                    ]
                                , section [] <|
                                    List.map
                                        (\b ->
                                            let
                                                bookmarkOptions =
                                                    Dict.get b.href options
                                                        |> Maybe.withDefault defaultOptions
                                            in
                                                Bookmarks.viewBookmark filter
                                                    { actionsExpanded = bookmarkOptions.actionsExpanded
                                                    , onExpandActions = ExpandActions
                                                    , onMarkRead = MarkReadBookmark
                                                    , onEdit = EditBookmark
                                                    , onDelete = DeleteBookmark
                                                    , onTagSelect = TagSelected
                                                    }
                                                    b
                                        )
                                        filteredUnread
                                ]

                            Nothing ->
                                [ info "No bookmarks fetched yet." ]
                        )
                   ]


viewRefresh : Status FetchBookmarksError -> Html Msg
viewRefresh status =
    case status of
        Initial ->
            okBtn FetchBookmarks

        Trying ->
            loadingIcon ()

        Error (UpdateSkippedError err) ->
            okBtn FetchBookmarks

        Error (HttpError err) ->
            notOkBtn <| httpErrorToString err
