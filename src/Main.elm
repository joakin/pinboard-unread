module Main exposing (..)

import Util exposing ((=>))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Net exposing (httpErrorToString, FetchBookmarksError(..))
import Http
import Task exposing (Task)
import Bookmarks exposing (Bookmark, BookmarkJSON, viewBookmark)
import Tags exposing (Tags, Filter(..), viewTags, viewTag)
import Ports exposing (save, logOut)
import DateUtils exposing (formatDate)
import Views exposing (info, tag, loadingIcon, okBtn, notOkBtn)
import Types exposing (Token, Status(..), DataJSON)
import Pages.Login as Login exposing (LoginData)
import Pages.Unread as Unread exposing (Data)
import Tuple2 as T


---- MODEL ----


type alias Model =
    Page


type Page
    = NoAuth LoginData
    | Auth Data


type alias Flags =
    { data : Maybe DataJSON }


init : Flags -> ( Model, Cmd Msg )
init { data } =
    case data of
        Just d ->
            Unread.initWithJSON d UnreadBookmarksResponse
                |> T.mapFirst Auth

        Nothing ->
            NoAuth Login.initEmpty ! []



---- UPDATE ----


type Msg
    = TagSelected String
    | FormTokenInput String
    | FormTokenSubmit
    | UnreadBookmarksResponse (Result FetchBookmarksError ( String, List BookmarkJSON ))
    | FetchBookmarks
    | DeleteBookmark String
    | DeleteBookmarkResponse String (Result Http.Error (Result String ()))
    | LogOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( FormTokenInput txt, NoAuth loginData ) ->
            NoAuth (Login.updateTokenInput txt loginData) ! []

        ( FormTokenSubmit, NoAuth loginData ) ->
            Login.updateFormSubmit UnreadBookmarksResponse loginData
                |> T.mapFirst NoAuth

        ( UnreadBookmarksResponse (Ok ( updateTime, bookmarks )), NoAuth loginData ) ->
            -- Tried token on auth and it worked fine
            Unread.initWithJSON
                { token = loginData.tokenInput.value
                , lastUpdateTime = updateTime
                , unread = Just bookmarks
                }
                UnreadBookmarksResponse
                |> T.mapFirst Auth
                |> T.mapSecond
                    (\cmds ->
                        Cmd.batch
                            [ cmds
                            , save ( loginData.tokenInput.value, updateTime, bookmarks )
                            ]
                    )

        ( UnreadBookmarksResponse (Err err), NoAuth loginData ) ->
            NoAuth { loginData | status = Error err } ! []

        ( UnreadBookmarksResponse (Ok ( updateTime, bookmarks )), Auth data ) ->
            Auth
                (Unread.dataWithBookmarksJSON bookmarks
                    { data
                        | lastUpdateTime = updateTime
                        , status = Initial
                    }
                )
                ! [ save ( data.token.value, updateTime, bookmarks ) ]

        ( UnreadBookmarksResponse (Err err), Auth data ) ->
            Auth
                { data | status = Error err }
                ! []

        ( TagSelected t, Auth data ) ->
            Auth { data | filter = updateFilter t data.filter } ! []

        ( LogOut, Auth data ) ->
            NoAuth Login.initEmpty ! [ logOut () ]

        ( FetchBookmarks, Auth data ) ->
            Unread.updateUnreadBookmarks UnreadBookmarksResponse data
                |> T.mapFirst Auth

        ( DeleteBookmark url, Auth data ) ->
            model
                ! [ Net.deleteBookmark data.token.value url
                        |> Http.send (DeleteBookmarkResponse url)
                  ]

        ( DeleteBookmarkResponse url (Ok _), Auth data ) ->
            -- It doesn't matter if the API responded OK or not. If it was OK,
            -- then it removed it, if it was not, then the URL didn't exist on
            -- the bookmarks, so we remove it anyways from the model.
            Auth (removeBookmark url data) ! []

        ( DeleteBookmarkResponse url (Err err), Auth data ) ->
            -- If the request failed don't do anything for now
            -- TODO: Have a deleteStatus: Status per bookmark to indicate errors?
            -- Or toasts?
            model ! []

        ( action, _ ) ->
            let
                _ =
                    Debug.log "SKIPPED ACTION" action
            in
                model ! []


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


updateFilter : String -> Filter -> Filter
updateFilter tag filter =
    case filter of
        Unfiltered ->
            Tags (Tags.add tag Tags.empty)

        Tags tags ->
            case Tags.isMember tag tags of
                True ->
                    let
                        newTags =
                            Tags.remove tag tags
                    in
                        if Tags.isEmpty newTags then
                            Unfiltered
                        else
                            Tags newTags

                False ->
                    Tags (Tags.add tag tags)



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ header []
            [ h1 [] [ text "Pinboard unread 📌" ]
            ]
        , case model of
            NoAuth data ->
                viewLogin data

            Auth data ->
                viewBookmarks data
        ]


viewLogin : LoginData -> Html Msg
viewLogin data =
    let
        showLoading =
            data.status == Trying

        ( hasError, error ) =
            case data.status of
                Error err ->
                    let
                        msg =
                            case err of
                                UpdateSkippedError str ->
                                    Debug.crash "Got a UpdateSkippedError in Login view where there is no lastUpdateTime"

                                HttpError err_ ->
                                    httpErrorToString err_
                    in
                        ( True, msg )

                _ ->
                    ( False, "" )
    in
        div []
            [ h2 [] [ text "Login info" ]
            , p [] [ text "You are not logged in, there is no token stored in this session." ]
            , p [] [ text "Please set a the auth token up to retrieve and manipulate your bookmarks." ]
            , p []
                [ text "You can find your token in your "
                , a
                    [ href "https://pinboard.in/settings/password"
                    , target "_blank"
                    ]
                    [ text "settings page on pinboard.in" ]
                , text "."
                ]
            , Html.form [ class "token-form", onSubmit FormTokenSubmit ]
                [ div
                    [ classList
                        [ "token-form-loading" => True
                        , "hidden" => not showLoading
                        ]
                    ]
                    [ span
                        (if showLoading then
                            [ class <|
                                String.join " "
                                    [ "animated"
                                    , "infinite"
                                    , "pulse"
                                    ]
                            ]
                         else
                            []
                        )
                        [ text "Trying to auth..." ]
                    ]
                , div
                    [ classList
                        [ "token-form-error" => True
                        , "hidden" => not hasError
                        ]
                    ]
                    [ text error ]
                , input
                    [ type_ "password"
                    , placeholder "input your token here!"
                    , class "token-form-input"
                    , classList [ "error" => hasError ]
                    , onInput FormTokenInput
                    ]
                    []
                , input
                    [ type_ "submit"
                    , value "Save"
                    , class "token-form-submit"
                    ]
                    []
                ]
            ]


viewBookmarks : Data -> Html Msg
viewBookmarks { unread, tags, filter, user, lastUpdateTime, status } =
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
                [ h2 [] [ text "Unread bookmarks" ]
                , section [ class "menu-bar" ]
                    [ span [] [ text <| "User: " ++ user ]
                    , a [ onClick LogOut ] [ text "Log out" ]
                    ]
                ]
            ]
                ++ (case unread of
                        Just unreadBookmarks ->
                            [ section [ class "unread-tags" ] <| viewTags filter TagSelected tags
                            , section [ class "stats" ]
                                [ span [ title "Last update date from pinboard.in" ]
                                    [ viewRefresh status
                                    , span [] [ text <| formatDate lastUpdateTime ]
                                    ]
                                , span [] [ text <| filteredTotal ++ " / " ++ total ]
                                ]
                            , section [] <|
                                List.map
                                    (viewBookmark filter
                                        { onDelete = DeleteBookmark
                                        , onTagSelect = TagSelected
                                        }
                                    )
                                    filteredUnread
                            ]

                        Nothing ->
                            [ info "No bookmarks fetched yet." ]
                   )


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


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
