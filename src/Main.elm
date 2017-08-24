module Main exposing (..)

import Util exposing ((=>))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Net
import Http
import Task exposing (Task)
import Bookmarks exposing (Bookmark, BookmarkJSON, viewBookmark)
import Tags exposing (Tags, Filter(..), viewTags, viewTag)
import Ports exposing (save, logOut)
import Date
import Views exposing (info, tag, loadingIcon, okBtn, notOkBtn)


---- MODEL ----


type alias Model =
    Page


type Page
    = NoAuth LoginData
    | Auth Data


type alias Token =
    { value : String }


type alias LoginData =
    { tokenInput : Token
    , status : Status
    }


type Status
    = Initial
    | Trying
    | Error Error


type Error
    = UpdateSkippedError String
    | HttpError Http.Error


type alias Data =
    { unread : Maybe (List Bookmark)
    , tags : Tags
    , filter : Filter
    , token : Token
    , user : String
    , lastUpdateTime : String
    , status : Status
    }


type alias DataJSON =
    { unread : Maybe (List BookmarkJSON)
    , token : String
    , lastUpdateTime : String
    }


type alias Flags =
    { data : Maybe DataJSON }


init : Flags -> ( Model, Cmd Msg )
init { data } =
    case data of
        Just d ->
            initWithJSON d

        Nothing ->
            initEmpty ! []


initEmpty : Model
initEmpty =
    NoAuth
        { tokenInput = { value = "" }
        , status = Initial
        }


initWithJSON : DataJSON -> ( Model, Cmd Msg )
initWithJSON { token, unread, lastUpdateTime } =
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
        updateUnreadBookmarks data


updateUnreadBookmarks : Data -> ( Model, Cmd Msg )
updateUnreadBookmarks data =
    Auth { data | status = Trying }
        ! [ fetchUnreadBookmarks data.token.value data.lastUpdateTime
                |> Task.attempt UnreadBookmarksResponse
          ]


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



---- UPDATE ----


type Msg
    = TagSelected String
    | FormTokenInput String
    | FormTokenSubmit
    | UnreadBookmarksResponse (Result Error ( String, List BookmarkJSON ))
    | FetchBookmarks
    | DeleteBookmark String
    | DeleteBookmarkResponse String (Result Http.Error (Result String ()))
    | LogOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( FormTokenInput txt, NoAuth loginData ) ->
            NoAuth { loginData | tokenInput = { value = txt } } ! []

        ( FormTokenSubmit, NoAuth loginData ) ->
            if String.isEmpty loginData.tokenInput.value then
                model ! []
            else
                NoAuth { loginData | status = Trying }
                    ! [ fetchUnreadBookmarks loginData.tokenInput.value ""
                            |> Task.attempt UnreadBookmarksResponse
                      ]

        ( UnreadBookmarksResponse (Ok ( updateTime, bookmarks )), NoAuth loginData ) ->
            -- Tried token on auth and it worked fine
            let
                ( mdl, cmds ) =
                    initWithJSON
                        { token = loginData.tokenInput.value
                        , lastUpdateTime = updateTime
                        , unread = Just bookmarks
                        }
            in
                mdl ! [ cmds, save ( loginData.tokenInput.value, updateTime, bookmarks ) ]

        ( UnreadBookmarksResponse (Err err), NoAuth loginData ) ->
            NoAuth { loginData | status = Error err } ! []

        ( UnreadBookmarksResponse (Ok ( updateTime, bookmarks )), Auth data ) ->
            Auth
                (dataWithBookmarksJSON bookmarks
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
            initEmpty ! [ logOut () ]

        ( FetchBookmarks, Auth data ) ->
            updateUnreadBookmarks data

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


fetchUnreadBookmarks : String -> String -> Task Error ( String, List BookmarkJSON )
fetchUnreadBookmarks token lastUpdateTime =
    Http.toTask (Net.lastUpdateTime token)
        |> Task.mapError HttpError
        |> Task.andThen
            (\{ updateTime } ->
                if updateTime /= lastUpdateTime then
                    Net.unreadBookmarks token
                        |> Http.toTask
                        |> Task.mapError HttpError
                        |> Task.map (\bms -> ( updateTime, bms ))
                else
                    Task.fail (UpdateSkippedError "Update time is the same.")
            )


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


viewRefresh : Status -> Html Msg
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


formatDate : String -> String
formatDate dateStr =
    let
        dateResult =
            Date.fromString dateStr
    in
        case dateResult of
            Ok date ->
                (Date.day date |> toString)
                    ++ " "
                    ++ (Date.month date |> toString)
                    ++ ", "
                    -- ++ (Date.year date |> toString)
                    ++ " "
                    ++ (Date.hour date |> toString)
                    ++ ":"
                    ++ (Date.minute date |> toString)

            Err err ->
                err


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


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
