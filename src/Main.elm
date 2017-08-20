module Main exposing (..)

import Util exposing ((=>))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Net
import Http
import Bookmarks exposing (Bookmark, BookmarkJSON, Filter(..))
import Tags exposing (Tags)
import Ports exposing (saveToken, saveLastUpdateTime, logOut)


---- MODEL ----


type alias Model =
    Status


type Status
    = NoAuth LoginData
    | Auth Data


type alias LoginData =
    { tokenInput : String
    , status : LoginStatus
    }


type LoginStatus
    = LoginForm
    | TryingAuth
    | AuthError Http.Error


type alias Data =
    { unread : Maybe (List Bookmark)
    , tags : Tags
    , filter : Filter
    , token : String
    , user : String
    , lastUpdateTime : String
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
        { tokenInput = ""
        , status = LoginForm
        }


initWithJSON : DataJSON -> ( Model, Cmd Msg )
initWithJSON { token, unread, lastUpdateTime } =
    let
        data =
            dataWithTokenAndLastUpdate token lastUpdateTime
    in
        case unread of
            Just unreadBookmarks ->
                let
                    bookmarks =
                        List.map Bookmarks.fromJSON unreadBookmarks

                    tags =
                        Bookmarks.tagsFrom bookmarks
                in
                    Auth
                        { data
                            | unread = Just bookmarks
                            , tags = tags
                        }
                        ! []

            Nothing ->
                Auth data ! []


dataWithTokenAndLastUpdate : String -> String -> Data
dataWithTokenAndLastUpdate token lastUpdateTime =
    { unread = Nothing
    , tags = Tags.empty
    , filter = Unfiltered
    , token = token
    , user =
        token
            |> String.split ":"
            |> List.head
            |> Maybe.withDefault "Unknown user"
    , lastUpdateTime = lastUpdateTime
    }



---- UPDATE ----


type Msg
    = TagSelected String
    | FormTokenInput String
    | FormTokenSubmit
    | FormTokenResponse (Result Http.Error Net.UpdateTimeJSON)
    | LogOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( FormTokenInput txt, NoAuth loginData ) ->
            NoAuth { loginData | tokenInput = txt } ! []

        ( FormTokenSubmit, NoAuth loginData ) ->
            if String.isEmpty loginData.tokenInput then
                model ! []
            else
                NoAuth { loginData | status = TryingAuth }
                    ! [ Http.send FormTokenResponse <| Net.lastUpdateTime loginData.tokenInput ]

        ( FormTokenResponse (Ok { updateTime }), NoAuth loginData ) ->
            -- Tried token on auth and it worked fine
            Auth (dataWithTokenAndLastUpdate loginData.tokenInput updateTime)
                ! [ saveToken loginData.tokenInput
                  , saveLastUpdateTime updateTime
                  ]

        ( FormTokenResponse (Err err), NoAuth loginData ) ->
            NoAuth { loginData | status = AuthError err } ! []

        -- Authenticated:
        --
        ( TagSelected t, Auth data ) ->
            Auth { data | filter = updateFilter t data.filter } ! []

        ( LogOut, Auth data ) ->
            initEmpty ! [ logOut () ]

        ( _, _ ) ->
            model ! []


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
        [ case model of
            NoAuth data ->
                viewLogin data

            Auth data ->
                viewBookmarks data
        ]


viewLogin : LoginData -> Html Msg
viewLogin data =
    let
        showLoading =
            data.status == TryingAuth

        ( hasError, error ) =
            case data.status of
                AuthError err ->
                    let
                        msg =
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
                    [ type_ "text"
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
viewBookmarks { unread, tags, filter, user } =
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
                            [ section [ class "unread-tags" ] <| viewTags filter tags
                            , section [ class "stats" ] [ text <| filteredTotal ++ " / " ++ total ]
                            , section [] <| List.map (viewBookmark filter) filteredUnread
                            ]

                        Nothing ->
                            [ info "No bookmarks fetched yet." ]
                   )


viewTags : Filter -> Tags -> List (Html Msg)
viewTags filter tags =
    let
        tagsList =
            Tags.toList tags
    in
        List.map (viewTag filter) tagsList


viewTag : Filter -> String -> Html Msg
viewTag filter t =
    case filter of
        Unfiltered ->
            tag { selected = False, onClick = TagSelected } t

        Tags tags ->
            if Tags.isMember t tags then
                tag { selected = True, onClick = TagSelected } t
            else
                tag { selected = False, onClick = TagSelected } t


viewBookmark : Filter -> Bookmark -> Html Msg
viewBookmark filter bookmark =
    div [ class "bookmark" ]
        [ div [ class "bookmark-header" ]
            [ a
                [ href bookmark.href
                , target "_blank"
                , title bookmark.description
                , class "bookmark-header-link"
                ]
                [ text
                    bookmark.description
                ]
            ]
        , div [ class "bookmark-separator" ] []
        , div [ class "bookmark-description" ]
            [ if String.isEmpty bookmark.extended then
                info "No description"
              else
                text bookmark.extended
            ]
        , div [ class "bookmark-separator" ] []
        , div [ class "bookmark-footer" ] <|
            if (Maybe.withDefault Tags.untagged <| List.head bookmark.tags) == Tags.untagged then
                [ info "No tags" ]
            else
                List.map (viewTag filter) bookmark.tags
        ]


info : String -> Html Msg
info txt =
    span [ class "info" ] [ text txt ]


type alias TagOptions =
    { selected : Bool
    , onClick : String -> Msg
    }


tag : TagOptions -> String -> Html Msg
tag options txt =
    span
        [ classList [ "tag" => True, "selected" => options.selected ]
        , onClick (options.onClick txt)
        ]
        [ text txt ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
