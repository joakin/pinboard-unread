module Main exposing (..)

import Html exposing (Html, div, header, h1, text, section, span)
import Html.Attributes exposing (class, classList)
import Ports
import Types
import Pages.Login as Login
import Pages.Unread as Unread
import Pages.About as About
import Tuple as T
import Util exposing ((=>))
import Views exposing (bookmarksBtn, aboutBtn)
import Html.Keyed as Keyed


---- MODEL ----


type alias Model =
    { route : Route
    , state : BookmarksState
    }


type BookmarksState
    = LoginPage Login.Data
    | UnreadPage Unread.Data


type Route
    = About
    | Bookmarks


type alias Flags =
    { data : Maybe Types.FlagsData }


init : Flags -> ( Model, Cmd Msg )
init { data } =
    (case data of
        Just d ->
            Unread.initWithFlagsAndFetch d
                |> T.mapFirst UnreadPage
                |> T.mapSecond (Cmd.map UnreadMsg)

        Nothing ->
            LoginPage Login.initEmpty => Cmd.none
    )
        |> T.mapFirst (\state -> { route = About, state = state })
        |> T.mapSecond (\cmds -> Cmd.batch [ cmds, Ports.ready () ])



---- UPDATE ----


type Msg
    = LoginMsg Login.Msg
    | UnreadMsg Unread.Msg
    | NavigateTo Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ route, state } as model) =
    case ( msg, route ) of
        ( NavigateTo Bookmarks, About ) ->
            { model | route = Bookmarks } => Cmd.none

        ( NavigateTo About, Bookmarks ) ->
            { model | route = About } => Cmd.none

        -- Pass the rest of msgs to update the bookmarks state regardless of
        -- which tab you are seeing, so that background work on the bookmarks
        -- page still gets processed and updates state and the DOM
        ( _, _ ) ->
            updateBookmarksState msg state
                |> T.mapFirst (\state -> { model | state = state })


updateBookmarksState : Msg -> BookmarksState -> ( BookmarksState, Cmd Msg )
updateBookmarksState msg state =
    case ( msg, state ) of
        ( LoginMsg msg, LoginPage data ) ->
            case Login.update msg data of
                ( data, Login.Idle ) ->
                    LoginPage data => Cmd.none

                ( data, Login.Cmd cmd ) ->
                    LoginPage data => (Cmd.map LoginMsg cmd)

                ( _, Login.Success token updateTime bookmarks ) ->
                    (Unread.initWithDecodedBookmarksAndSave
                        { token = token
                        , lastUpdateTime = updateTime
                        , unread = Just bookmarks
                        }
                    )
                        |> T.mapFirst UnreadPage
                        |> T.mapSecond (Cmd.map UnreadMsg)

        ( UnreadMsg msg, UnreadPage data ) ->
            case Unread.update msg data of
                ( data, Unread.Idle ) ->
                    UnreadPage data => Cmd.none

                ( data, Unread.Cmd cmd ) ->
                    UnreadPage data => Cmd.map UnreadMsg cmd

                ( _, Unread.LogOut ) ->
                    LoginPage Login.initEmpty => Ports.logOut ()

        ( action, state ) ->
            let
                _ =
                    Debug.log "SKIPPED ACTION when updating bookmarks state" action
            in
                state => Cmd.none



---- VIEW ----


view : Model -> Html Msg
view { route, state } =
    div [ class "app" ]
        [ header [ class "app-header" ]
            [ div [ class "app-header-content" ]
                [ h1 [ class "app-header-title" ]
                    [ span [ class "app-header-pin" ] [ text "📌" ]
                    , text "Pinboard unread"
                    ]
                ]
            ]
        , Keyed.node "section" [ class "app-body" ] <|
            [ page "about" (route == About) About.view
            , page "login"
                (route == Bookmarks)
                (case state of
                    LoginPage data ->
                        Html.map LoginMsg <| Login.viewLogin data

                    UnreadPage data ->
                        Html.map UnreadMsg <| Unread.view data
                )
            ]
        , section [ class "app-bottom-navigation" ]
            [ section [ class "app-bottom-navigation-content" ]
                [ bookmarksBtn (route == Bookmarks) (NavigateTo Bookmarks)
                , aboutBtn (route == About) (NavigateTo About)
                ]
            ]
        ]


page : a -> Bool -> Html msg -> ( a, Html msg )
page key active html =
    ( key, div [ class "page", classList [ ( "active", active ) ] ] [ html ] )


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
