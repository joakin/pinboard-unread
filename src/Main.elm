module Main exposing (..)

import Html exposing (Html, div, header, h1, text, section, span)
import Html.Attributes exposing (class)
import Ports
import Types
import Pages.Login as Login
import Pages.Unread as Unread
import Tuple2 as T
import Util exposing ((=>))


---- MODEL ----


type alias Model =
    Page


type Page
    = LoginPage Login.Data
    | UnreadPage Unread.Data


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
        |> T.mapSecond (\cmds -> Cmd.batch [ cmds, Ports.ready () ])



---- UPDATE ----


type Msg
    = LoginMsg Login.Msg
    | UnreadMsg Unread.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
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

        ( action, _ ) ->
            let
                _ =
                    Debug.log "SKIPPED ACTION" action
            in
                model => Cmd.none



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ header [ class "app-header" ]
            [ div [ class "app-header-content" ]
                [ h1 [ class "app-header-title" ]
                    [ span [ class "app-header-pin" ] [ text "📌" ]
                    , text "Pinboard unread"
                    ]
                ]
            ]
        , section [ class "app-body" ]
            [ case model of
                LoginPage data ->
                    Html.map LoginMsg <| Login.viewLogin data

                UnreadPage data ->
                    Html.map UnreadMsg <| Unread.viewBookmarks data
            ]
        ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
