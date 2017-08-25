module Main exposing (..)

import Html exposing (Html, div, header, h1, text)
import Html.Attributes exposing (class)
import Ports
import Types exposing (DataJSON)
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
    { data : Maybe DataJSON }


init : Flags -> ( Model, Cmd Msg )
init { data } =
    case data of
        Just d ->
            Unread.initWithJSONAndFetch d
                |> T.mapFirst UnreadPage
                |> T.mapSecond (Cmd.map UnreadMsg)

        Nothing ->
            LoginPage Login.initEmpty => Cmd.none



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
        [ header []
            [ h1 [] [ text "Pinboard unread 📌" ]
            ]
        , case model of
            LoginPage data ->
                Html.map LoginMsg <| Login.viewLogin data

            UnreadPage data ->
                Html.map UnreadMsg <| Unread.viewBookmarks data
        ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
