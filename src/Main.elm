module Main exposing (..)

import Html exposing (Html, div, header, h1, text)
import Html.Attributes exposing (class)
import Ports exposing (save, logOut)
import Types exposing (DataJSON)
import Pages.Login as Login
import Pages.Unread as Unread
import Tuple2 as T
import Util exposing ((=>))


---- MODEL ----


type alias Model =
    Page


type Page
    = NoAuth Login.Data
    | Auth Unread.Data


type alias Flags =
    { data : Maybe DataJSON }


init : Flags -> ( Model, Cmd Msg )
init { data } =
    case data of
        Just d ->
            Unread.initWithJSONAndFetch d
                |> T.mapFirst Auth
                |> T.mapSecond (Cmd.map UnreadMsg)

        Nothing ->
            NoAuth Login.initEmpty => Cmd.none



---- UPDATE ----


type Msg
    = LoginMsg Login.Msg
    | UnreadMsg Unread.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoginMsg msg, NoAuth data ) ->
            case Login.update msg data of
                ( data, Login.Idle ) ->
                    NoAuth data => Cmd.none

                ( data, Login.Cmd cmd ) ->
                    NoAuth data => (Cmd.map LoginMsg cmd)

                ( _, Login.Success token updateTime bookmarks ) ->
                    Auth
                        (Unread.initWithDecodedBookmarks
                            { token = token
                            , lastUpdateTime = updateTime
                            , unread = Just bookmarks
                            }
                        )
                        => save ( token, updateTime, bookmarks )

        ( UnreadMsg msg, Auth data ) ->
            case Unread.update msg data of
                ( data, Unread.Idle ) ->
                    Auth data => Cmd.none

                ( data, Unread.Cmd cmd ) ->
                    Auth data => Cmd.map UnreadMsg cmd

                ( _, Unread.LogOut ) ->
                    NoAuth Login.initEmpty => logOut ()

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
            NoAuth data ->
                Html.map LoginMsg <| Login.viewLogin data

            Auth data ->
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
