module Pages.Login
    exposing
        ( Data
        , Msg
        , Response(..)
        , initEmpty
        , update
        , viewLogin
        )

import Types exposing (Token, Status(..))
import Bookmarks exposing (Bookmark)
import Net exposing (FetchBookmarksError(..))
import Task
import Html exposing (Html, div, input, text, h2, p, a, span)
import Html.Attributes exposing (class, classList, placeholder, target, value, type_, href)
import Html.Events exposing (onInput, onSubmit)
import Net exposing (httpErrorToString)
import Util exposing ((=>))
import Views exposing (loadingIcon, togglable)


type alias Data =
    { tokenInput : Token
    , status : Status FetchBookmarksError
    }


initEmpty : Data
initEmpty =
    { tokenInput = { value = "" }
    , status = Initial
    }


type Msg
    = FormTokenInput String
    | FormTokenSubmit
    | UnreadBookmarksResponse (Result FetchBookmarksError ( String, List Bookmark ))


type Response
    = Cmd (Cmd Msg)
    | Success String String (List Bookmark)
    | Idle


update : Msg -> Data -> ( Data, Response )
update msg data =
    case msg of
        FormTokenInput txt ->
            { data | tokenInput = { value = txt } } => Idle

        FormTokenSubmit ->
            if String.isEmpty data.tokenInput.value then
                data => Idle
            else
                { data | status = Trying }
                    => (Net.fetchUnreadBookmarks data.tokenInput.value ""
                            |> Task.attempt UnreadBookmarksResponse
                            |> Cmd
                       )

        UnreadBookmarksResponse (Ok ( updateTime, bookmarks )) ->
            -- Tried token on auth and it worked fine
            data => Success data.tokenInput.value updateTime bookmarks

        UnreadBookmarksResponse (Err err) ->
            { data | status = Error err } => Idle


viewLogin : Data -> Html Msg
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

                                FetchHttpError err_ ->
                                    httpErrorToString err_
                    in
                        ( True, msg )

                _ ->
                    ( False, "" )
    in
        div [ class "content" ]
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
                        [ text "Trying to auth "
                        , loadingIcon ()
                        ]
                    ]
                , togglable hasError
                    3
                    [ div
                        [ class "token-form-error" ]
                        [ text error ]
                    ]
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
