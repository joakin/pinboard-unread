module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set exposing (Set)
import Net
import Http


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
    , tags : Set String
    , filter : Filter
    , token : String
    , lastUpdateTime : String
    }


type Filter
    = Unfiltered
    | Tags (Set String)


type alias BookmarkJSON =
    { description : String
    , extended : String
    , hash : String
    , href : String
    , shared : String
    , tags : String
    , time : String
    , toread : String
    }


type alias Bookmark =
    { description : String
    , extended : String
    , href : String
    , tags : List String
    , toread : Bool
    }


type alias DataJSON =
    { unread : Maybe (List BookmarkJSON)
    , token : String
    , lastUpdateTime : String
    }


type alias Flags =
    { data : Maybe DataJSON }


untaggedTag : String
untaggedTag =
    "[no tags]"


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
                        List.map bookmarkFromJSON unreadBookmarks

                    tags =
                        List.foldl
                            (\b s -> Set.union s (tagsSetFromBookmark b))
                            Set.empty
                            bookmarks
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
    , tags = Set.empty
    , filter = Unfiltered
    , token = token
    , lastUpdateTime = lastUpdateTime
    }


bookmarkFromJSON : BookmarkJSON -> Bookmark
bookmarkFromJSON bj =
    { description = bj.description
    , extended = bj.extended
    , href = bj.href
    , tags =
        if String.isEmpty bj.tags then
            [ untaggedTag ]
        else
            String.words bj.tags
    , toread = bj.toread == "yes"
    }


tagsSetFromBookmark : Bookmark -> Set String
tagsSetFromBookmark b =
    List.foldl (\t s -> Set.insert t s) Set.empty b.tags


filterBookmark : Filter -> Bookmark -> Bool
filterBookmark filter bookmark =
    case filter of
        Unfiltered ->
            True

        Tags tags ->
            List.any (\t -> Set.member t tags) bookmark.tags



---- UPDATE ----


type Msg
    = TagSelected String
    | FormTokenInput String
    | FormTokenSubmit
    | LastUpdateTime (Result Http.Error Net.UpdateTimeJSON)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( FormTokenInput txt, NoAuth loginData ) ->
            ( NoAuth { loginData | tokenInput = txt }, Cmd.none )

        ( FormTokenSubmit, NoAuth loginData ) ->
            if String.isEmpty loginData.tokenInput then
                model ! []
            else
                ( NoAuth { loginData | status = TryingAuth }
                , Http.send LastUpdateTime <| Net.postsUpdate loginData.tokenInput
                )

        -- Tried token on auth and it worked fine
        ( LastUpdateTime (Ok { updateTime }), NoAuth loginData ) ->
            Auth (dataWithTokenAndLastUpdate loginData.tokenInput updateTime)
                ! []

        ( LastUpdateTime (Err err), NoAuth loginData ) ->
            NoAuth { loginData | status = AuthError err } ! []

        ( TagSelected t, Auth data ) ->
            ( Auth { data | filter = updateFilter t data.filter }, Cmd.none )

        ( _, _ ) ->
            model ! []


updateFilter : String -> Filter -> Filter
updateFilter tag filter =
    case filter of
        Unfiltered ->
            Tags (Set.insert tag Set.empty)

        Tags tags ->
            case Set.member tag tags of
                True ->
                    let
                        newTags =
                            Set.remove tag tags
                    in
                        if Set.isEmpty newTags then
                            Unfiltered
                        else
                            Tags newTags

                False ->
                    Tags (Set.insert tag tags)



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
            [ h1 [] [ text "Login info" ]
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
                        [ ( "token-form-loading", True )
                        , ( "hidden", not showLoading )
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
                        [ ( "token-form-error", True )
                        , ( "hidden", not hasError )
                        ]
                    ]
                    [ text error ]
                , input
                    [ type_ "text"
                    , placeholder "input your token here!"
                    , class "token-form-input"
                    , classList [ ( "error", hasError ) ]
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
viewBookmarks { unread, tags, filter } =
    let
        unreadBookmarks =
            Maybe.withDefault [] unread

        total =
            List.length unreadBookmarks |> toString

        filteredUnread =
            List.filter (filterBookmark filter) unreadBookmarks

        filteredTotal =
            List.length filteredUnread |> toString
    in
        div []
            [ h1 [] [ text "Unread bookmarks" ]
            , section [ class "unread-tags" ] <| viewTags filter tags
            , section [ class "stats" ] [ text <| filteredTotal ++ " / " ++ total ]
            , section [] <| List.map (viewBookmark filter) filteredUnread
            ]


viewTags : Filter -> Set String -> List (Html Msg)
viewTags filter tags =
    let
        tagsList =
            Set.toList tags
    in
        List.map (viewTag filter) tagsList


viewTag : Filter -> String -> Html Msg
viewTag filter t =
    case filter of
        Unfiltered ->
            tag { selected = False, onClick = TagSelected } t

        Tags tags ->
            if Set.member t tags then
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
            if (Maybe.withDefault untaggedTag <| List.head bookmark.tags) == untaggedTag then
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
        [ classList [ ( "tag", True ), ( "selected", options.selected ) ]
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



-- Utils


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>
