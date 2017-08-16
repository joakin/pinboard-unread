module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set exposing (Set)


---- MODEL ----


type alias Model =
    { unread : List Bookmark
    , tags : Set String
    , filter : Filter
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


type alias Flags =
    { unread : List BookmarkJSON
    }


untaggedTag : String
untaggedTag =
    "[no tags]"


init : Flags -> ( Model, Cmd Msg )
init { unread } =
    let
        bookmarks =
            List.map bookmarkFromJSON unread

        tags =
            List.foldl
                (\b s -> Set.union s (tagsSetFromBookmark b))
                Set.empty
                bookmarks
    in
        ( { unread = bookmarks
          , tags = tags
          , filter = Unfiltered
          }
        , Cmd.none
        )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TagSelected t ->
            ( { model | filter = updateFilter t model.filter }, Cmd.none )


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
view { unread, tags, filter } =
    let
        total =
            List.length unread |> toString

        filteredUnread =
            List.filter (filterBookmark filter) unread

        filteredTotal =
            List.length filteredUnread |> toString
    in
        div [ class "app" ]
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
