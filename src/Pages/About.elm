module Pages.About exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


links =
    [ { href = "https://pinboard.in"
      , text = "pinboard.in"
      , label = "An awesome bookmarking service"
      }
    , { href = "https://github.com/joakin/pinboard-unread/"
      , text = "joakin/pinboard-unread"
      , label = "The OSS code repository on Github"
      }
    , { href = "https://material.io/"
      , text = "material.io"
      , label = "Material design guidelines for mobile web design from Google"
      }
    , { href = "http://elm-lang.org/"
      , text = "elm-lang.org"
      , label = "An awesome programming language for web applications"
      }
    , { href = "https://github.com/halfzebra/create-elm-app"
      , text = "create-elm-app"
      , label = "A nice cli tool for bundling elm webapps"
      }
    ]


view : Html msg
view =
    div [ class "content" ]
        [ h2 [] [ text "About \"Pinboard Unread\"" ]
        , p []
            [ text """
Pinboard Unread is a mobile web application made for checking and reading your unread bookmarks from
        """
            , a
                [ href "https://pinboard.in"
                , target "_blank"
                ]
                [ text "pinboard.in" ]
            , text """, which is an awesome bookmarking service."""
            ]
        , p [] [ text "For more information, see:" ]
        , dl [] <|
            List.concatMap
                (\link ->
                    [ dt []
                        [ a [ href link.href, target "_blank" ] [ text link.text ]
                        ]
                    , dd [] [ text link.label ]
                    ]
                )
                links
        ]
