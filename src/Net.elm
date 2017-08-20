module Net
    exposing
        ( UpdateTimeJSON
        , decodeUpdateTime
        , lastUpdateTime
        , unreadBookmarks
        )

import Json.Decode as D exposing (Decoder)
import Http exposing (stringBody)
import Util exposing ((=>))


type alias UpdateTimeJSON =
    { updateTime : String
    }


decodeUpdateTime : Decoder UpdateTimeJSON
decodeUpdateTime =
    D.map UpdateTimeJSON
        (D.field "update_time" D.string)


lastUpdateTime : String -> Http.Request UpdateTimeJSON
lastUpdateTime token =
    get "posts/update" token [] decodeUpdateTime


unreadBookmarks : String -> Http.Request UpdateTimeJSON
unreadBookmarks token =
    get "posts/all" token [ "toread" => "yes" ] decodeUpdateTime



-- HELPERS --


corsProxy : String
corsProxy =
    "https://cors-proxy-mhwanfbyyu.now.sh/"


apiUrl : String
apiUrl =
    corsProxy ++ "https://api.pinboard.in/v1"


get : String -> String -> List ( String, String ) -> Decoder a -> Http.Request a
get path token params decoder =
    let
        allParams =
            [ "auth_token" => token
            , "format" => "json"
            ]
                ++ params

        uri =
            url (apiUrl ++ "/" ++ path) allParams
    in
        Http.get uri decoder


url : String -> List ( String, String ) -> String
url baseUrl args =
    case args of
        [] ->
            baseUrl

        _ ->
            baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    queryEscape key ++ "=" ++ queryEscape value


queryEscape : String -> String
queryEscape string =
    String.join "+" (String.split "%20" (Http.encodeUri string))
