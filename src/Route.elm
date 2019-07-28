module Route exposing (Route(..), fromUrl, projectUrl)

import Url exposing (Url)
import Url.Builder as B
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string, top)


type Route
    = Default
    | Project String
    | NotFound Url


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Default top
        , map Project (s "project" </> string)
        ]


fromUrl : Url -> Route
fromUrl url =
    parse routeParser url
        |> Maybe.withDefault (NotFound url)


projectUrl : String -> String
projectUrl pid =
    B.absolute [ "project", pid ] []
