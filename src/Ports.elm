module Ports exposing (cacheModel)

import Json.Encode exposing (Value)


port cacheModel : Value -> Cmd msg
