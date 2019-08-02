port module Ports exposing (cacheSyncQueue)

import Json.Encode exposing (Value)


port cacheSyncQueue : Value -> Cmd msg
