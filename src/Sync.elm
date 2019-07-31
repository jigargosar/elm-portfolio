module Sync exposing (SyncMsg(..))

import Todo exposing (TodoId)


type SyncMsg
    = TodoSync TodoId Todo.Msg
