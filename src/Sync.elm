module Sync exposing (SyncMsg(..), batchEncoder)

import Json.Encode as JE exposing (Value)
import Now exposing (Millis)
import Todo exposing (TodoId)
import TodoId


type SyncMsg
    = TodoSync TodoId Todo.Msg


type alias SyncBatch =
    { modifiedAt : Millis
    , list : List SyncMsg
    }


encoder : SyncMsg -> Value
encoder sync =
    case sync of
        TodoSync todoId todoMsg ->
            JE.object
                [ ( "type", JE.string "Todo" )
                , ( "id", TodoId.encoder todoId )
                , ( "patch", Todo.msgEncoder todoMsg )
                ]


batchEncoder : Millis -> List SyncMsg -> Value
batchEncoder now list =
    JE.object
        [ ( "modifiedAt", JE.int now )
        , ( "list", JE.list encoder list )
        ]
