module Sync exposing (SyncMsg(..), SyncQueue, batchEncoder, emptyQueue, queueDecoder)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
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


type alias SyncQueue =
    List SyncBatch


emptyQueue : SyncQueue
emptyQueue =
    []


encoder : SyncMsg -> Value
encoder sync =
    case sync of
        TodoSync todoId todoMsg ->
            JE.object
                [ ( "type", JE.string "Todo" )
                , ( "id", TodoId.encoder todoId )
                , ( "patch", Todo.msgEncoder todoMsg )
                ]


decoder : Decoder SyncMsg
decoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\type_ ->
                case type_ of
                    "Todo" ->
                        JD.succeed TodoSync
                            |> JDP.required "id" TodoId.decoder
                            |> JDP.required "patch" Todo.msgDecoder

                    _ ->
                        JD.fail ("Invalid SyncMsg type: " ++ type_)
            )


batchEncoder : Millis -> List SyncMsg -> Value
batchEncoder now list =
    JE.object
        [ ( "modifiedAt", JE.int now )
        , ( "list", JE.list encoder list )
        ]


batchDecoder : Decoder SyncBatch
batchDecoder =
    JD.succeed SyncBatch
        |> JDP.required "modifiedAt" JD.int
        |> JDP.required "list" (JD.list decoder)


queueDecoder : Decoder SyncQueue
queueDecoder =
    JD.list batchDecoder
