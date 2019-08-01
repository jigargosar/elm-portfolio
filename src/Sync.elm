module Sync exposing
    ( Patch(..)
    , SyncQueue
    , appendTodoPatches
    , initialQueue
    , queueDecoder
    , queueEncoder
    )

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Todo exposing (TodoId)


type Patch
    = TodoPatch Todo.Patch


type alias SyncQueue =
    List Patch


initialQueue : SyncQueue
initialQueue =
    []


patchEncoder : Patch -> Value
patchEncoder sync =
    case sync of
        TodoPatch patch ->
            JE.object
                [ ( "kind", JE.string "TodoPatch" )
                , ( "value", Todo.patchEncoder patch )
                ]


patchDecoder : Decoder Patch
patchDecoder =
    JD.oneOf [ Todo.patchDecoder |> JD.map TodoPatch ]


queueDecoder : Decoder SyncQueue
queueDecoder =
    JD.list patchDecoder


queueEncoder : SyncQueue -> Value
queueEncoder =
    JE.list patchEncoder


appendTodoPatches : List Todo.Patch -> SyncQueue -> SyncQueue
appendTodoPatches patches queue =
    List.append queue (List.map TodoPatch patches)
