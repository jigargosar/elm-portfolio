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
                [ ( "type", JE.string "TodoPatch" )
                , ( "value", Todo.patchEncoder patch )
                ]


patchDecoder : Decoder Patch
patchDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\type_ ->
                case type_ of
                    "TodoPatch" ->
                        Todo.patchDecoder |> JD.map TodoPatch

                    _ ->
                        JD.fail ("Invalid Patch type: " ++ type_)
            )


queueDecoder : Decoder SyncQueue
queueDecoder =
    JD.list patchDecoder


queueEncoder : SyncQueue -> Value
queueEncoder =
    JE.list patchEncoder


appendTodoPatches : List Todo.Patch -> SyncQueue -> SyncQueue
appendTodoPatches patches queue =
    List.append queue (List.map TodoPatch patches)
