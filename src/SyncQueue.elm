module SyncQueue exposing
    ( SyncQueue
    , appendTodoPatches
    , decoder
    , encoder
    , init
    )

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TodoPatch exposing (TodoPatch)


type SyncQueue
    = SyncQueue (List TodoPatch)


init : SyncQueue
init =
    SyncQueue []


decoder : Decoder SyncQueue
decoder =
    JD.list TodoPatch.decoder
        |> JD.map SyncQueue


encoder : SyncQueue -> Value
encoder (SyncQueue list) =
    JE.list TodoPatch.encoder list


appendTodoPatches : List TodoPatch -> SyncQueue -> SyncQueue
appendTodoPatches newList (SyncQueue list) =
    list
        ++ newList
        |> SyncQueue
