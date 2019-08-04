module SyncQueue exposing
    ( SyncQueue
    , decoder
    , encoder
    , init
    )

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TodoPatch exposing (TodoPatch)


type SyncQueue
    = TodoPatchQueue (List TodoPatch)


init : SyncQueue
init =
    TodoPatchQueue []


decoder : Decoder SyncQueue
decoder =
    JD.list TodoPatch.decoder
        |> JD.map TodoPatchQueue


encoder : SyncQueue -> Value
encoder (TodoPatchQueue list) =
    JE.list TodoPatch.encoder list
