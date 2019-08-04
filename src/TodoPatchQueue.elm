module TodoPatchQueue exposing
    ( TodoPatchQueue
    , init
    , queueDecoder
    , queueEncoder
    )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Now exposing (Millis)
import Todo exposing (Todo, TodoId, TodoList)
import TodoId
import TodoPatch exposing (TodoPatch)


type TodoPatchQueue
    = TodoPatchQueue (List TodoPatch)


init : TodoPatchQueue
init =
    TodoPatchQueue []


queueDecoder : Decoder TodoPatchQueue
queueDecoder =
    JD.list TodoPatch.decoder
        |> JD.map TodoPatchQueue


queueEncoder : TodoPatchQueue -> Value
queueEncoder (TodoPatchQueue list) =
    JE.list TodoPatch.encoder list
