module TodoPatch exposing (TodoPatch, TodoPatchList, createPatch, patchDecoder, patchEncoder, patchListDecoder, patchListEncoder)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Now exposing (Millis)
import Todo exposing (Todo, TodoId, TodoList)
import TodoId


type alias TodoPatchList =
    List TodoPatch


type alias TodoPatch =
    { todoId : TodoId
    , key : String
    , value : Value
    , modifiedAt : Int
    }


patchDecoder : Decoder TodoPatch
patchDecoder =
    JD.succeed TodoPatch
        |> JDP.required "todoId" TodoId.decoder
        |> JDP.required "key" JD.string
        |> JDP.required "value" JD.value
        |> JDP.required "modifiedAt" JD.int


patchEncoder : TodoPatch -> Value
patchEncoder { todoId, key, value, modifiedAt } =
    JE.object
        [ ( "todoId", TodoId.encoder todoId )
        , ( "key", JE.string key )
        , ( "value", value )
        , ( "modifiedAt", JE.int modifiedAt )
        ]


createPatch : TodoId -> Todo.Msg -> Millis -> TodoPatch
createPatch todoId todoMsg now =
    let
        ( key, value ) =
            Todo.msgKVEncoder todoMsg
    in
    TodoPatch todoId key value now


patchListDecoder : Decoder TodoPatchList
patchListDecoder =
    JD.list patchDecoder


patchListEncoder : TodoPatchList -> Value
patchListEncoder =
    JE.list patchEncoder
