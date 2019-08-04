module TodoPatch exposing
    ( TodoPatch
    , TodoPatchList
    , decoder
    , encoder
    , init
    , listDecoder
    , listEncoder
    )

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


decoder : Decoder TodoPatch
decoder =
    JD.succeed TodoPatch
        |> JDP.required "todoId" TodoId.decoder
        |> JDP.required "key" JD.string
        |> JDP.required "value" JD.value
        |> JDP.required "modifiedAt" JD.int


encoder : TodoPatch -> Value
encoder { todoId, key, value, modifiedAt } =
    JE.object
        [ ( "todoId", TodoId.encoder todoId )
        , ( "key", JE.string key )
        , ( "value", value )
        , ( "modifiedAt", JE.int modifiedAt )
        ]


init : TodoId -> Todo.Msg -> Millis -> TodoPatch
init todoId todoMsg now =
    let
        ( key, value ) =
            Todo.msgKVEncoder todoMsg
    in
    TodoPatch todoId key value now


listDecoder : Decoder TodoPatchList
listDecoder =
    JD.list decoder


listEncoder : TodoPatchList -> Value
listEncoder =
    JE.list encoder
