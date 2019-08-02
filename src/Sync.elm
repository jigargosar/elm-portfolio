module Sync exposing
    ( Msg(..)
    , Patch(..)
    , SyncQueue
    , decoder
    , encoder
    , initialQueue
    , update
    )

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Todo exposing (TodoId)


type Patch
    = TodoPatch Todo.Patch


type alias Model =
    List Patch


type SyncQueue
    = SyncQueue Model


initialQueue : SyncQueue
initialQueue =
    SyncQueue []


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


decoder : Decoder SyncQueue
decoder =
    JD.list patchDecoder
        |> JD.map SyncQueue


encoder : SyncQueue -> Value
encoder (SyncQueue q) =
    JE.list patchEncoder q


appendTodoPatches : List Todo.Patch -> SyncQueue -> SyncQueue
appendTodoPatches patches (SyncQueue q) =
    List.append q (List.map TodoPatch patches)
        |> SyncQueue


type Msg
    = AppendTodoPatches (List Todo.Patch)


update : Msg -> SyncQueue -> ( SyncQueue, Cmd Msg )
update msg model =
    case msg of
        AppendTodoPatches pl ->
            ( appendTodoPatches pl model, Cmd.none )
