module Sync exposing
    ( Msg(..)
    , Patch(..)
    , SyncQueue
    , decoder
    , encoder
    , initialQueue
    , update
    )

import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Todo exposing (TodoId)


type Patch
    = TodoPatch Todo.Patch


type alias Model =
    Array Patch


type SyncQueue
    = SyncQueue Model


initialQueue : SyncQueue
initialQueue =
    SyncQueue Array.empty


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
    JD.array patchDecoder
        |> JD.map SyncQueue


encoder : SyncQueue -> Value
encoder (SyncQueue q) =
    JE.array patchEncoder q


appendTodoPatches : Array Todo.Patch -> SyncQueue -> SyncQueue
appendTodoPatches patches (SyncQueue q) =
    Array.append q (Array.map TodoPatch patches)
        |> SyncQueue


type Msg
    = AppendTodoPatches (List Todo.Patch)


update : Msg -> SyncQueue -> ( SyncQueue, Cmd Msg )
update msg model =
    case msg of
        AppendTodoPatches pl ->
            ( appendTodoPatches (Array.fromList pl) model, Cmd.none )
