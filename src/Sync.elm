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


type Model
    = NotSent (List Patch)
    | Sent (List Patch) (List Patch)


type alias SyncQueue =
    Model


initialQueue : SyncQueue
initialQueue =
    NotSent []


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
        |> JD.map NotSent


encoder : SyncQueue -> Value
encoder model =
    let
        allPatches =
            case model of
                NotSent pl ->
                    pl

                Sent pl1 pl2 ->
                    pl1 ++ pl2
    in
    JE.list patchEncoder allPatches


type Msg
    = AppendTodoPatches (List Todo.Patch)


update : Msg -> SyncQueue -> ( SyncQueue, Cmd Msg )
update msg model =
    case model of
        NotSent notSent ->
            case msg of
                AppendTodoPatches pl ->
                    ( NotSent (notSent ++ List.map TodoPatch pl), Cmd.none )

        Sent sent notSent ->
            case msg of
                AppendTodoPatches pl ->
                    ( Sent sent (notSent ++ List.map TodoPatch pl), Cmd.none )
