module Sync exposing
    ( Msg(..)
    , Patch(..)
    , SyncQueue
    , decoder
    , encoder
    , initialQueue
    , update
    )

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Todo exposing (TodoId)


type Patch
    = TodoPatch Todo.Patch


type Model
    = Empty
    | Sent (List Patch) (List Patch)


type alias SyncQueue =
    Model


initialQueue : SyncQueue
initialQueue =
    Empty


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
        |> JD.map (Sent [])


encoder : SyncQueue -> Value
encoder model =
    let
        allPatches =
            case model of
                Empty ->
                    []

                Sent pl1 pl2 ->
                    pl1 ++ pl2
    in
    JE.list patchEncoder allPatches


type Msg
    = AppendTodoPatches (List Todo.Patch)
    | OnSyncResponse (Result Http.Error String)


update : Msg -> SyncQueue -> ( SyncQueue, Cmd Msg )
update msg model =
    case msg of
        AppendTodoPatches todoPatchList ->
            let
                patchList =
                    List.map TodoPatch todoPatchList
            in
            case model of
                Empty ->
                    ( Sent patchList []
                    , Http.post
                        { url = "/api/sync"
                        , body = Http.jsonBody (JE.list patchEncoder patchList)
                        , expect = Http.expectString OnSyncResponse
                        }
                    )

                Sent sent notSent ->
                    ( Sent sent (notSent ++ patchList), Cmd.none )

        OnSyncResponse result ->
            let
                _ =
                    Debug.log "syncResponse" result
            in
            ( model, Cmd.none )
