module Sync exposing
    ( Msg(..)
    , Patch(..)
    , SyncQueue
    , init
    , initialValue
    , update
    )

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Ports
import Return
import Todo exposing (TodoId)


type Patch
    = TodoPatch Todo.Patch


type Model
    = Empty
    | Sent (List Patch) (List Patch)


type alias SyncQueue =
    Model


initialValue : SyncQueue
initialValue =
    Empty


type alias Return =
    Return.Return Msg Model


init : Value -> Result JD.Error Return
init encodedValue =
    JD.decodeValue (JD.list patchDecoder) encodedValue
        |> Result.map
            (\pl ->
                case pl of
                    [] ->
                        ( Empty, Cmd.none )

                    _ ->
                        ( Sent pl [], Cmd.none )
            )


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
                        JD.field "value"
                            (Todo.patchDecoder |> JD.map TodoPatch)

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


cacheSyncQueueEffect : Model -> Cmd Msg
cacheSyncQueueEffect model =
    Ports.cacheSyncQueue (encoder model)


type Msg
    = AppendTodoPatches (List Todo.Patch)
    | OnSyncResponse (Result Http.Error String)


update : Msg -> SyncQueue -> Return
update msg model =
    updateHelp msg model
        |> effect cacheSyncQueueEffect


updateHelp : Msg -> SyncQueue -> Return
updateHelp msg model =
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



-- UPDATE HELPERS


pure =
    Return.singleton


effect =
    Return.effect_


andThen =
    Return.andThen


command =
    Return.command



-- CORE HELPERS


unpackErr : (e -> v) -> Result e v -> v
unpackErr fn result =
    case result of
        Err e ->
            fn e

        Ok v ->
            v
